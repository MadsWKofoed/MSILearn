# R/config.R
# Runtime configuration shared by the app and tests.

.env_or_default <- function(name, default) {
  value <- Sys.getenv(name, unset = NA_character_)
  if (is.na(value) || !nzchar(value)) default else value
}

DB_NAME <- .env_or_default("MONGO_DB", "MSI_DB")
MONGO_URL <- .env_or_default("MONGO_URL", "mongodb://localhost:27018")

.env_int_or_default <- function(name, default) {
  value <- suppressWarnings(as.integer(.env_or_default(name, as.character(default))))
  if (is.na(value)) default else value
}

.env_num_or_default <- function(name, default) {
  value <- suppressWarnings(as.numeric(.env_or_default(name, as.character(default))))
  if (is.na(value)) default else value
}

.read_first_line <- function(path) {
  if (!file.exists(path)) {
    return(NA_character_)
  }
  value <- tryCatch(
    suppressWarnings(readLines(path, n = 1L, warn = FALSE)),
    error = function(e) NA_character_
  )
  if (length(value) == 0L) NA_character_ else value[[1]]
}

.parse_cpuset_count <- function(value) {
  value <- trimws(value)
  if (is.na(value) || !nzchar(value)) {
    return(NA_integer_)
  }

  ranges <- strsplit(value, ",", fixed = TRUE)[[1]]
  counts <- vapply(ranges, function(range) {
    bounds <- suppressWarnings(as.integer(strsplit(range, "-", fixed = TRUE)[[1]]))
    if (length(bounds) == 1L && !is.na(bounds)) {
      return(1L)
    }
    if (length(bounds) == 2L && all(!is.na(bounds))) {
      return(max(0L, bounds[[2]] - bounds[[1]] + 1L))
    }
    0L
  }, integer(1))

  count <- sum(counts)
  if (count > 0L) count else NA_integer_
}

.cgroup_cpu_limit <- function() {
  cpu_max <- .read_first_line("/sys/fs/cgroup/cpu.max")
  if (!is.na(cpu_max)) {
    parts <- strsplit(cpu_max, "\\s+")[[1]]
    if (length(parts) >= 2L && parts[[1]] != "max") {
      quota <- suppressWarnings(as.numeric(parts[[1]]))
      period <- suppressWarnings(as.numeric(parts[[2]]))
      if (is.finite(quota) && is.finite(period) && quota > 0 && period > 0) {
        return(max(1L, floor(quota / period)))
      }
    }
  }

  quota <- suppressWarnings(as.numeric(.read_first_line("/sys/fs/cgroup/cpu/cpu.cfs_quota_us")))
  period <- suppressWarnings(as.numeric(.read_first_line("/sys/fs/cgroup/cpu/cpu.cfs_period_us")))
  if (is.finite(quota) && is.finite(period) && quota > 0 && period > 0) {
    return(max(1L, floor(quota / period)))
  }

  NA_integer_
}

.cgroup_cpuset_limit <- function() {
  candidates <- c(
    "/sys/fs/cgroup/cpuset.cpus.effective",
    "/sys/fs/cgroup/cpuset/cpuset.cpus"
  )

  for (path in candidates) {
    count <- .parse_cpuset_count(.read_first_line(path))
    if (!is.na(count)) {
      return(count)
    }
  }

  NA_integer_
}

app_available_cores <- function(logical = FALSE) {
  detected <- suppressWarnings(parallel::detectCores(logical = logical))
  if (length(detected) != 1L || !is.finite(detected) || is.na(detected)) {
    detected <- NA_integer_
  }
  if (is.na(detected) && identical(Sys.info()[["sysname"]], "Darwin")) {
    key <- if (isTRUE(logical)) "hw.logicalcpu" else "hw.physicalcpu"
    detected <- suppressWarnings(as.integer(system2("sysctl", c("-n", key), stdout = TRUE, stderr = FALSE)))
  }
  if (length(detected) != 1L || !is.finite(detected) || is.na(detected)) {
    detected <- suppressWarnings(parallel::detectCores(logical = TRUE))
  }
  if (length(detected) != 1L || !is.finite(detected) || is.na(detected)) {
    detected <- NA_integer_
  }
  limits <- c(detected, .cgroup_cpu_limit(), .cgroup_cpuset_limit())
  limits <- limits[is.finite(limits) & !is.na(limits) & limits > 0]
  if (length(limits) == 0L) {
    return(1L)
  }
  max(1L, floor(min(limits)))
}

app_available_memory_gb <- function() {
  candidates <- c(
    "/sys/fs/cgroup/memory.max",
    "/sys/fs/cgroup/memory/memory.limit_in_bytes"
  )

  for (path in candidates) {
    value <- .read_first_line(path)
    if (is.na(value) || value == "max") {
      next
    }

    bytes <- suppressWarnings(as.numeric(value))
    if (is.finite(bytes) && bytes > 0 && bytes < 9e18) {
      return(bytes / 1024^3)
    }
  }

  meminfo <- if (file.exists("/proc/meminfo")) {
    tryCatch(
      suppressWarnings(readLines("/proc/meminfo", warn = FALSE)),
      error = function(e) character()
    )
  } else {
    character()
  }
  mem_total <- grep("^MemTotal:", meminfo, value = TRUE)
  if (length(mem_total) == 1L) {
    kb <- suppressWarnings(as.numeric(gsub("[^0-9]", "", mem_total)))
    if (is.finite(kb) && kb > 0) {
      return(kb / 1024^2)
    }
  }

  if (identical(Sys.info()[["sysname"]], "Darwin")) {
    bytes <- suppressWarnings(as.numeric(system2("sysctl", c("-n", "hw.memsize"), stdout = TRUE, stderr = FALSE)))
    if (length(bytes) == 1L && is.finite(bytes) && bytes > 0) {
      return(bytes / 1024^3)
    }
  }

  NA_real_
}

app_worker_count <- function(max_workers = .env_int_or_default("APP_WORKER_MAX", 8L)) {
  available_cores <- app_available_cores(logical = FALSE)
  override <- .env_or_default("APP_WORKERS", "")
  if (nzchar(override)) {
    requested <- suppressWarnings(as.integer(override))
    if (!is.na(requested) && requested > 0L) {
      return(max(1L, requested))
    }
  }

  reserve_cores <- max(0L, .env_int_or_default("APP_WORKER_RESERVE_CORES", 1L))
  cpu_fraction <- min(1, max(0.1, .env_num_or_default("APP_WORKER_CPU_FRACTION", 0.75)))
  cpu_workers <- max(
    1L,
    min(
      available_cores - reserve_cores,
      floor(available_cores * cpu_fraction)
    )
  )

  memory_gb <- app_available_memory_gb()
  memory_per_worker_gb <- max(0.25, .env_num_or_default("APP_MEMORY_PER_WORKER_GB", 4))
  reserve_memory_gb <- max(0, .env_num_or_default("APP_RESERVE_MEMORY_GB", 4))
  memory_workers <- if (is.finite(memory_gb)) {
    max(1L, floor((memory_gb - reserve_memory_gb) / memory_per_worker_gb))
  } else {
    max_workers
  }

  max(1L, min(cpu_workers, memory_workers, max_workers))
}
