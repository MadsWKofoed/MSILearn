args <- commandArgs(trailingOnly = TRUE)
if (length(args) != 1L) {
  stop("Usage: Rscript install_r_packages.R /path/to/r-packages.csv", call. = FALSE)
}

manifest <- read.csv(args[[1]], stringsAsFactors = FALSE)
required_cols <- c("Package", "Version", "Source")
missing_cols <- setdiff(required_cols, names(manifest))
if (length(missing_cols) > 0L) {
  stop("Package manifest is missing columns: ", paste(missing_cols, collapse = ", "), call. = FALSE)
}

cran_repo <- Sys.getenv("CRAN_REPO", unset = "https://packagemanager.posit.co/cran/__linux__/jammy/2026-03-10")
bioc_version <- Sys.getenv("BIOC_VERSION", unset = "3.20")
ncpus <- suppressWarnings(as.integer(Sys.getenv("R_INSTALL_NCPUS", unset = "1")))
if (is.na(ncpus) || ncpus < 1L) {
  ncpus <- 1L
}

options(
  repos = c(CRAN = cran_repo),
  Ncpus = ncpus
)

cran_packages <- manifest$Package[manifest$Source == "CRAN"]
bioc_packages <- manifest$Package[manifest$Source == "Bioconductor"]

install.packages(cran_packages, Ncpus = ncpus)

install_exact_cran_version <- function(package, version) {
  candidates <- c(
    sprintf("%s/src/contrib/%s_%s.tar.gz", cran_repo, package, version),
    sprintf("https://cran.r-project.org/src/contrib/Archive/%s/%s_%s.tar.gz", package, package, version)
  )

  for (url in candidates) {
    message("Installing exact CRAN version: ", package, " ", version)
    suppressWarnings(try(
      install.packages(url, repos = NULL, type = "source", Ncpus = ncpus),
      silent = TRUE
    ))

    installed_version <- tryCatch(
      packageDescription(package, fields = "Version"),
      error = function(e) NA_character_
    )
    if (installed_version == version) {
      return(invisible(TRUE))
    }
  }

  stop("Unable to install exact CRAN package version: ", package, " ", version, call. = FALSE)
}

installed <- as.data.frame(installed.packages()[, "Version", drop = FALSE])
installed$Package <- rownames(installed)
cran_manifest <- manifest[manifest$Source == "CRAN", ]

for (i in seq_len(nrow(cran_manifest))) {
  package <- cran_manifest$Package[[i]]
  expected_version <- cran_manifest$Version[[i]]
  installed_version <- installed$Version[installed$Package == package]

  if (length(installed_version) != 1L || installed_version != expected_version) {
    install_exact_cran_version(package, expected_version)
  }
}

BiocManager::install(version = bioc_version, ask = FALSE, update = FALSE)
BiocManager::install(bioc_packages, ask = FALSE, update = FALSE, Ncpus = ncpus)

installed <- as.data.frame(installed.packages()[, "Version", drop = FALSE])
installed$Package <- rownames(installed)
merged <- merge(manifest, installed, by = "Package", suffixes = c("_expected", "_installed"), all.x = TRUE)
mismatches <- merged[is.na(merged$Version_installed) | merged$Version_expected != merged$Version_installed, ]

if (nrow(mismatches) > 0L) {
  print(mismatches[, c("Package", "Version_expected", "Version_installed", "Source")], row.names = FALSE)
  stop("Installed R package versions do not match the server package manifest.", call. = FALSE)
}

message("R package versions match the server package manifest.")
