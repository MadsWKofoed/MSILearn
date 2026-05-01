ALIGNMENT_REFERENCE_COLLECTION <- "alignment_references"
ALIGNMENT_REFERENCE_EXTENSIONS <- c("csv")

alignment_reference_resource_dir <- function() {
  pkg_dir <- system.file("extdata", "alignment_references", package = "Master")
  candidate_dirs <- c(
    pkg_dir,
    file.path(getwd(), "inst", "extdata", "alignment_references"),
    file.path(getwd(), "extdata", "alignment_references")
  )
  candidate_dirs <- unique(candidate_dirs[nzchar(candidate_dirs)])
  for (dir_path in candidate_dirs) {
    if (dir.exists(dir_path)) {
      return(normalizePath(dir_path, winslash = "/", mustWork = TRUE))
    }
  }
  stop("Bundled alignment reference directory not found.")
}

alignment_reference_manifest_path <- function() {
  file.path(alignment_reference_resource_dir(), "references_manifest.csv")
}

read_default_alignment_reference_manifest <- function() {
  manifest_path <- alignment_reference_manifest_path()
  if (!file.exists(manifest_path)) {
    stop("Alignment reference manifest not found: ", manifest_path)
  }

  manifest <- utils::read.csv(
    manifest_path,
    stringsAsFactors = FALSE,
    check.names = FALSE
  )

  required_cols <- c(
    "reference_name",
    "display_name",
    "filename",
    "file_type",
    "date_added",
    "built_in"
  )
  missing_cols <- setdiff(required_cols, names(manifest))
  if (length(missing_cols) > 0) {
    stop(
      "Alignment reference manifest is missing required columns: ",
      paste(missing_cols, collapse = ", ")
    )
  }

  if (!("description" %in% names(manifest))) {
    manifest$description <- ""
  }

  manifest$built_in <- as.logical(manifest$built_in)
  manifest
}

alignment_reference_query_json <- function(query) {
  jsonlite::toJSON(query, auto_unbox = TRUE, null = "null")
}

default_alignment_reference_display_name <- function(filename) {
  tools::file_path_sans_ext(basename(filename))
}

read_alignment_reference_text <- function(path) {
  paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
}

extract_alignment_reference_mz_values <- function(df) {
  if (!is.data.frame(df) || ncol(df) == 0) {
    stop("Reference file must contain at least one column of m/z values.")
  }

  nms_lower <- tolower(names(df))
  preferred_idx <- match(TRUE, nms_lower %in% c("mz", "m.z", "x"), nomatch = 0L)

  candidate_cols <- integer(0)
  if (preferred_idx > 0) {
    candidate_cols <- preferred_idx
  } else if (ncol(df) == 1L) {
    candidate_cols <- 1L
  } else {
    candidate_cols <- seq_len(ncol(df))
  }

  for (idx in candidate_cols) {
    vals <- suppressWarnings(as.numeric(df[[idx]]))
    vals <- vals[is.finite(vals)]
    if (length(vals) > 0) {
      return(vals)
    }
  }

  stop(
    "Reference file must contain numeric m/z values in an 'mz' column, ",
    "an 'x' column, or a single numeric column."
  )
}

parse_alignment_reference_file <- function(path) {
  if (is.null(path) || !nzchar(path) || !file.exists(path)) {
    stop("Reference file is missing or cannot be found.")
  }

  info <- file.info(path)
  if (is.na(info$size) || info$size <= 0) {
    stop("Reference file is empty.")
  }

  ext <- tolower(tools::file_ext(path))
  if (!(ext %in% ALIGNMENT_REFERENCE_EXTENSIONS)) {
    stop(
      "Unsupported reference file type: .", ext,
      ". Allowed types: ", paste(ALIGNMENT_REFERENCE_EXTENSIONS, collapse = ", ")
    )
  }

  df <- utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  mz_values <- extract_alignment_reference_mz_values(df)
  if (length(mz_values) == 0) {
    stop("Reference file contains no valid m/z values.")
  }

  list(
    mz_values = mz_values,
    file_size = unname(info$size),
    file_type = ext,
    file_contents = read_alignment_reference_text(path)
  )
}

validate_alignment_reference_upload <- function(upload_info,
                                                allowed_extensions = ALIGNMENT_REFERENCE_EXTENSIONS) {
  if (is.null(upload_info) || is.null(upload_info$datapath) || !nzchar(upload_info$datapath)) {
    return(list(valid = FALSE, message = "Upload a reference file first."))
  }

  if (!file.exists(upload_info$datapath)) {
    return(list(valid = FALSE, message = "Uploaded reference file could not be found."))
  }

  original_name <- basename(upload_info$name %||% "")
  ext <- tolower(tools::file_ext(original_name))
  if (!(ext %in% allowed_extensions)) {
    return(list(
      valid = FALSE,
      message = paste0(
        "Unsupported reference file type: .", ext,
        ". Allowed types: ", paste(allowed_extensions, collapse = ", ")
      )
    ))
  }

  parsed <- tryCatch(
    parse_alignment_reference_file(upload_info$datapath),
    error = function(e) e
  )
  if (inherits(parsed, "error")) {
    return(list(valid = FALSE, message = conditionMessage(parsed)))
  }

  list(
    valid = TRUE,
    message = "OK",
    parsed = parsed
  )
}

prepare_uploaded_alignment_reference <- function(upload_info,
                                                 display_name = NULL,
                                                 description = "",
                                                 uploaded_at = Sys.time()) {
  validation <- validate_alignment_reference_upload(upload_info)
  if (!isTRUE(validation$valid)) {
    stop(validation$message)
  }

  parsed <- validation$parsed
  display_name <- trimws(as.character(display_name %||% ""))
  if (!nzchar(display_name)) {
    display_name <- default_alignment_reference_display_name(upload_info$name %||% "reference.csv")
  }

  reference_digest <- digest::digest(
    list(
      display_name = display_name,
      original_filename = basename(upload_info$name %||% "reference.csv"),
      mz_values = parsed$mz_values
    ),
    algo = "sha256",
    serialize = TRUE
  )

  reference_name <- paste0("uploaded_", substr(reference_digest, 1, 24))
  list(
    `_id` = reference_name,
    reference_name = reference_name,
    display_name = display_name,
    filename = basename(upload_info$name %||% paste0(reference_name, ".csv")),
    original_filename = basename(upload_info$name %||% paste0(reference_name, ".csv")),
    file_type = parsed$file_type,
    file_size = parsed$file_size,
    date_added = format(uploaded_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    uploaded_at = format(uploaded_at, "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    built_in = FALSE,
    source = "uploaded",
    description = description %||% "",
    mz_values = as.numeric(parsed$mz_values),
    file_contents = parsed$file_contents
  )
}

build_bundled_alignment_reference_document <- function(manifest_row) {
  stopifnot(nrow(manifest_row) == 1L)

  resource_dir <- alignment_reference_resource_dir()
  source_path <- file.path(resource_dir, manifest_row$filename[1])
  parsed <- parse_alignment_reference_file(source_path)

  list(
    `_id` = manifest_row$reference_name[1],
    reference_name = manifest_row$reference_name[1],
    display_name = manifest_row$display_name[1],
    filename = manifest_row$filename[1],
    original_filename = manifest_row$filename[1],
    file_type = manifest_row$file_type[1],
    file_size = parsed$file_size,
    date_added = manifest_row$date_added[1],
    uploaded_at = NA_character_,
    built_in = TRUE,
    source = "built_in",
    description = manifest_row$description[1] %||% "",
    mz_values = as.numeric(parsed$mz_values),
    file_contents = parsed$file_contents
  )
}

get_alignment_reference_collection <- function(db = DB_NAME,
                                               url = MONGO_URL,
                                               collection = ALIGNMENT_REFERENCE_COLLECTION) {
  .con(collection, db, url)
}

alignment_reference_fields_json <- function(include_mz_values = FALSE) {
  fields <- list(
    `_id` = 1,
    reference_name = 1,
    display_name = 1,
    filename = 1,
    original_filename = 1,
    file_type = 1,
    file_size = 1,
    date_added = 1,
    uploaded_at = 1,
    built_in = 1,
    source = 1,
    description = 1
  )
  if (isTRUE(include_mz_values)) {
    fields$mz_values <- 1
    fields$file_contents <- 1
  }
  jsonlite::toJSON(fields, auto_unbox = TRUE)
}

normalize_alignment_reference_table <- function(df) {
  expected_cols <- c(
    "_id",
    "reference_name",
    "display_name",
    "filename",
    "original_filename",
    "file_type",
    "file_size",
    "date_added",
    "uploaded_at",
    "built_in",
    "source",
    "description"
  )
  if (nrow(df) == 0) {
    empty <- as.data.frame(
      setNames(rep(list(character(0)), length(expected_cols)), expected_cols),
      stringsAsFactors = FALSE
    )
    empty$file_size <- numeric(0)
    empty$built_in <- logical(0)
    return(empty)
  }

  for (nm in setdiff(expected_cols, names(df))) {
    df[[nm]] <- NA
  }

  if ("built_in" %in% names(df)) {
    df$built_in <- as.logical(df$built_in)
  }

  display_order <- ifelse(
    is.na(df$display_name) | !nzchar(df$display_name),
    df$reference_name,
    df$display_name
  )
  df[order(!df$built_in, tolower(display_order)), expected_cols, drop = FALSE]
}

list_alignment_references <- function(db = DB_NAME,
                                      url = MONGO_URL,
                                      include_mz_values = FALSE,
                                      collection = NULL) {
  collection <- collection %||% get_alignment_reference_collection(db, url)
  refs <- tryCatch(
    collection$find("{}", fields = alignment_reference_fields_json(include_mz_values)),
    error = function(e) {
      stop("Alignment reference database unavailable: ", conditionMessage(e))
    }
  )

  if (nrow(refs) == 0) {
    return(normalize_alignment_reference_table(data.frame(stringsAsFactors = FALSE)))
  }

  normalize_alignment_reference_table(refs)
}

load_alignment_reference <- function(reference_name,
                                     db = DB_NAME,
                                     url = MONGO_URL,
                                     collection = NULL) {
  reference_name <- trimws(as.character(reference_name %||% ""))
  if (!nzchar(reference_name)) {
    stop("Reference name is required.")
  }

  collection <- collection %||% get_alignment_reference_collection(db, url)
  query <- alignment_reference_query_json(list(reference_name = reference_name))
  doc <- tryCatch(
    collection$find(query, fields = alignment_reference_fields_json(include_mz_values = TRUE)),
    error = function(e) {
      stop("Alignment reference database unavailable: ", conditionMessage(e))
    }
  )

  if (nrow(doc) == 0) {
    stop("Reference list not found: ", reference_name)
  }

  mz_values <- as.numeric(unlist(doc$mz_values[[1]]))
  mz_values <- mz_values[is.finite(mz_values)]
  if (length(mz_values) == 0) {
    stop("Reference list contains no valid m/z values: ", reference_name)
  }

  list(
    reference_name = as.character(doc$reference_name[1]),
    display_name = as.character(doc$display_name[1] %||% doc$reference_name[1]),
    filename = as.character(doc$filename[1] %||% NA_character_),
    original_filename = as.character(doc$original_filename[1] %||% NA_character_),
    file_type = as.character(doc$file_type[1] %||% NA_character_),
    file_size = suppressWarnings(as.numeric(doc$file_size[1] %||% NA_real_)),
    date_added = as.character(doc$date_added[1] %||% NA_character_),
    uploaded_at = as.character(doc$uploaded_at[1] %||% NA_character_),
    built_in = isTRUE(doc$built_in[1]),
    source = as.character(doc$source[1] %||% NA_character_),
    description = as.character(doc$description[1] %||% ""),
    mz_values = mz_values,
    file_contents = as.character(doc$file_contents[1] %||% "")
  )
}

insert_alignment_reference_document <- function(doc, collection) {
  collection$insert(
    jsonlite::toJSON(doc, auto_unbox = TRUE, null = "null", na = "null")
  )
  invisible(doc)
}

seed_default_alignment_references <- function(db = DB_NAME,
                                              url = MONGO_URL,
                                              manifest = NULL,
                                              collection = NULL) {
  manifest <- manifest %||% read_default_alignment_reference_manifest()
  collection <- collection %||% get_alignment_reference_collection(db, url)

  results <- lapply(seq_len(nrow(manifest)), function(idx) {
    row <- manifest[idx, , drop = FALSE]
    ref_name <- row$reference_name[1]
    exists <- tryCatch(
      collection$count(alignment_reference_query_json(list(reference_name = ref_name))) > 0,
      error = function(e) {
        stop("Could not seed alignment references: ", conditionMessage(e))
      }
    )

    if (isTRUE(exists)) {
      return(data.frame(reference_name = ref_name, status = "exists", stringsAsFactors = FALSE))
    }

    doc <- build_bundled_alignment_reference_document(row)
    insert_alignment_reference_document(doc, collection)
    data.frame(reference_name = ref_name, status = "inserted", stringsAsFactors = FALSE)
  })

  do.call(rbind, results)
}

save_uploaded_alignment_reference <- function(upload_info,
                                              display_name = NULL,
                                              description = "",
                                              db = DB_NAME,
                                              url = MONGO_URL,
                                              collection = NULL) {
  collection <- collection %||% get_alignment_reference_collection(db, url)
  doc <- prepare_uploaded_alignment_reference(upload_info, display_name, description)

  exists <- tryCatch(
    collection$count(alignment_reference_query_json(list(reference_name = doc$reference_name))) > 0,
    error = function(e) {
      stop("Could not save uploaded alignment reference: ", conditionMessage(e))
    }
  )

  if (!isTRUE(exists)) {
    tryCatch(
      insert_alignment_reference_document(doc, collection),
      error = function(e) {
        stop("Could not save uploaded alignment reference: ", conditionMessage(e))
      }
    )
  }

  load_alignment_reference(doc$reference_name, collection = collection)
}

alignment_reference_choices <- function(refs) {
  if (nrow(refs) == 0) {
    return(c("No references found" = ""))
  }

  labels <- ifelse(
    refs$built_in,
    paste0(refs$display_name, " [built-in]"),
    paste0(refs$display_name, " [uploaded]")
  )
  stats::setNames(refs$reference_name, labels)
}
