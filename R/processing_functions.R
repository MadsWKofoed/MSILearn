# R/processing_functions.R

empty_uploaded_raw_pair_validation <- function(message = "Upload exactly one .imzML file and one .ibd file.") {
  list(
    valid = FALSE,
    message = message,
    imzml_idx = integer(),
    ibd_idx = integer(),
    sample_name_default = NA_character_,
    imzml_name = NA_character_,
    ibd_name = NA_character_
  )
}

normalize_hex_string <- function(x) {
  if (is.null(x) || length(x) == 0) {
    return(NA_character_)
  }
  value <- tolower(gsub("[^0-9a-fA-F]", "", as.character(x[[1]])))
  if (!nzchar(value)) NA_character_ else value
}

extract_imzml_pair_metadata <- function(imzml_path) {
  xml_lines <- readLines(imzml_path, warn = FALSE, encoding = "latin1")
  xml_text <- paste(xml_lines, collapse = "\n")

  extract_cv_value <- function(accession) {
    match <- regexec(
      paste0('accession="', accession, '".*?value="([^"]+)"'),
      xml_text,
      perl = TRUE
    )
    parts <- regmatches(xml_text, match)[[1]]
    if (length(parts) >= 2L) parts[2] else NA_character_
  }

  list(
    uuid = normalize_hex_string(extract_cv_value("IMS:1000080")),
    ibd_sha1 = normalize_hex_string(extract_cv_value("IMS:1000091"))
  )
}

read_ibd_uuid_hex <- function(ibd_path) {
  con <- file(ibd_path, open = "rb")
  on.exit(close(con), add = TRUE)
  uuid_raw <- readBin(con, what = "raw", n = 16L)
  if (length(uuid_raw) != 16L) {
    return(NA_character_)
  }
  paste(sprintf("%02x", as.integer(uuid_raw)), collapse = "")
}

validate_uploaded_raw_pair <- function(upload_info,
                                       probe_pair = TRUE,
                                       probe_resolution = 10,
                                       probe_bpparam = NULL) {
  invalid <- empty_uploaded_raw_pair_validation

  if (is.null(upload_info) || NROW(upload_info) == 0) {
    return(invalid())
  }

  if (!all(c("name", "datapath") %in% names(upload_info))) {
    return(invalid("The uploaded files are missing required metadata. Please upload them again."))
  }

  file_names <- as.character(upload_info$name)
  file_paths <- as.character(upload_info$datapath)
  ext <- tolower(tools::file_ext(file_names))
  imzml_idx <- which(ext == "imzml")
  ibd_idx <- which(ext == "ibd")

  if (length(file_names) != 2L || length(imzml_idx) != 1L || length(ibd_idx) != 1L) {
    return(invalid("Upload exactly one .imzML file and one .ibd file."))
  }

  if (any(!nzchar(file_paths[c(imzml_idx, ibd_idx)])) ||
      any(!file.exists(file_paths[c(imzml_idx, ibd_idx)]))) {
    return(invalid("The uploaded files could not be read. Please try uploading them again."))
  }

  imzml_name <- basename(file_names[imzml_idx][1])
  ibd_name <- basename(file_names[ibd_idx][1])
  imzml_stem <- tools::file_path_sans_ext(imzml_name)
  ibd_stem <- tools::file_path_sans_ext(ibd_name)

  if (!identical(tolower(imzml_stem), tolower(ibd_stem))) {
    return(invalid("The .imzML and .ibd files must have the same base filename."))
  }

  if (isTRUE(probe_pair)) {
    probe_dir <- tempfile("raw_pair_validation_")
    dir.create(probe_dir, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(probe_dir, recursive = TRUE, force = TRUE), add = TRUE)

    staged_imzml <- file.path(probe_dir, imzml_name)
    staged_ibd <- file.path(probe_dir, ibd_name)
    copied <- file.copy(
      from = c(file_paths[imzml_idx][1], file_paths[ibd_idx][1]),
      to = c(staged_imzml, staged_ibd),
      overwrite = TRUE
    )

    if (!all(copied)) {
      return(invalid("The uploaded files could not be staged for validation. Please try uploading them again."))
    }

    if (is.null(probe_bpparam)) {
      probe_bpparam <- BiocParallel::SerialParam()
    }

    metadata <- tryCatch(
      extract_imzml_pair_metadata(staged_imzml),
      error = function(e) {
        invalid(paste("Could not read the .imzML metadata.", conditionMessage(e)))
      }
    )
    if (is.list(metadata) && identical(metadata$valid, FALSE)) {
      return(metadata)
    }

    ibd_uuid <- tryCatch(
      read_ibd_uuid_hex(staged_ibd),
      error = function(e) NA_character_
    )
    if (is.na(metadata$uuid) || !nzchar(metadata$uuid)) {
      return(invalid("The .imzML metadata does not contain a readable UUID for the paired .ibd file."))
    }
    if (is.na(ibd_uuid) || !nzchar(ibd_uuid)) {
      return(invalid("The uploaded .ibd file header could not be read."))
    }
    if (!identical(metadata$uuid, ibd_uuid)) {
      return(invalid("The uploaded .ibd file does not match the UUID recorded in the .imzML file."))
    }

    if (!is.na(metadata$ibd_sha1) &&
        nzchar(metadata$ibd_sha1) &&
        requireNamespace("digest", quietly = TRUE)) {
      ibd_sha1 <- normalize_hex_string(
        digest::digest(file = staged_ibd, algo = "sha1", serialize = FALSE)
      )
      if (!identical(metadata$ibd_sha1, ibd_sha1)) {
        return(invalid("The uploaded .ibd file checksum does not match the value recorded in the .imzML file."))
      }
    }

    probe_result <- tryCatch(
      {
        suppressWarnings(
          Cardinal::readMSIData(
            staged_imzml,
            memory = FALSE,
            check = FALSE,
            resolution = probe_resolution,
            units = "ppm",
            BPPARAM = probe_bpparam
          )
        )
        NULL
      },
      error = function(e) trimws(gsub("[\r\n]+", " ", conditionMessage(e)))
    )

    if (!is.null(probe_result)) {
      return(invalid(paste(
        "The uploaded .imzML and .ibd files could not be read together.",
        probe_result
      )))
    }
  }

  list(
    valid = TRUE,
    message = NULL,
    imzml_idx = imzml_idx,
    ibd_idx = ibd_idx,
    sample_name_default = imzml_stem,
    imzml_name = imzml_name,
    ibd_name = ibd_name
  )
}

load_raw_object_from_mongo <- function(sample_name, workdir,
                                       db_name   = DB_NAME,
                                       mongo_url = MONGO_URL,
                                       resolution = 10) {
  paths <- fetch_raw_pair_from_mongo(
    sample_name = sample_name,
    dest_dir    = workdir,
    db_name     = db_name,
    mongo_url   = mongo_url
  )
  Cardinal::readMSIData(
    paths$imzml,
    memory     = FALSE,
    check      = FALSE,
    resolution = resolution,
    units      = "ppm",
    BPPARAM    = BiocParallel::bpparam()
  )
}
