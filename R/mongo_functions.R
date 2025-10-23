# R/mongo_functions.R

sanitize_colnames <- function(nms) {
  nms <- gsub("\\.", "_", nms, perl = TRUE)
  nms <- ifelse(grepl("^\\$", nms), paste0("dollar_", sub("^\\$", "", nms)), nms)
  nms
}

normalize_for_mongo <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  # factors -> character
  is_fac  <- vapply(df, is.factor, logical(1))
  if (any(is_fac)) df[is_fac] <- lapply(df[is_fac], as.character)
  # list-cols -> scalar character
  is_list <- vapply(df, is.list, logical(1))
  if (any(is_list)) {
    df[is_list] <- lapply(df[is_list], function(col)
      vapply(col, function(x) if (length(x) == 0) NA_character_ else as.character(x[[1]]), character(1))
    )
  }
  # NaN/Inf -> NA
  is_num <- vapply(df, is.numeric, logical(1))
  if (any(is_num)) {
    df[is_num] <- lapply(df[is_num], function(x) { x[is.nan(x) | is.infinite(x)] <- NA_real_; x })
  }
  rownames(df) <- NULL
  names(df) <- sanitize_colnames(names(df))
  df
}





# Upload data at different stages during processing
save_stage_to_mongo <- function(msi_object, run_id, stage_name, params = list(),
                                db_name = "MSI_database",
                                mongo_url = "mongodb://localhost",
                                bucket = "fs") {
  
  fs_files  <- mongo(collection = paste0(bucket, ".files"),  db = db_name, url = mongo_url)
  fs_chunks <- mongo(collection = paste0(bucket, ".chunks"), db = db_name, url = mongo_url)
  mongo_meta <- mongo(collection = "processing_runs", db = db_name, url = mongo_url)
  
  # serialize object to raw
  data_raw <- serialize(msi_object, connection = NULL)
  
  # make simple 24-hex id
  file_id <- paste0(format(as.hexmode(sample(0:255, 12, replace = TRUE)), width = 2), collapse = "")
  
  chunk_size <- 255 * 1024
  n_chunks <- ceiling(length(data_raw) / chunk_size)
  
  # ---- INSERT USING JSON STRINGS ----
  file_doc <- jsonlite::toJSON(list(
    `_id`      = file_id,
    filename   = paste0(run_id, "_", stage_name, ".rds"),
    length     = length(data_raw),
    chunkSize  = chunk_size,
    uploadDate = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
  ), auto_unbox = TRUE)
  
  fs_files$insert(file_doc)
  
  for (i in seq_len(n_chunks)) {
    chunk_start <- (i - 1) * chunk_size + 1
    chunk_end   <- min(i * chunk_size, length(data_raw))
    chunk_data  <- data_raw[chunk_start:chunk_end]
    
    chunk_doc <- jsonlite::toJSON(list(
      files_id = file_id,
      n        = i - 1L,
      data     = chunk_data
    ), auto_unbox = TRUE)
    
    fs_chunks$insert(chunk_doc)
  }
  
  # update metadata doc
  timestamp <- format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ")
  file_name <- paste0(run_id, "_", stage_name, ".rds")
  params_json <- if (length(params) == 0) "{}" else jsonlite::toJSON(params, auto_unbox = TRUE)
  
  json_update <- paste0(
    '{"$set": {"stages.', stage_name, '": {',
    '"gridfs_id": "', file_id, '", ',
    '"parameters": ', params_json, ', ',
    '"timestamp": "', timestamp, '", ',
    '"file_name": "', file_name, '"}}}'
  )
  
  mongo_meta$update(paste0('{"run_id": "', run_id, '"}'), json_update)
  message("✓ Saved stage '", stage_name, "' for run ", run_id, " to GridFS.")
  invisible(file_id)
}






# --- Function to load a GridFS file directly into R ---
load_gridfs_rds <- function(gridfs_id,
                            db = "MSI_database",
                            url = "mongodb://localhost",
                            bucket = "fs") {
  
  fs_files  <- mongo(collection = paste0(bucket, ".files"),  db = db, url = url)
  fs_chunks <- mongo(collection = paste0(bucket, ".chunks"), db = db, url = url)
  
  # --- Check file document ---
  file_doc <- fs_files$find(sprintf('{"_id": "%s"}', gridfs_id))
  if (nrow(file_doc) == 0) stop("File not found in GridFS.")
  
  # --- Retrieve chunks ---
  chunk_docs <- fs_chunks$find(sprintf('{"files_id": "%s"}', gridfs_id))
  if (nrow(chunk_docs) == 0) stop("No chunks found for this file.")
  
  # --- Sort chunks and decode base64 ---
  chunk_docs <- chunk_docs[order(chunk_docs$n), ]
  chunk_data_list <- lapply(chunk_docs$data, base64decode)
  
  # --- Combine chunks ---
  data_raw <- do.call(c, chunk_data_list)
  
  # --- Write and read RDS ---
  tmpfile <- tempfile(fileext = ".rds")
  writeBin(data_raw, tmpfile)
  obj <- readRDS(tmpfile)
  
  return(obj)
}





# Retrieve data from any stage
# --- Load an object from MongoDB (GridFS) ---
load_stage_from_mongo <- function(run_id, stage_name,
                                  db_name = "MSI_database",
                                  mongo_url = "mongodb://localhost",
                                  bucket = "fs") {
  # Connect to metadata collection
  mongo_meta <- mongo(collection = "processing_runs", db = db_name, url = mongo_url)
  
  # Retrieve metadata document
  run_doc <- mongo_meta$find(sprintf('{"run_id": "%s"}', run_id))
  if (nrow(run_doc) == 0) stop("No run found with run_id: ", run_id)
  
  # --- Safely extract stages structure ---
  stages_obj <- run_doc$stages[[1]]
  if (is.null(names(stages_obj))) {
    stages_obj <- run_doc$stages
  }
  
  # Handle case where we're inside a single stage
  if (all(c("gridfs_id", "parameters", "timestamp", "file_name") %in% names(stages_obj))) {
    # Go up one level if we accidentally went inside one stage
    stages_obj <- run_doc$stages
  }
  
  stage_names <- names(stages_obj)
  if (!(stage_name %in% stage_names)) {
    stop("Stage '", stage_name, "' not found for run_id ", run_id)
  }
  
  # --- Extract GridFS ID ---
  gridfs_id <- stages_obj[[stage_name]]$gridfs_id
  if (is.null(gridfs_id) || is.na(gridfs_id)) {
    stop("No GridFS ID found for stage '", stage_name, "'.")
  }
  
  message("Loading stage '", stage_name, "' for run ", run_id, " (GridFS ID: ", gridfs_id, ")")
  
  # --- Load directly from GridFS ---
  obj <- load_gridfs_rds(gridfs_id = gridfs_id, db = db_name, url = mongo_url, bucket = bucket)
  
  message("✓ Successfully loaded stage '", stage_name, "' from MongoDB.")
  return(obj)
}




list_binned_stages <- function(run_id,
                               db_name = "MSI_database",
                               mongo_url = "mongodb://localhost") {
  mongo_meta <- mongo(collection = "processing_runs", db = db_name, url = mongo_url)
  run_doc <- mongo_meta$find(paste0('{"run_id": "', run_id, '"}'))
  if (nrow(run_doc) == 0) return(character(0))
  
  stages_obj <- run_doc$stages[[1]]
  if (is.null(names(stages_obj))) stages_obj <- run_doc$stages
  stage_names <- names(stages_obj)
  
  grep("^binned_dataframe", stage_names, value = TRUE)
}

