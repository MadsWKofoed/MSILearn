# R/mongo_functions.R

sanitize_colnames <- function(nms) {
  nms <- gsub("\\.", "_", nms, perl = TRUE)
  nms <- ifelse(grepl("^\\$", nms), paste0("dollar_", sub("^\\$", "", nms)), nms)
  nms
}

normalize_for_mongo <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  for (col in names(df)) {
    if (is.factor(df[[col]])) {
      df[[col]] <- as.character(df[[col]])
    }
  }
  df
}


# --- Save artifact + create metadata row ---
save_stage_to_mongo <- function(msi_object, run_id, stage_type, 
                                sample_name,
                                params = list(),
                                db_name = "MSI_database",
                                mongo_url = "mongodb://localhost") {
  
  # Save RDS to GridFS
  temp_path <- tempfile(pattern = paste0(stage_type, "_"), fileext = ".rds")
  saveRDS(msi_object, temp_path)
  
  grid <- gridfs(db = db_name, prefix = "fs", url = mongo_url)
  grid_id <- grid$upload(temp_path, name = paste0(run_id, "_", stage_type, ".rds"))
  
  # Create metadata row
  mongo_meta <- mongo(collection = "processing_artifacts_metadata", 
                      db = db_name, url = mongo_url)
  
  metadata_row <- list(
    gridfs_id = as.character(grid_id$id),
    run_id = run_id,
    sample_name = sample_name,
    stage_type = stage_type,
    created_at = Sys.time()
  )
  
  # Add all parameters as flat fields
  metadata_row <- c(metadata_row, params)
  
  mongo_meta$insert(metadata_row)
  
  message("Saved '", stage_type, "' (GridFS ID: ", grid_id$id, ")")
  invisible(grid_id$id)
}


# --- Query artifacts (returns dataframe) ---
query_artifacts <- function(sample_name = NULL, 
                            stage_type = NULL,
                            snr = NULL, 
                            tolerance = NULL,
                            reference_name = NULL,
                            run_id = NULL,
                            db_name = "MSI_database",
                            mongo_url = "mongodb://localhost") {
  
  mongo_meta <- mongo(collection = "processing_artifacts_metadata", 
                      db = db_name, url = mongo_url)
  
  # Build query
  query_parts <- list()
  if (!is.null(sample_name)) query_parts$sample_name <- sample_name
  if (!is.null(stage_type)) query_parts$stage_type <- stage_type
  if (!is.null(snr)) query_parts$snr <- snr
  if (!is.null(tolerance)) query_parts$tolerance <- tolerance
  if (!is.null(reference_name)) query_parts$reference_name <- reference_name
  if (!is.null(run_id)) query_parts$run_id <- run_id
  
  query_json <- if (length(query_parts) == 0) {
    '{}'
  } else {
    jsonlite::toJSON(query_parts, auto_unbox = TRUE)
  }
  
  results <- mongo_meta$find(query_json)
  results
}


# --- Load artifact by gridfs_id ---
load_artifact_by_id <- function(gridfs_id,
                                db_name = "MSI_database",
                                mongo_url = "mongodb://localhost") {
  
  grid <- gridfs(db = db_name, prefix = "fs", url = mongo_url)
  
  # Find file by _id
  query <- sprintf('{"_id": {"$oid": "%s"}}', gridfs_id)
  file_info <- grid$find(query)
  
  if (nrow(file_info) == 0) {
    stop("GridFS file not found: ", gridfs_id)
  }
  
  filename <- file_info$name[1]
  
  # Download to temp directory
  temp_dir <- tempdir()
  grid$download(filename, temp_dir)
  
  # Read the downloaded file
  downloaded_path <- file.path(temp_dir, filename)
  obj <- readRDS(downloaded_path)
  
  message("Loaded artifact (GridFS ID: ", gridfs_id, ")")
  obj
}


# --- Load artifact by query (gets first match) ---
load_artifact <- function(sample_name = NULL, 
                          stage_type = NULL,
                          snr = NULL, 
                          tolerance = NULL,
                          reference_name = NULL,
                          run_id = NULL,
                          db_name = "MSI_database",
                          mongo_url = "mongodb://localhost") {
  
  artifacts <- query_artifacts(
    sample_name = sample_name,
    stage_type = stage_type,
    snr = snr,
    tolerance = tolerance,
    reference_name = reference_name,
    run_id = run_id,
    db_name = db_name,
    mongo_url = mongo_url
  )
  
  if (nrow(artifacts) == 0) {
    stop("No artifacts found matching query")
  }
  
  if (nrow(artifacts) > 1) {
    message("Multiple artifacts found (", nrow(artifacts), "), loading first match")
  }
  
  gridfs_id <- artifacts$gridfs_id[1]
  load_artifact_by_id(gridfs_id, db_name, mongo_url)
}



# --- Check if artifact exists ---
artifact_exists <- function(sample_name, stage_type, params = list(),
                           db_name = "MSI_database",
                           mongo_url = "mongodb://localhost") {
  
  mongo_meta <- mongo(collection = "processing_artifacts_metadata", 
                      db = db_name, url = mongo_url)
  
  # Build query with sample_name, stage_type, and all params
  query_parts <- list(
    sample_name = sample_name,
    stage_type = stage_type
  )
  query_parts <- c(query_parts, params)
  
  query_json <- jsonlite::toJSON(query_parts, auto_unbox = TRUE)
  results <- mongo_meta$find(query_json)
  
  nrow(results) > 0
}


# --- Find what stages exist for a sample ---
get_existing_stages <- function(sample_name, 
                               db_name = "MSI_database",
                               mongo_url = "mongodb://localhost") {
  
  mongo_meta <- mongo(collection = "processing_artifacts_metadata", 
                      db = db_name, url = mongo_url)
  
  query <- sprintf('{"sample_name": "%s"}', sample_name)
  results <- mongo_meta$find(query)
  
  if (nrow(results) == 0) return(NULL)
  
  # Return list of stages with their parameters
  stages <- lapply(seq_len(nrow(results)), function(i) {
    row <- results[i, ]
    list(
      stage_type = row$stage_type,
      run_id = row$run_id,
      gridfs_id = row$gridfs_id,
      snr = row$snr,
      tolerance = row$tolerance,
      reference_name = row$reference_name,
      reference_source = row$reference_source
    )
  })
  
  stages
}


# --- Find compatible run_id (same sample + raw stage exists) ---
find_compatible_run <- function(sample_name,
                               db_name = "MSI_database",
                               mongo_url = "mongodb://localhost") {
  
  mongo_meta <- mongo(collection = "processing_artifacts_metadata", 
                      db = db_name, url = mongo_url)
  
  # Find raw stage for this sample
  query <- sprintf('{"sample_name": "%s", "stage_type": "raw"}', sample_name)
  results <- mongo_meta$find(query)
  
  if (nrow(results) == 0) return(NULL)
  
  # Return first run_id (assuming one raw per sample)
  results$run_id[1]
}

