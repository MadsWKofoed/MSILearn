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
                                mongo_url = "mongodb://localhost") {
  
  # Create temp file and save object
  temp_path <- tempfile(pattern = paste0(stage_name, "_"), fileext = ".rds")
  saveRDS(msi_object, temp_path)
  
  # Connect to MongoDB + GridFS
  mongo_meta <- mongo(collection = "processing_runs", db = db_name, url = mongo_url)
  grid <- gridfs(db = db_name, prefix = "fs", url = mongo_url)
  
  # Upload file to GridFS
  grid_id <- grid$upload(temp_path, name = paste0(run_id, "_", stage_name, ".rds"))
  
  # Build stage metadata
  stage_doc <- list(
    gridfs_id = grid_id,
    parameters = params,
    timestamp = Sys.time(),
    file_name = paste0(run_id, "_", stage_name, ".rds")
  )
  
  # Update the metadata collection
  mongo_meta$update(
    paste0('{"run_id": "', run_id, '"}'),
    paste0('{$set: {"stages.', stage_name, '": ',
           jsonlite::toJSON(stage_doc, auto_unbox = TRUE), '}}')
  )
  
  message("Saved stage '", stage_name, "' to MongoDB with GridFS ID: ", grid_id)
  invisible(grid_id)
}


# Retrieve data from any stage
load_stage_from_mongo <- function(run_id, stage_name,
                                  db_name = "MSI_database",
                                  mongo_url = "mongodb://localhost:27017") {
  # Connect to MongoDB + GridFS
  mongo_meta <- mongo(collection = "processing_runs", db = db_name, url = mongo_url)
  grid <- gridfs(db = db_name, prefix = "fs", url = mongo_url)
  
  # Retrieve metadata for this run
  run_doc <- mongo_meta$find(paste0('{"run_id": "', run_id, '"}'))
  
  if (nrow(run_doc) == 0) {
    stop("No run found with run_id: ", run_id)
  }
  
  # Check stage existence
  stages <- run_doc$stages[[1]]
  if (is.null(stages[[stage_name]])) {
    stop("Stage '", stage_name, "' not found for run_id ", run_id)
  }
  
  # Extract GridFS id and download
  grid_id <- stages[[stage_name]]$gridfs_id
  temp_path <- tempfile(pattern = paste0(stage_name, "_"), fileext = ".rds")
  grid$download(grid_id, temp_path)
  
  # Load R object
  obj <- readRDS(temp_path)
  message("Loaded stage '", stage_name, "' for run ", run_id)
  return(obj)
}
