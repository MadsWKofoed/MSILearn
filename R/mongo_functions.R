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

# ADD THIS - from fixing.R
sanitize_name <- function(x) gsub("[^A-Za-z0-9._-]+", "_", x)

# ===== RAW FILE STORAGE =====
save_raw_pair_to_mongo <- function(sample_name, imzml_path, ibd_path,
                                   db_name = "MSI_database",
                                   mongo_url = "mongodb://localhost:27018",
                                   bucket = "fs") {
  stopifnot(file.exists(imzml_path), file.exists(ibd_path))
  
  grid <- gridfs(db = db_name, prefix = bucket, url = mongo_url)
  meta <- mongo(collection = "processing_artifacts_metadata", 
                db = db_name, url = mongo_url)
  
  base <- tools::file_path_sans_ext(basename(sample_name))
  ts   <- format(Sys.time(), "%Y%m%d_%H%M%S")
  imz_name <- sanitize_name(sprintf("%s__%s.imzML", base, ts))
  ibd_name <- sanitize_name(sprintf("%s__%s.ibd", base, ts))
  
  message("Uploading imzML to GridFS...")
  imz_id <- unname(grid$upload(imzml_path, name = imz_name))
  
  message("Uploading ibd to GridFS...")
  ibd_id <- unname(grid$upload(ibd_path, name = ibd_name))
  
  meta$insert(list(
    sample_name         = sample_name,
    stage_type          = "raw_files",
    created_at          = Sys.time(),
    imzml_gridfs_id     = as.character(imz_id),
    imzml_gridfs_name   = imz_name,
    ibd_gridfs_id       = as.character(ibd_id),
    ibd_gridfs_name     = ibd_name
  ))
  
  message("✓ Raw files saved to MongoDB")
  invisible(list(imzml_id = as.character(imz_id), ibd_id = as.character(ibd_id)))
}

fetch_raw_pair_from_mongo <- function(sample_name, dest_dir,
                                      db_name = "MSI_database",
                                      mongo_url = "mongodb://localhost:27018",
                                      bucket = "fs") {
  grid <- gridfs(db = db_name, prefix = bucket, url = mongo_url)
  meta <- mongo(collection = "processing_artifacts_metadata", 
                db = db_name, url = mongo_url)
  
  query <- jsonlite::toJSON(
    list(sample_name = sample_name, stage_type = "raw_files"),
    auto_unbox = TRUE
  )
  
  artifacts <- meta$find(query)
  
  if (nrow(artifacts) == 0) {
    stop("No raw_files artifact for sample_name = ", sample_name)
  }
  
  row <- artifacts[nrow(artifacts), , drop = FALSE]
  
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  
  base <- tools::file_path_sans_ext(basename(sample_name))
  final_imzml <- file.path(dest_dir, paste0(base, ".imzML"))
  final_ibd   <- file.path(dest_dir, paste0(base, ".ibd"))
  
  # Download imzML
  imzml_name <- as.character(row$imzml_gridfs_name[1])
  message("Downloading imzML: ", imzml_name)
  grid$download(imzml_name, final_imzml)
  
  if (!file.exists(final_imzml)) {
    stop("imzML download failed - file not found at: ", final_imzml)
  }
  message("✓ imzML: ", file.size(final_imzml), " bytes")
  
  # Download ibd
  ibd_name <- as.character(row$ibd_gridfs_name[1])
  message("Downloading ibd: ", ibd_name)
  grid$download(ibd_name, final_ibd)
  
  if (!file.exists(final_ibd)) {
    stop("ibd download failed - file not found at: ", final_ibd)
  }
  message("✓ ibd: ", file.size(final_ibd), " bytes")
  
  message("✓ Raw files ready at: ", dest_dir)
  
  list(imzml = final_imzml, ibd = final_ibd)
}

load_raw_object_from_mongo <- function(sample_name, workdir,
                                       db_name = "MSI_database",
                                       mongo_url = "mongodb://localhost:27018",
                                       bucket = "fs",
                                       resolution = 10,
                                       BPPARAM = BiocParallel::bpparam()) {
  paths <- fetch_raw_pair_from_mongo(sample_name, workdir, db_name, mongo_url, bucket)
  
  message("Reading imzML with Cardinal...")
  obj <- readMSIData(
    paths$imzml, 
    memory = FALSE, 
    check = FALSE,
    mass.range = NULL, 
    resolution = resolution,
    units = "ppm",
    guess.max = 1000L, 
    as = "auto", 
    parse.only = FALSE,
    verbose = getCardinalVerbose(), 
    chunkopts = list(),
    BPPARAM = BPPARAM
  )
  
  message("✓ MSI object loaded")
  obj
}


# ===== CARDINAL OBJECT SAVE/LOAD AS RDS =====

# Materialize a Cardinal object: forces lazy pipeline, severs .ibd dependency
materialize_cardinal <- function(obj) {
  # Force all pending lazy operations
  obj <- Cardinal::process(obj)

  # Extract components using Cardinal 3.x API
  spec_matrix <- as.matrix(Cardinal::spectra(obj))   # features × pixels
  mz_vals     <- Cardinal::mz(obj)
  coords_df   <- Cardinal::coord(obj)                # data.frame with x, y (and optionally z)
  run_vec     <- Cardinal::run(obj)                  # factor/character vector, length = ncols

  # Rebuild as in-memory MSImagingExperiment (no .ibd file)
  Cardinal::MSImagingExperiment(
    spectraData = S4Vectors::SimpleList(intensity = spec_matrix),
    featureData = Cardinal::MassDataFrame(mz = mz_vals),
    pixelData   = Cardinal::PositionDataFrame(
      coord = coords_df,
      run   = run_vec
    )
  )
}


save_msi_stage_to_mongo <- function(msi_obj, run_id, stage_type,
                                    sample_name,
                                    params   = list(),
                                    db_name  = "MSI_database",
                                    mongo_url = "mongodb://localhost:27018") {

  # Dedup check
  if (stage_type %in% c("control_mean", "snr_reference", "mean_snr_reference")) {
    mongo_meta <- mongo(collection = "processing_artifacts_metadata",
                        db = db_name, url = mongo_url)
    query_list <- list(sample_name = sample_name, stage_type = stage_type)
    if (!is.null(params$resolution)) query_list$resolution <- as.numeric(params$resolution)
    if (!is.null(params$snr))        query_list$snr        <- as.numeric(params$snr)

    existing <- mongo_meta$find(jsonlite::toJSON(query_list, auto_unbox = TRUE))
    if (nrow(existing) > 0) {
      message("⚠ '", stage_type, "' already exists. Skipping save.")
      return(invisible(existing$gridfs_id[1]))
    }
  }

  message("Materializing '", stage_type, "' for serialization...")
  obj_materialized <- materialize_cardinal(msi_obj)

  temp_path <- tempfile(pattern = paste0(stage_type, "_"), fileext = ".rds")
  on.exit(unlink(temp_path), add = TRUE)

  # compress = FALSE is ~3× faster for large matrices
  saveRDS(obj_materialized, temp_path, compress = FALSE)
  size_mb <- file.size(temp_path) / 1024^2
  message(sprintf("  RDS size: %.1f MB", size_mb))

  grid     <- gridfs(db = db_name, prefix = "fs", url = mongo_url)
  filename <- paste0(run_id, "_", stage_type, ".rds")
  grid_result <- grid$upload(temp_path, name = filename)
  grid_id  <- as.character(grid_result$id)

  mongo_meta <- mongo(collection = "processing_artifacts_metadata",
                      db = db_name, url = mongo_url)

  metadata_row <- c(
    list(
      gridfs_id   = grid_id,
      filename    = filename,
      run_id      = run_id,
      sample_name = sample_name,
      stage_type  = stage_type,
      created_at  = Sys.time(),
      file_format = "rds"
    ),
    params
  )
  mongo_meta$insert(metadata_row)

  message(sprintf("✓ Saved '%s' as RDS (%.1f MB, GridFS ID: %s)",
                  stage_type, size_mb, grid_id))
  invisible(grid_id)
}


load_msi_stage_from_mongo <- function(sample_name, stage_type,
                                      run_id     = NULL,
                                      resolution = NULL,
                                      snr        = NULL,
                                      db_name    = "MSI_database",
                                      mongo_url  = "mongodb://localhost:27018",
                                      verbose    = TRUE) {

  if (verbose) message("[load] sample='", sample_name, "', stage='", stage_type, "'")

  meta <- mongo(collection = "processing_artifacts_metadata",
                db = db_name, url = mongo_url)

  query_list <- list(sample_name = sample_name, stage_type = stage_type,
                     file_format = "rds")
  if (!is.null(run_id))    query_list$run_id     <- run_id
  if (!is.null(resolution)) query_list$resolution <- as.numeric(resolution)
  if (!is.null(snr))        query_list$snr        <- as.numeric(snr)

  artifacts <- meta$find(jsonlite::toJSON(query_list, auto_unbox = TRUE))

  if (nrow(artifacts) == 0)
    stop("No RDS artifact for sample='", sample_name, "', stage='", stage_type, "'")

  if (nrow(artifacts) > 1) {
    if (is.list(artifacts$created_at))
      artifacts$created_at <- do.call(c, artifacts$created_at)
    artifacts <- artifacts[order(artifacts$created_at, decreasing = TRUE), ]
  }
  row <- artifacts[1, , drop = FALSE]

  grid_id  <- as.character(row$gridfs_id[1])
  filename <- as.character(row$filename[1])
  if (verbose) message("[load] Downloading ", filename)

  # Temp file cleaned immediately after readRDS
  temp_path <- tempfile(fileext = ".rds")
  on.exit(unlink(temp_path), add = TRUE)

  grid <- gridfs(db = db_name, prefix = "fs", url = mongo_url)
  grid$download(filename, temp_path)
  if (!file.exists(temp_path)) stop("Download failed: ", filename)

  obj <- readRDS(temp_path)
  # temp_path deleted by on.exit

  if (verbose) {
    tryCatch(
      message(sprintf("[load] OK: %d pixels, %d features", ncol(obj), nrow(obj))),
      error = function(e) NULL
    )
  }
  obj
}










# ===== SAVE/LOAD STAGES =====
save_stage_to_mongo <- function(msi_object, run_id, stage_type, 
                                sample_name,
                                params = list(),
                                db_name = "MSI_database", 
                                mongo_url = "mongodb://localhost:27018") {
  
  # Check for duplicates (control_mean and snr_reference only)
  if (stage_type %in% c("control_mean", "snr_reference")) {
    mongo_meta <- mongo(collection = "processing_artifacts_metadata", 
                        db = db_name, url = mongo_url)
    
    query_list <- list(
      sample_name = sample_name,
      stage_type = stage_type
    )
    
    if (stage_type == "snr_reference" && !is.null(params$snr)) {
      query_list$snr <- params$snr
    }
    
    existing <- mongo_meta$find(
      query = jsonlite::toJSON(query_list, auto_unbox = TRUE)
    )
    
    if (nrow(existing) > 0) {
      message("⚠ ", stage_type, " already exists. Skipping save.")
      return(invisible(existing$run_id[1]))
    }
  }
  
  # Save RDS to GridFS
  temp_path <- tempfile(pattern = paste0(stage_type, "_"), fileext = ".rds")
  saveRDS(msi_object, temp_path)
  
  grid <- gridfs(db = db_name, prefix = "fs", url = mongo_url)
  filename <- paste0(run_id, "_", stage_type, ".rds")
  grid_result <- grid$upload(temp_path, name = filename)
  unlink(temp_path)
  
  grid_id <- as.character(grid_result$id)
  
  # Create metadata
  mongo_meta <- mongo(collection = "processing_artifacts_metadata", 
                      db = db_name, url = mongo_url)
  
  metadata_row <- list(
    gridfs_id = grid_id,
    filename = filename,
    run_id = run_id,
    sample_name = sample_name,
    stage_type = stage_type,
    created_at = Sys.time()
  )
  
  metadata_row <- c(metadata_row, params)
  
  mongo_meta$insert(metadata_row)
  
  message("✓ Saved '", stage_type, "' (GridFS ID: ", grid_id, ")")
  invisible(grid_id)
}

load_stage_from_mongo <- function(sample_name, stage_type, run_id = NULL,
                                  db_name = "MSI_database",
                                  mongo_url = "mongodb://localhost:27018") {
  meta <- mongo("processing_artifacts_metadata", db = db_name, url = mongo_url)

  query_list <- list(sample_name = sample_name, stage_type = stage_type)
  if (!is.null(run_id)) query_list$run_id <- run_id

  artifacts <- meta$find(jsonlite::toJSON(query_list, auto_unbox = TRUE))
  if (nrow(artifacts) == 0)
    stop("No artifact found for sample=", sample_name, ", stage=", stage_type)

  row  <- artifacts[nrow(artifacts), , drop = FALSE]
  grid <- gridfs(db = db_name, prefix = "fs", url = mongo_url)
  fname <- as.character(row$filename[1])

  temp_path <- tempfile(fileext = ".rds")
  on.exit(unlink(temp_path), add = TRUE)   # <-- was missing

  message("Downloading ", fname, "...")
  grid$download(fname, temp_path)

  obj <- readRDS(temp_path)
  message("✓ Loaded stage: ", stage_type)
  obj
}

# --- Query artifacts (returns dataframe) ---
query_artifacts <- function(sample_name = NULL, 
                            stage_type = NULL,
                            resolution = NULL,  
                            snr = NULL, 
                            tolerance = NULL,
                            reference_name = NULL,
                            run_id = NULL,
                            db_name = "MSI_database",  
                            mongo_url = "mongodb://localhost:27018") {
  
  mongo_meta <- mongo(collection = "processing_artifacts_metadata", 
                      db = db_name, url = mongo_url)
  
  # Build query
  query_parts <- list()
  if (!is.null(sample_name)) query_parts$sample_name <- sample_name
  if (!is.null(stage_type)) query_parts$stage_type <- stage_type
  if (!is.null(resolution)) query_parts$resolution <- as.numeric(resolution)  # TILFØJET
  if (!is.null(snr)) query_parts$snr <- as.numeric(snr)
  if (!is.null(tolerance)) query_parts$tolerance <- as.numeric(tolerance)
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
                                mongo_url = "mongodb://localhost:27018") {
  grid <- gridfs(db = db_name, prefix = "fs", url = mongo_url)

  query <- sprintf('{"_id": {"$oid": "%s"}}', gridfs_id)
  file_info <- grid$find(query)
  if (nrow(file_info) == 0) stop("GridFS file not found: ", gridfs_id)

  filename  <- file_info$name[1]
  temp_path <- tempfile(fileext = ".rds")
  on.exit(unlink(temp_path), add = TRUE)

  grid$download(filename, temp_path)
  obj <- readRDS(temp_path)
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
                          mongo_url = "mongodb://localhost:27018") {
  
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
                           mongo_url = "mongodb://localhost:27018") {
  
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
                               mongo_url = "mongodb://localhost:27018") {
  
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
      snr = if ("snr" %in% names(row)) row$snr else NULL,
      tolerance = if ("tolerance" %in% names(row)) row$tolerance else NULL,
      reference_name = if ("reference_name" %in% names(row)) row$reference_name else NULL
      # reference_source removed - not used for comparison
    )
  })
  
  stages
}


# --- Find compatible run_id (same sample + raw stage exists) ---
find_compatible_run <- function(sample_name,
                               db_name = "MSI_database",
                               mongo_url = "mongodb://localhost:27018") {
  
  mongo_meta <- mongo(collection = "processing_artifacts_metadata", 
                      db = db_name, url = mongo_url)
  
  # Find raw stage for this sample
  query <- sprintf('{"sample_name": "%s", "stage_type": "raw"}', sample_name)
  results <- mongo_meta$find(query)
  
  if (nrow(results) == 0) return(NULL)
  
  # Return first run_id (assuming one raw per sample)
  results$run_id[1]
}




#======== CLUSTERING ARTIFACTS FUNCTIONS ========#

# --- Query clustering artifacts ---
query_clustering_artifacts <- function(sample_name = NULL,
                                      assignment_id = NULL,
                                      clustering_method = NULL,
                                      snr = NULL,
                                      tolerance = NULL,
                                      reference_name = NULL,
                                      db_name = "MSI_database",
                                      mongo_url = "mongodb://localhost:27018") {
  
  mongo_cluster_meta <- mongo(collection = "clustering_metadata",
                             db = db_name, url = mongo_url)
  
  # Build query
  query_parts <- list()
  if (!is.null(sample_name)) query_parts$sample_name <- sample_name
  if (!is.null(assignment_id)) query_parts$assignment_id <- assignment_id
  if (!is.null(clustering_method)) query_parts$clustering_method <- clustering_method
  if (!is.null(snr)) query_parts$snr <- snr
  if (!is.null(tolerance)) query_parts$tolerance <- tolerance
  if (!is.null(reference_name)) query_parts$reference_name <- reference_name
  
  query_json <- if (length(query_parts) == 0) {
    '{}'
  } else {
    jsonlite::toJSON(query_parts, auto_unbox = TRUE)
  }
  
  results <- mongo_cluster_meta$find(query_json)
  results
}


# --- Load clustering result by assignment_id ---

load_clustering_by_id <- function(assignment_id,
                                  db_name = "MSI_database",
                                  mongo_url = "mongodb://localhost:27018") {
  artifacts <- query_clustering_artifacts(
    assignment_id = assignment_id,
    db_name = db_name, mongo_url = mongo_url
  )
  if (nrow(artifacts) == 0)
    stop("No clustering found with assignment_id: ", assignment_id)

  gridfs_id <- artifacts$gridfs_id[1]
  grid      <- gridfs(db = db_name, prefix = "fs", url = mongo_url)

  query     <- sprintf('{"_id": {"$oid": "%s"}}', gridfs_id)
  file_info <- grid$find(query)
  if (nrow(file_info) == 0) stop("GridFS file not found: ", gridfs_id)

  filename  <- file_info$name[1]
  temp_path <- tempfile(fileext = ".rds")
  on.exit(unlink(temp_path), add = TRUE)

  grid$download(filename, temp_path)
  df <- readRDS(temp_path)

  message("Loaded clustering (assignment_id: ", assignment_id, ")")
  message("  Sample: ", artifacts$sample_name[1])
  message("  Method: ", artifacts$clustering_method[1])
  message("  Dimensions: ", nrow(df), " × ", ncol(df))
  df
}


# --- Load clustering result by query (first match) ---
load_clustering <- function(sample_name = NULL,
                           assignment_id = NULL,
                           clustering_method = NULL,
                           snr = NULL,
                           tolerance = NULL,
                           reference_name = NULL,
                           most_recent = TRUE,
                           db_name = "MSI_database",
                           mongo_url = "mongodb://localhost:27018") {
  
  artifacts <- query_clustering_artifacts(
    sample_name = sample_name,
    assignment_id = assignment_id,
    clustering_method = clustering_method,
    snr = snr,
    tolerance = tolerance,
    reference_name = reference_name,
    db_name = db_name,
    mongo_url = mongo_url
  )
  
  if (nrow(artifacts) == 0) {
    stop("No clustering artifacts found matching query")
  }
  
  if (nrow(artifacts) > 1) {
    message("Multiple clusterings found (", nrow(artifacts), ")")
    
    if (most_recent) {
      # Convert created_at to POSIXct if it's a list
      if (is.list(artifacts$created_at)) {
        artifacts$created_at <- do.call(c, artifacts$created_at)
      }
      # Sort by created_at descending and take first
      artifacts <- artifacts[order(artifacts$created_at, decreasing = TRUE), ]
      message("Loading most recent (", artifacts$created_at[1], ")")
    } else {
      message("Loading first match")
    }
  }
  
  assignment_id <- artifacts$assignment_id[1]
  load_clustering_by_id(assignment_id, db_name, mongo_url)
}