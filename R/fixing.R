library(mongolite)
library(Cardinal)
library(BiocParallel)
library(dplyr)

bp <- parallel::detectCores() - 1
setCardinalParallel(workers = bp)

# ===== UTILITY FUNCTIONS =====
sanitize_name <- function(x) gsub("[^A-Za-z0-9._-]+", "_", x)

# ===== RAW FILE STORAGE (imzML + ibd) =====
save_raw_pair_to_mongo <- function(sample_name, imzml_path, ibd_path,
                                   db_name = "MSI_test_database",
                                   mongo_url = "mongodb://localhost",
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
                                      db_name = "MSI_test_database",
                                      mongo_url = "mongodb://localhost",
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
  
  # Use most recent
  row <- artifacts[nrow(artifacts), , drop = FALSE]
  
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  base <- tools::file_path_sans_ext(basename(sample_name))
  out_imzml <- file.path(dest_dir, paste0(base, ".imzML"))
  out_ibd   <- file.path(dest_dir, paste0(base, ".ibd"))
  
  # Download imzML
  imzml_name <- as.character(row$imzml_gridfs_name[1])
  message("Downloading imzML: ", imzml_name)
  grid$download(imzml_name, out_imzml)
  
  # Download ibd
  ibd_name <- as.character(row$ibd_gridfs_name[1])
  message("Downloading ibd: ", ibd_name)
  grid$download(ibd_name, out_ibd)
  
  message("✓ Raw files downloaded to: ", dest_dir)
  list(imzml = out_imzml, ibd = out_ibd)
}

# ===== LOAD RAW OBJECT FROM MONGO =====
load_raw_object_from_mongo <- function(sample_name, workdir,
                                       db_name = "MSI_test_database",
                                       mongo_url = "mongodb://localhost",
                                       bucket = "fs",
                                       materialize = FALSE,
                                       BPPARAM = BiocParallel::bpparam()) {
  paths <- fetch_raw_pair_from_mongo(sample_name, workdir, db_name, mongo_url, bucket)
  
  message("Reading imzML with Cardinal...")
  obj <- readImzML(
    paths$imzml, 
    memory = FALSE, 
    check = FALSE,
    mass.range = NULL, 
    resolution = 10, 
    units = "ppm",
    guess.max = 1000L, 
    as = "auto", 
    parse.only = FALSE,
    verbose = getCardinalVerbose(), 
    chunkopts = list(),
    BPPARAM = BPPARAM
  )
  
  if (isTRUE(materialize)) {
    message("Materializing data...")
    obj <- process(obj)
  }
  
  message("✓ MSI object loaded")
  obj
}

# ===== SAVE PROCESSING STAGES (RDS) =====
save_stage_to_mongo <- function(msi_object, run_id, stage_type, 
                                sample_name,
                                params = list(),
                                db_name = "MSI_test_database",
                                mongo_url = "mongodb://localhost",
                                materialize = FALSE) {
  
  if (isTRUE(materialize)) {
    message("Materializing before saving...")
    try(msi_object <- Cardinal::process(msi_object), silent = TRUE)
  }
  
  tmp <- tempfile(pattern = paste0(stage_type, "_"), fileext = ".rds")
  message("Saving RDS to temp file...")
  saveRDS(msi_object, tmp)
  
  grid <- gridfs(db = db_name, prefix = "fs", url = mongo_url)
  fname <- paste0(run_id, "_", stage_type, ".rds")
  
  message("Uploading ", fname, " to GridFS...")
  grid_result <- grid$upload(tmp, name = fname)
  gid <- as.character(grid_result$id)
  
  meta <- mongo("processing_artifacts_metadata", db = db_name, url = mongo_url)
  row <- c(list(
    gridfs_id   = gid,
    filename    = fname,
    run_id      = run_id,
    sample_name = sample_name,
    stage_type  = stage_type,
    created_at  = Sys.time()
  ), params)
  
  meta$insert(row)
  message("✓ Saved stage: ", stage_type)
  
  invisible(gid)
}

# ===== LOAD PROCESSING STAGE =====
load_stage_from_mongo <- function(sample_name, stage_type, run_id = NULL,
                                  db_name = "MSI_test_database",
                                  mongo_url = "mongodb://localhost") {
  
  meta <- mongo("processing_artifacts_metadata", db = db_name, url = mongo_url)
  
  query_list <- list(
    sample_name = sample_name,
    stage_type = stage_type
  )
  
  if (!is.null(run_id)) {
    query_list$run_id <- run_id
  }
  
  query <- jsonlite::toJSON(query_list, auto_unbox = TRUE)
  artifacts <- meta$find(query)
  
  if (nrow(artifacts) == 0) {
    stop("No artifact found for sample=", sample_name, ", stage=", stage_type)
  }
  
  # Use most recent
  row <- artifacts[nrow(artifacts), , drop = FALSE]
  
  grid <- gridfs(db = db_name, prefix = "fs", url = mongo_url)
  fname <- as.character(row$filename[1])
  
  message("Downloading ", fname, "...")
  temp_dir <- tempdir()
  grid$download(fname, temp_dir)
  
  downloaded_path <- file.path(temp_dir, fname)
  message("Loading RDS...")
  obj <- readRDS(downloaded_path)
  
  message("✓ Loaded stage: ", stage_type)
  obj
}

# ===== COMPLETE TEST WORKFLOW =====
test_complete_workflow <- function() {
  
  # Configuration
  imzml_file  <- "tumorinfiltrat.imzML"
  ibd_file    <- "tumorinfiltrat.ibd"
  sample_name <- "tumorinfiltrat.imzML"
  db_name     <- "MSI_test_database"
  run_id      <- paste0("test_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  
  message("\n===== STEP 1: Save raw files to MongoDB =====")
  save_raw_pair_to_mongo(
    sample_name = sample_name,
    imzml_path  = imzml_file,
    ibd_path    = ibd_file,
    db_name     = db_name
  )
  
  message("\n===== STEP 2: Load raw files and create MSI object =====")
  workdir <- file.path("data_cache_test", tools::file_path_sans_ext(sample_name))
  msi_data <- load_raw_object_from_mongo(
    sample_name = sample_name,
    workdir     = workdir,
    db_name     = db_name,
    materialize = FALSE
  )
  
  message("\n===== STEP 3: Calculate mean spectrum =====")
  control_mean <- summarizeFeatures(msi_data, "mean")
  
  save_stage_to_mongo(
    control_mean, 
    run_id, 
    "control_mean",
    sample_name = sample_name,
    db_name = db_name,
    materialize = TRUE
  )
  
  message("\n===== STEP 4: Load reference m/z from database =====")
  mongo_ref <- mongo(collection = "mz_references",
                     db = "msi_project", 
                     url = "mongodb://localhost")
  
  ref_doc <- mongo_ref$find(
    query = '{"reference_name": "113_lipids_gangliosides"}'
  )
  
  if (nrow(ref_doc) == 0) {
    stop("Reference '113_lipids_gangliosides' not found in database")
  }
  
  # Extract mz_values correctly - it's a list containing a vector
  ref_mz <- unlist(ref_doc$mz_values[[1]])
  reference_name <- ref_doc$reference_name[1]
  
  message("Loaded reference '", reference_name, "' with ", length(ref_mz), " m/z values")
  message("First 10 m/z values: ", paste(head(ref_mz, 10), collapse = ", "))
  
  message("\n===== STEP 5: Apply tolerance and create reference =====")
  snr_value <- 3
  tolerance_value <- 0.5
  
  # Apply SNR to mean spectrum first
  control_SNR_ref <- control_mean %>%
    peakPick(SNR = snr_value)
  
  save_stage_to_mongo(
    control_SNR_ref,
    run_id,
    "snr_reference",
    sample_name = sample_name,
    params = list(snr = snr_value),
    db_name = db_name,
    materialize = TRUE
  )
  
  # Align to the database reference
  control_MSI_ref <- control_SNR_ref %>%
    peakAlign(ref = ref_mz, tolerance = tolerance_value, units = "mz") %>%
    subsetFeatures() %>%
    process()
  
  save_stage_to_mongo(
    control_MSI_ref,
    run_id,
    "aligned_reference",
    sample_name = sample_name,
    params = list(
      snr = snr_value, 
      tolerance = tolerance_value,
      reference_name = reference_name
    ),
    db_name = db_name,
    materialize = FALSE
  )
  
  message("\n===== STEP 6: Bin full MSI data =====")
  msi_data_binned <- bin(
    msi_data, 
    ref = mz(control_MSI_ref),
    tolerance = tolerance_value, 
    units = "mz", 
    BPPARAM = BiocParallel::bpparam()
  ) %>% process()
  
  save_stage_to_mongo(
    msi_data_binned,
    run_id,
    "binned_msi",
    sample_name = sample_name,
    params = list(
      snr = snr_value, 
      tolerance = tolerance_value,
      reference_name = reference_name
    ),
    db_name = db_name,
    materialize = FALSE
  )
  
  message("\n===== STEP 7: Create feature matrix dataframe =====")
  msi_matrix <- t(as.matrix(spectra(msi_data_binned)))
  mz_names <- paste0("mz_", mz(msi_data_binned))
  coords <- coord(msi_data_binned)
  run_name <- runNames(msi_data_binned)
  pixel_names <- rep(run_name, nrow(msi_matrix))
  
  full_df <- data.frame(
    runNames = pixel_names,
    x = coords$x,
    y = coords$y,
    msi_matrix
  )
  colnames(full_df) <- c("runNames", "x", "y", mz_names)
  
  message("Final dataframe dimensions: ", nrow(full_df), " pixels × ", 
          sum(grepl("^mz_", names(full_df))), " m/z features")
  
  save_stage_to_mongo(
    full_df,
    run_id,
    "binned_dataframe",
    sample_name = sample_name,
    params = list(
      snr = snr_value, 
      tolerance = tolerance_value,
      reference_name = reference_name,
      num_features = ncol(full_df) - 3,
      num_pixels = nrow(full_df)
    ),
    db_name = db_name,
    materialize = FALSE
  )
  
  message("\n===== TEST LOADING STAGES =====")
  
  message("\nLoading control_mean...")
  loaded_mean <- load_stage_from_mongo(
    sample_name = sample_name,
    stage_type = "control_mean",
    run_id = run_id,
    db_name = db_name
  )
  print(loaded_mean)
  
  message("\nLoading binned_dataframe...")
  loaded_df <- load_stage_from_mongo(
    sample_name = sample_name,
    stage_type = "binned_dataframe",
    run_id = run_id,
    db_name = db_name
  )
  print(dim(loaded_df))
  print(head(loaded_df[, 1:5]))
  
  message("\n✅ COMPLETE WORKFLOW TEST SUCCESSFUL!")
  message("Run ID: ", run_id)
  message("Reference used: ", reference_name)
  message("Final features: ", sum(grepl("^mz_", names(full_df))))
  
  invisible(list(
    run_id = run_id,
    msi_data = msi_data,
    final_df = full_df,
    reference_used = reference_name
  ))
}

# ===== RUN TEST =====
if (interactive()) {
  message("Run the test with: test_complete_workflow()")
}





# ===== TEST LOADING FROM SCRATCH =====
test_load_from_database <- function(sample_name = "tumorinfiltrat.imzML",
                                    run_id = NULL,
                                    db_name = "MSI_test_database") {
  
  message("\n===== TESTING LOAD FROM DATABASE (FRESH R SESSION) =====")
  
  # Clear workspace to ensure nothing is in memory
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
  gc()
  
  message("\n1. Loading raw files from MongoDB...")
  workdir <- file.path("data_cache_test_reload", tools::file_path_sans_ext(sample_name))
  
  # Remove existing cache directory if it exists
  if (dir.exists(workdir)) {
    message("Removing existing cache directory: ", workdir)
    unlink(workdir, recursive = TRUE)
  }
  
  msi_data <- load_raw_object_from_mongo(
    sample_name = sample_name,
    workdir = workdir,
    db_name = db_name,
    materialize = FALSE
  )
  
  message("✓ Raw data loaded from MongoDB")
  message("  Dimensions: ", nrow(msi_data), " pixels, ", ncol(msi_data), " m/z values")
  
  message("\n2. Loading control_mean stage...")
  control_mean <- load_stage_from_mongo(
    sample_name = sample_name,
    stage_type = "control_mean",
    run_id = run_id,
    db_name = db_name
  )
  message("✓ control_mean loaded")
  print(control_mean)
  
  message("\n3. Loading snr_reference stage...")
  snr_ref <- load_stage_from_mongo(
    sample_name = sample_name,
    stage_type = "snr_reference",
    run_id = run_id,
    db_name = db_name
  )
  message("✓ snr_reference loaded")
  print(snr_ref)
  
  message("\n4. Loading aligned_reference stage...")
  aligned_ref <- load_stage_from_mongo(
    sample_name = sample_name,
    stage_type = "aligned_reference",
    run_id = run_id,
    db_name = db_name
  )
  message("✓ aligned_reference loaded")
  message("  Number of aligned m/z: ", length(mz(aligned_ref)))
  
  message("\n5. Loading binned_msi stage...")
  binned_msi <- load_stage_from_mongo(
    sample_name = sample_name,
    stage_type = "binned_msi",
    run_id = run_id,
    db_name = db_name
  )
  message("✓ binned_msi loaded")
  message("  Dimensions: ", nrow(binned_msi), " pixels, ", ncol(binned_msi), " m/z bins")
  
  message("\n6. Loading binned_dataframe stage...")
  final_df <- load_stage_from_mongo(
    sample_name = sample_name,
    stage_type = "binned_dataframe",
    run_id = run_id,
    db_name = db_name
  )
  message("✓ binned_dataframe loaded")
  message("  Dimensions: ", nrow(final_df), " rows × ", ncol(final_df), " columns")
  message("  Feature columns: ", sum(grepl("^mz_", names(final_df))))
  
  message("\n7. Verifying data integrity...")
  message("  First 5 rows, first 8 columns:")
  print(head(final_df[, 1:min(8, ncol(final_df))], 5))
  
  message("\n✅ ALL STAGES LOADED SUCCESSFULLY FROM DATABASE!")
  message("Cache directory created at: ", workdir)
  message("You can delete this directory to verify files came from MongoDB")
  
  invisible(list(
    msi_data = msi_data,
    control_mean = control_mean,
    snr_ref = snr_ref,
    aligned_ref = aligned_ref,
    binned_msi = binned_msi,
    final_df = final_df
  ))
}

# ===== COMPLETE VERIFICATION TEST =====
test_full_cycle <- function() {
  
  message("\n╔═══════════════════════════════════════════════════════╗")
  message("║  COMPLETE DATABASE STORAGE AND RETRIEVAL TEST        ║")
  message("╚═══════════════════════════════════════════════════════╝")
  
  # Step 1: Run complete workflow
  message("\n[PHASE 1] Running complete workflow...")
  result <- test_complete_workflow()
  run_id <- result$run_id
  
  message("\n[PHASE 1 COMPLETE] Data saved to database with run_id: ", run_id)
  message("Press Enter to continue to Phase 2 (reload test)...")
  readline()
  
  # Step 2: Clear environment
  message("\n[PHASE 2] Clearing R environment...")
  rm(list = ls(envir = .GlobalEnv), envir = .GlobalEnv)
  gc()
  
  message("Environment cleared. All objects removed from memory.")
  message("Press Enter to reload from database...")
  readline()
  
  # Step 3: Reload everything
  message("\n[PHASE 2] Reloading all data from MongoDB...")
  reloaded <- test_load_from_database(
    sample_name = "tumorinfiltrat.imzML",
    run_id = run_id,
    db_name = "MSI_test_database"
  )
  
  message("\n╔═══════════════════════════════════════════════════════╗")
  message("║  ✅ VERIFICATION COMPLETE                             ║")
  message("║                                                       ║")
  message("║  All data successfully saved to and loaded from      ║")
  message("║  MongoDB database.                                   ║")
  message("╚═══════════════════════════════════════════════════════╝")
  
  invisible(reloaded)
}



# ===== INSPECT METADATA =====
inspect_metadata <- function(sample_name = NULL, 
                            stage_type = NULL,
                            run_id = NULL,
                            db_name = "MSI_test_database",
                            mongo_url = "mongodb://localhost",
                            limit = 0) {  # Change from NULL to 0 (0 means no limit)
  
  meta <- mongo("processing_artifacts_metadata", db = db_name, url = mongo_url)
  
  # Build query
  query_list <- list()
  if (!is.null(sample_name)) query_list$sample_name <- sample_name
  if (!is.null(stage_type)) query_list$stage_type <- stage_type
  if (!is.null(run_id)) query_list$run_id <- run_id
  
  # Execute query
  if (length(query_list) == 0) {
    query <- "{}"
  } else {
    query <- jsonlite::toJSON(query_list, auto_unbox = TRUE)
  }
  
  # Only pass limit if it's greater than 0
  if (limit > 0) {
    artifacts <- meta$find(query, limit = limit)
  } else {
    artifacts <- meta$find(query)
  }
  
  if (nrow(artifacts) == 0) {
    message("No artifacts found matching criteria")
    return(invisible(NULL))
  }
  
  message("\n===== METADATA INSPECTION =====")
  message("Total artifacts found: ", nrow(artifacts))
  message("\nSummary by stage_type:")
  print(table(artifacts$stage_type))
  
  if (!is.null(artifacts$run_id)) {
    message("\nUnique run_ids: ", length(unique(artifacts$run_id)))
    print(unique(artifacts$run_id))
  }
  
  message("\n===== DETAILED VIEW =====")
  print(artifacts)
  
  invisible(artifacts)
}