# R/processing_functions.R

# process_msi_files <- function(imzml_path, ibd_path, imzml_name, ref_mz_path) {
#   message("Copying uploaded files to a temporary directory...")
#   temp_dir <- tempfile()
#   dir.create(temp_dir)
#   
#   # use original name to build consistent basename
#   base <- tools::file_path_sans_ext(basename(imzml_name))
#   temp_imzml <- file.path(temp_dir, paste0(base, ".imzML"))
#   temp_ibd   <- file.path(temp_dir, paste0(base, ".ibd"))
#   
#   file.copy(imzml_path, temp_imzml, overwrite = TRUE)
#   file.copy(ibd_path,   temp_ibd,   overwrite = TRUE)
#   
#   message("Reading MSI data...")
#   msi_data <- readImzML(temp_imzml, , memory = FALSE, check = FALSE,
#                         mass.range = NULL, resolution = 10, units = c("ppm"),
#                         guess.max = 1000L, as = "auto", parse.only=FALSE,
#                         verbose = getCardinalVerbose(), chunkopts = list(),
#                         BPPARAM = bpparam())
#   
#   message("Summarizing reference sample...")
#   control_mean <- summarizeFeatures(msi_data, "mean")
#   ref_mz <- read.csv(ref_mz_path)
#   
#   control_MSI_ref <- control_mean %>%
#     peakPick(SNR = 3) %>%
#     peakAlign(ref = as.numeric(ref_mz[, 1]), tolerance = 0.5, units = "mz") %>%
#     subsetFeatures() %>%
#     process()
#   
#   message("Binning MSI data...")
#   msi_data <- bin(msi_data, ref = mz(control_MSI_ref),
#                   tolerance = 0.5, units = "mz", BPPARAM = bpparam()) %>% process()
#   
#   message("Building feature matrix...")
#   msi_matrix <- t(as.matrix(spectra(msi_data)))
#   mz_names <- paste0("mz_", mz(msi_data))
#   coords <- coord(msi_data)
#   run_id <- runNames(msi_data)
#   pixel_names <- rep(run_id, nrow(msi_matrix))
#   
#   full_df <- data.frame(
#     runNames = pixel_names,
#     x = coords$x,
#     y = coords$y,
#     msi_matrix
#   )
#   colnames(full_df) <- c("runNames", "x", "y", mz_names)
#   
#   full_df
# }



# process_msi_files <- function(imzml_path, ibd_path, imzml_name,
#                               ref_mz_values, ref_source, ref_name,
#                               snr, tolerance) {
#   
#   base <- tools::file_path_sans_ext(basename(imzml_name))
#   run_id <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
#   
#   mongo_meta <- mongo(collection = "processing_runs", db = "MSI_database")
#   grid <- gridfs(db = "MSI_database")
#   
#   mongo_meta$insert(list(
#     run_id = run_id,
#     sample_name = imzml_name,
#     created_at = Sys.time(),
#     parameters = list(
#       SNR = snr,
#       tolerance = tolerance,
#       reference_source = ref_source,
#       reference_name = ref_name
#     ),
#     stages = structure(list(), names = character(0))  
#   ))
#   
#   message("Reading raw MSI data...")
#   temp_dir <- tempfile()
#   dir.create(temp_dir)
#   temp_imzml <- file.path(temp_dir, paste0(base, ".imzML"))
#   temp_ibd   <- file.path(temp_dir, paste0(base, ".ibd"))
#   file.copy(imzml_path, temp_imzml, overwrite = TRUE)
#   file.copy(ibd_path, temp_ibd, overwrite = TRUE)
#   
  # msi_data <- readImzML(temp_imzml, , memory = FALSE, check = FALSE,
  #                       mass.range = NULL, resolution = 10, units = c("ppm"),
  #                       guess.max = 1000L, as = "auto", parse.only=FALSE,
  #                       verbose = getCardinalVerbose(), chunkopts = list(),
  #                       BPPARAM = bpparam())
#   save_stage_to_mongo(msi_data, run_id, "raw")
#   
#   # Summary
#   msi_summary <- summarizeFeatures(msi_data, "mean")
#   save_stage_to_mongo(msi_summary, run_id, "summary",
#                       params = list(method = "mean"))
#   
#   # SNR
#   msi_data_snr <- peakPick(msi_data, SNR = snr)
#   save_stage_to_mongo(msi_data_snr, run_id, "snr", params = list(SNR = snr))
#   
#   # Align + bin using user’s reference
#   msi_data_aligned <- msi_data_snr %>%
#     peakAlign(ref = ref_mz_values, tolerance = tolerance, units = "mz") %>%
#     bin(ref = ref_mz_values, tolerance = tolerance, units = "mz") %>%
#     process()
#   
#   save_stage_to_mongo(msi_data_aligned, run_id, "aligned_binned",
#                       params = list(tolerance = tolerance,
#                                     reference_name = ref_name,
#                                     reference_source = ref_source))
#   
#   message("Processing complete. Run ID: ", run_id)
#   return(run_id)
# }




process_import_and_summary <- function(imzml_path, ibd_path, imzml_name, run_id) {
  base <- tools::file_path_sans_ext(basename(imzml_name))
  temp_dir <- tempfile(); dir.create(temp_dir)
  temp_imzml <- file.path(temp_dir, paste0(base, ".imzML"))
  temp_ibd   <- file.path(temp_dir, paste0(base, ".ibd"))
  file.copy(imzml_path, temp_imzml, overwrite = TRUE)
  file.copy(ibd_path, temp_ibd, overwrite = TRUE)
  
  message("Reading MSI data...")
  msi_data <- readImzML(temp_imzml, , memory = FALSE, check = FALSE,
                        mass.range = NULL, resolution = 10, units = c("ppm"),
                        guess.max = 1000L, as = "auto", parse.only=FALSE,
                        verbose = getCardinalVerbose(), chunkopts = list(),
                        BPPARAM = bpparam())
  save_stage_to_mongo(msi_data, run_id, "raw")
  
  message("Summarizing features (mean spectrum)...")
  control_mean <- summarizeFeatures(msi_data, "mean")
  
  invisible(list(msi_data = msi_data, control_mean = control_mean))
}



process_reference_creation <- function(run_id, control_mean, ref_mz_values, snr,
                                       ref_name, ref_source) {
  message("Building control MSI reference...")
  control_SNR_ref <- control_mean %>%
    peakPick(SNR = snr)
  
  invisible(control_SNR_ref)
}




process_binning_and_matrix <- function(run_id, msi_data, control_SNR_ref, ref_mz_values, snr, tolerance) {
  
  message("Aligning control reference (applying tolerance)...")
  control_MSI_ref <- control_SNR_ref %>%
    peakAlign(ref = ref_mz_values, tolerance = tolerance, units = "mz") %>%
    subsetFeatures() %>%
    process()
  
  message("Binning MSI data...")
  msi_data_binned <- bin(msi_data, ref = mz(control_MSI_ref),
                         tolerance = tolerance, units = "mz", BPPARAM = bpparam()) %>%
    process()
  
  message("Building feature matrix dataframe...")
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
  
  
  invisible(full_df)
}



process_msi_pipeline <- function(imzml_path, ibd_path, imzml_name,
                                 ref_mz_values, ref_source, ref_name,
                                 snr, tolerance) {
  
  mongo_meta <- mongo(collection = "processing_runs", db = "MSI_database", url = "mongodb://localhost")
  
  # --- Check if dataset already exists in DB ---
  existing_run <- mongo_meta$find(paste0('{"sample_name": "', imzml_name, '"}'))
  
  if (nrow(existing_run) > 0) {
    # Dataset already in DB → reuse raw
    run_id <- existing_run$run_id[[1]]
    message("Found existing dataset for sample: ", imzml_name)
    
    stages <- colnames(existing_run$stages)
    raw_available <- "raw" %in% stages
    
    if (!raw_available) {
      stop("Existing record found, but no raw stage present in database.")
    }
    
    # --- Load existing raw stage ---
    message("Loading existing raw MSI data from database...")
    raw <- load_stage_from_mongo(run_id, "raw")
    
  } else {
    # --- New dataset: create new run and process raw stage ---
    run_id <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    message("Creating new run: ", run_id)
    
    mongo_meta$insert(list(
      run_id = run_id,
      sample_name = imzml_name,
      created_at = Sys.time(),
      parameters = list(
        reference_source = ref_source,
        reference_name = ref_name
      ),
      stages = setNames(list(), character(0))  # Initialize stages as an empty object
    ))
    
    # Import raw data and save
    step1 <- process_import_and_summary(imzml_path, ibd_path, imzml_name, run_id)
    raw <- step1$msi_data
  }
  
  # --- Build parameter-specific stage names ---
  snr_tag <- gsub("\\.", "_", sprintf("%.3f", snr))
  tol_tag <- gsub("\\.", "_", sprintf("%.3f", tolerance))
  
  mean_stage_name <- paste0("mean_snr_reference_SNR", snr_tag)
  binned_stage_name <- paste0("binned_dataframe_SNR", snr_tag, "_tol", tol_tag)
  
  # --- Check if this parameter version already exists ---
  run_doc <- mongo_meta$find(paste0('{"run_id": "', run_id, '"}'))
  existing_stages <- names(run_doc$stages[[1]])
  
  if (binned_stage_name %in% existing_stages) {
    message("⚠️ Dataset with SNR=", snr, " and tolerance=", tolerance, " already exists. Skipping.")
    showNotification(
      paste("This parameter combination (SNR=", snr, ", tol=", tolerance, ") already exists for this dataset."),
      type = "warning", duration = 8
    )
    return(invisible(run_id))
  }
  
  # --- Process missing reference and binned stages ---
  message("Processing new version: SNR=", snr, " tol=", tolerance)
  
  # Generate mean spectrum and reference
  control_mean <- summarizeFeatures(raw, "mean")
  control_SNR_ref <- process_reference_creation(run_id, control_mean,
                                                ref_mz_values, snr,
                                                ref_name, ref_source)
  
  # Save versioned reference stage
  save_stage_to_mongo(control_SNR_ref, run_id, mean_stage_name,
                      params = list(SNR = snr, reference_name = ref_name, reference_source = ref_source))
  
  # Build new binned matrix
  df <- process_binning_and_matrix(run_id, raw, control_SNR_ref,
                                   ref_mz_values, snr, tolerance)
  
  # Save versioned binned dataframe
  save_stage_to_mongo(df, run_id, binned_stage_name,
                      params = list(SNR = snr, tolerance = tolerance, columns = ncol(df)))
  
  message("✅ Processing complete for ", imzml_name, " (run_id=", run_id, ")")
  showNotification(paste("Processing complete for", imzml_name, "with SNR=", snr, " tol=", tolerance),
                   type = "message", duration = 8)
  invisible(run_id)
}

















