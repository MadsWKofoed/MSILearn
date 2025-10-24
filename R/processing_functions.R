# R/processing_functions.R

process_import_and_summary <- function(imzml_path, ibd_path, imzml_name, run_id) {
  base <- tools::file_path_sans_ext(basename(imzml_name))
  temp_dir <- tempfile(); dir.create(temp_dir)
  temp_imzml <- file.path(temp_dir, paste0(base, ".imzML"))
  temp_ibd   <- file.path(temp_dir, paste0(base, ".ibd"))
  file.copy(imzml_path, temp_imzml, overwrite = TRUE)
  file.copy(ibd_path, temp_ibd, overwrite = TRUE)
  
  message("Reading MSI data...")
  msi_data <- readImzML(temp_imzml, memory = FALSE, check = FALSE,
                        mass.range = NULL, resolution = 10, units = c("ppm"),
                        guess.max = 1000L, as = "auto", parse.only=FALSE,
                        verbose = getCardinalVerbose(), chunkopts = list(),
                        BPPARAM = bpparam())
  
  save_stage_to_mongo(msi_data, run_id, "raw", 
                      sample_name = imzml_name)
  
  message("Summarizing features (mean spectrum)...")
  control_mean <- summarizeFeatures(msi_data, "mean")
  
  # Save control_mean for reuse
  save_stage_to_mongo(control_mean, run_id, "control_mean",
                      sample_name = imzml_name)
  
  invisible(list(msi_data = msi_data, control_mean = control_mean))
}


process_reference_creation <- function(run_id, control_mean, ref_mz_values, snr,
                                       ref_name, ref_source, sample_name) {
  message("Building control MSI reference...")
  control_SNR_ref <- control_mean %>%
    peakPick(SNR = snr)
  
  save_stage_to_mongo(
    control_SNR_ref, 
    run_id, 
    "mean_snr_reference",
    sample_name = sample_name,
    params = list(
      snr = snr
    )
  )
  
  invisible(control_SNR_ref)
}


process_binning_and_matrix <- function(run_id, msi_data, control_SNR_ref, 
                                       ref_mz_values, snr, tolerance, 
                                       ref_name, ref_source, sample_name) {
  
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
  
  save_stage_to_mongo(
    full_df, 
    run_id, 
    "binned_dataframe",
    sample_name = sample_name,
    params = list(
      snr = snr,
      tolerance = tolerance,
      reference_name = ref_name,
      reference_source = ref_source,
      num_features = ncol(full_df) - 3,
      num_pixels = nrow(full_df)
    )
  )
  
  invisible(full_df)
}


process_msi_pipeline <- function(imzml_path, ibd_path, imzml_name,
                                 ref_mz_values, ref_source, ref_name,
                                 snr, tolerance) {
  
  # Check what already exists for this sample
  existing_stages <- get_existing_stages(imzml_name)
  
  # --- STEP 0: Check if EXACT combination already exists ---
  if (!is.null(existing_stages)) {
    exact_binned_match <- any(sapply(existing_stages, function(s) {
      s$stage_type == "binned_dataframe" &&
        identical(s$snr, snr) &&
        identical(s$tolerance, tolerance) &&
        identical(s$reference_name, ref_name)
    }))
    
    if (exact_binned_match) {
      stop("Processing with identical parameters already exists. No action needed.")
    }
  }
  
  # --- STEP 1: Load or import raw data + control_mean ---
  raw_exists <- !is.null(existing_stages) && 
    any(sapply(existing_stages, function(s) s$stage_type == "raw"))
  
  if (raw_exists) {
    message("Raw data already exists. Reusing...")
    
    run_id <- find_compatible_run(imzml_name)
    message("Reusing existing run_id: ", run_id)
    
    raw_artifact <- query_artifacts(sample_name = imzml_name, stage_type = "raw")
    msi_data <- load_artifact_by_id(raw_artifact$gridfs_id[1])
    
    control_mean_artifact <- query_artifacts(sample_name = imzml_name, stage_type = "control_mean")
    
    if (nrow(control_mean_artifact) > 0) {
      message("Loading existing control_mean...")
      control_mean <- load_artifact_by_id(control_mean_artifact$gridfs_id[1])
    } else {
      message("Recalculating control_mean from raw data...")
      control_mean <- summarizeFeatures(msi_data, "mean")
      save_stage_to_mongo(control_mean, run_id, "control_mean", sample_name = imzml_name)
    }
    
  } else {
    message("No existing data found. Starting full import...")
    run_id <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    
    step1 <- process_import_and_summary(imzml_path, ibd_path, imzml_name, run_id)
    msi_data <- step1$msi_data
    control_mean <- step1$control_mean
  }
  
  # --- STEP 2: Load or create reference (FIXED: check before creating) ---
  ref_exists <- !is.null(existing_stages) && 
    any(sapply(existing_stages, function(s) {
      s$stage_type == "mean_snr_reference" &&
        !is.null(s$snr) &&  # Add NULL check
        identical(as.numeric(s$snr), as.numeric(snr))  # Force numeric comparison
    }))
  
  if (ref_exists) {
    message("Reference with SNR=", snr, " already exists. Reusing...")
    
    # Query more precisely
    ref_artifact <- query_artifacts(
      sample_name = imzml_name,
      stage_type = "mean_snr_reference",
      snr = as.numeric(snr)
    )
    
    if (nrow(ref_artifact) > 0) {
      control_SNR_ref <- load_artifact_by_id(ref_artifact$gridfs_id[1])
    } else {
      # Fallback if query fails
      message("Query failed, creating new reference...")
      control_SNR_ref <- process_reference_creation(
        run_id, control_mean, ref_mz_values, snr, 
        ref_name, ref_source, imzml_name
      )
    }
    
  } else {
    message("Creating new reference with SNR=", snr, "...")
    control_SNR_ref <- process_reference_creation(
      run_id, control_mean, ref_mz_values, snr, 
      ref_name, ref_source, imzml_name
    )
  }
  
  # --- STEP 3: Always run binning ---
  message("Running binning (tolerance=", tolerance, ", ref='", ref_name, "')...")
  binned_df <- process_binning_and_matrix(
    run_id, msi_data, control_SNR_ref, ref_mz_values, snr, tolerance,
    ref_name, ref_source, imzml_name
  )
  
  message("✅ Processing complete. Run ID: ", run_id)
  invisible(run_id)
}