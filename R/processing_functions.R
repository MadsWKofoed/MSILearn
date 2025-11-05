# R/processing_functions.R

process_import_and_summary <- function(imzml_path, ibd_path, imzml_name, run_id) {
  # Get base name without extension
  base <- tools::file_path_sans_ext(basename(imzml_name))
  
  # Create persistent temp directory for this session
  temp_dir <- file.path(tempdir(), "msi_processing")
  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir, recursive = TRUE)
  }
  
  # CRITICAL: Both files must have the same base name
  temp_imzml <- file.path(temp_dir, paste0(base, ".imzML"))
  temp_ibd   <- file.path(temp_dir, paste0(base, ".ibd"))
  
  # Copy files with correct names
  file.copy(imzml_path, temp_imzml, overwrite = TRUE)
  file.copy(ibd_path, temp_ibd, overwrite = TRUE)
  
  # Verify both files exist
  if (!file.exists(temp_imzml)) {
    stop("Failed to copy imzML file to: ", temp_imzml)
  }
  if (!file.exists(temp_ibd)) {
    stop("Failed to copy ibd file to: ", temp_ibd)
  }
  
  # Read with Cardinal - it will automatically find the .ibd file
  msi_data <- readImzML(
    temp_imzml, 
    memory = TRUE, 
    check = FALSE,
    mass.range = NULL, 
    resolution = 10, 
    units = "ppm",
    guess.max = 1000L, 
    as = "auto", 
    parse.only = FALSE,
    verbose = FALSE, 
    chunkopts = list(),
    BPPARAM = bpparam()
  )
  
  save_stage_to_mongo(msi_data, run_id, "raw", sample_name = imzml_name)
  
  control_mean <- summarizeFeatures(msi_data, "mean")
  save_stage_to_mongo(control_mean, run_id, "control_mean", sample_name = imzml_name)
  
  invisible(list(msi_data = msi_data, control_mean = control_mean))
}


process_reference_creation <- function(run_id, control_mean, ref_mz_values, snr,
                                       ref_name, ref_source, sample_name) {
  control_SNR_ref <- control_mean %>%
    peakPick(SNR = snr)
  
  save_stage_to_mongo(
    control_SNR_ref, 
    run_id, 
    "mean_snr_reference",
    sample_name = sample_name,
    params = list(snr = snr)
  )
  
  invisible(control_SNR_ref)
}


process_binning_and_matrix <- function(run_id, msi_data, control_SNR_ref, 
                                       ref_mz_values, snr, tolerance, 
                                       ref_name, ref_source, sample_name) {
  
  control_MSI_ref <- control_SNR_ref %>%
    peakAlign(ref = ref_mz_values, tolerance = tolerance, units = "mz") %>%
    subsetFeatures() %>%
    process()
  
  msi_data_binned <- bin(msi_data, ref = mz(control_MSI_ref),
                         tolerance = tolerance, units = "mz", 
                         BPPARAM = bpparam()) %>%
    process()
  
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
  
  # --- STEP 0: Check if EXACT combination already exists ---
  existing_binned <- query_artifacts(
    sample_name = imzml_name,
    stage_type = "binned_dataframe",
    snr = as.numeric(snr),
    tolerance = as.numeric(tolerance),
    reference_name = ref_name
  )
  
  if (nrow(existing_binned) > 0) {
    stop("Processing with identical parameters already exists. No action needed.")
  }
  
  # --- STEP 1: Try to load reference with same SNR (most downstream stage before binning) ---
  existing_refs <- query_artifacts(
    sample_name = imzml_name,
    stage_type = "mean_snr_reference",
    snr = as.numeric(snr)
  )
  
  if (nrow(existing_refs) > 0) {
    message("✓ Found existing reference with SNR=", snr)
    
    # Load from reference stage
    run_id <- find_compatible_run(imzml_name)
    control_SNR_ref <- load_artifact_by_id(existing_refs$gridfs_id[1])
    
    # Load raw data (needed for binning)
    raw_artifact <- query_artifacts(sample_name = imzml_name, stage_type = "raw")
    msi_data <- load_artifact_by_id(raw_artifact$gridfs_id[1])
    
    message("✓ Loaded raw data")
    message("→ Skipping: import, control_mean calculation, peak picking")
    message("→ Running: binning with new tolerance/reference")
    
  } else {
    message("✗ No reference with SNR=", snr, " found")
    
    # --- STEP 2: Try to load control_mean (next level up) ---
    existing_mean <- query_artifacts(
      sample_name = imzml_name,
      stage_type = "control_mean"
    )
    
    if (nrow(existing_mean) > 0) {
      message("✓ Found existing control_mean")
      
      run_id <- find_compatible_run(imzml_name)
      control_mean <- load_artifact_by_id(existing_mean$gridfs_id[1])
      
      # Load raw data (needed for binning)
      raw_artifact <- query_artifacts(sample_name = imzml_name, stage_type = "raw")
      msi_data <- load_artifact_by_id(raw_artifact$gridfs_id[1])
      
      message("✓ Loaded raw data")
      message("→ Skipping: import, control_mean calculation")
      message("→ Running: peak picking, binning")
      
      # Create reference
      control_SNR_ref <- process_reference_creation(
        run_id, control_mean, ref_mz_values, snr, 
        ref_name, ref_source, imzml_name
      )
      
    } else {
      message("✗ No control_mean found")
      
      # --- STEP 3: Try to load raw (next level up) ---
      existing_raw <- query_artifacts(
        sample_name = imzml_name,
        stage_type = "raw"
      )
      
      if (nrow(existing_raw) > 0) {
        message("✓ Found existing raw data")
        
        run_id <- find_compatible_run(imzml_name)
        msi_data <- load_artifact_by_id(existing_raw$gridfs_id[1])
        
        message("→ Skipping: import")
        message("→ Running: control_mean calculation, peak picking, binning")
        
        # Create control_mean
        control_mean <- summarizeFeatures(msi_data, "mean")
        save_stage_to_mongo(control_mean, run_id, "control_mean", 
                           sample_name = imzml_name)
        
        # Create reference
        control_SNR_ref <- process_reference_creation(
          run_id, control_mean, ref_mz_values, snr, 
          ref_name, ref_source, imzml_name
        )
        
      } else {
        message("✗ No raw data found")
        message("→ Running: full pipeline from import")
        
        # --- STEP 4: Import from files (nothing exists) ---
        run_id <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
        
        step1 <- process_import_and_summary(imzml_path, ibd_path, imzml_name, run_id)
        msi_data <- step1$msi_data
        control_mean <- step1$control_mean
        
        # Create reference
        control_SNR_ref <- process_reference_creation(
          run_id, control_mean, ref_mz_values, snr, 
          ref_name, ref_source, imzml_name
        )
      }
    }
  }
  
  # --- FINAL STEP: Always run binning (only step that varies by tolerance/reference) ---
  message("→ Running binning with tolerance=", tolerance, ", reference=", ref_name)
  binned_df <- process_binning_and_matrix(
    run_id, msi_data, control_SNR_ref, ref_mz_values, snr, tolerance,
    ref_name, ref_source, imzml_name
  )
  
  invisible(run_id)
}
