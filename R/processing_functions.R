# R/processing_functions.R

process_import_and_summary <- function(imzml_path, ibd_path, imzml_name, run_id) {
  base <- tools::file_path_sans_ext(basename(imzml_name))
  
  # Use a persistent temp directory that won't be garbage collected
  temp_dir <- file.path(tempdir(), "msi_processing", run_id)
  if (!dir.exists(temp_dir)) {
    dir.create(temp_dir, recursive = TRUE)
  }
  
  temp_imzml <- file.path(temp_dir, paste0(base, ".imzML"))
  temp_ibd   <- file.path(temp_dir, paste0(base, ".ibd"))
  
  file.copy(imzml_path, temp_imzml, overwrite = TRUE)
  file.copy(ibd_path, temp_ibd, overwrite = TRUE)
  
  # Ensure files are readable
  Sys.chmod(temp_imzml, mode = "0644")
  Sys.chmod(temp_ibd, mode = "0644")
  
  # Use serial processing to avoid file access issues
  msi_data <- readImzML(temp_imzml, memory = FALSE, check = FALSE,
                        mass.range = NULL, resolution = 10, units = c("ppm"),
                        guess.max = 1000L, as = "auto", parse.only = FALSE,
                        verbose = FALSE, chunkopts = list(),
                        BPPARAM = SerialParam())  # Changed to SerialParam
  
  save_stage_to_mongo(msi_data, run_id, "raw", sample_name = imzml_name)
  
  control_mean <- summarizeFeatures(msi_data, "mean")
  save_stage_to_mongo(control_mean, run_id, "control_mean", sample_name = imzml_name)
  
  # Clean up temp files after saving
  unlink(temp_imzml)
  unlink(temp_ibd)
  
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
                         BPPARAM = SerialParam()) %>%  # Changed to SerialParam
    process()
  
  # ...existing code...
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
    stage_type = "binned_dataframe"
  )
  
  if (nrow(existing_binned) > 0) {
    for (i in seq_len(nrow(existing_binned))) {
      row <- existing_binned[i, ]
      
      snr_match <- !is.null(row$snr) && 
                   isTRUE(all.equal(as.numeric(row$snr), as.numeric(snr)))
      
      tol_match <- !is.null(row$tolerance) && 
                   isTRUE(all.equal(as.numeric(row$tolerance), as.numeric(tolerance)))
      
      ref_match <- !is.null(row$reference_name) && 
                   identical(as.character(row$reference_name), as.character(ref_name))
      
      if (snr_match && tol_match && ref_match) {
        stop("Processing with identical parameters already exists. No action needed.")
      }
    }
  }
  
  existing_stages <- get_existing_stages(imzml_name)
  
  # --- STEP 1: Load or import raw data + control_mean ---
  raw_exists <- !is.null(existing_stages) && 
    any(sapply(existing_stages, function(s) s$stage_type == "raw"))
  
  if (raw_exists) {
    run_id <- find_compatible_run(imzml_name)
    
    raw_artifact <- query_artifacts(sample_name = imzml_name, stage_type = "raw")
    msi_data <- load_artifact_by_id(raw_artifact$gridfs_id[1])
    
    control_mean_artifact <- query_artifacts(sample_name = imzml_name, 
                                             stage_type = "control_mean")
    
    if (nrow(control_mean_artifact) > 0) {
      control_mean <- load_artifact_by_id(control_mean_artifact$gridfs_id[1])
    } else {
      control_mean <- summarizeFeatures(msi_data, "mean")
      save_stage_to_mongo(control_mean, run_id, "control_mean", 
                         sample_name = imzml_name)
    }
    
  } else {
    run_id <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    
    step1 <- process_import_and_summary(imzml_path, ibd_path, imzml_name, run_id)
    msi_data <- step1$msi_data
    control_mean <- step1$control_mean
  }
  
  # --- STEP 2: Load or create reference ---
  existing_refs <- query_artifacts(
    sample_name = imzml_name,
    stage_type = "mean_snr_reference"
  )
  
  ref_exists <- FALSE
  if (nrow(existing_refs) > 0) {
    for (i in seq_len(nrow(existing_refs))) {
      row <- existing_refs[i, ]
      if (!is.null(row$snr) && 
          isTRUE(all.equal(as.numeric(row$snr), as.numeric(snr)))) {
        ref_exists <- TRUE
        existing_ref_id <- row$gridfs_id
        break
      }
    }
  }
  
  if (ref_exists) {
    control_SNR_ref <- load_artifact_by_id(existing_ref_id)
  } else {
    control_SNR_ref <- process_reference_creation(
      run_id, control_mean, ref_mz_values, snr, 
      ref_name, ref_source, imzml_name
    )
  }
  
  # --- STEP 3: Always run binning ---
  binned_df <- process_binning_and_matrix(
    run_id, msi_data, control_SNR_ref, ref_mz_values, snr, tolerance,
    ref_name, ref_source, imzml_name
  )
  
  invisible(run_id)
}
