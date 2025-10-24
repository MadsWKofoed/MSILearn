# R/processing_functions.R

process_import_and_summary <- function(imzml_path, ibd_path, imzml_name, run_id) {
  base <- tools::file_path_sans_ext(basename(imzml_name))
  temp_dir <- tempfile(); dir.create(temp_dir)
  temp_imzml <- file.path(temp_dir, paste0(base, ".imzML"))
  temp_ibd   <- file.path(temp_dir, paste0(base, ".ibd"))
  
  message("📁 Copying files to temporary directory...")
  file.copy(imzml_path, temp_imzml, overwrite = TRUE)
  file.copy(ibd_path, temp_ibd, overwrite = TRUE)
  
  message("📖 Reading MSI data from imzML file...")
  message("   This may take several minutes for large datasets...")
  msi_data <- readImzML(temp_imzml, memory = FALSE, check = FALSE,
                        mass.range = NULL, resolution = 10, units = c("ppm"),
                        guess.max = 1000L, as = "auto", parse.only=FALSE,
                        verbose = getCardinalVerbose(), chunkopts = list(),
                        BPPARAM = bpparam())
  
  message("💾 Saving raw data to database (GridFS)...")
  save_stage_to_mongo(msi_data, run_id, "raw", 
                      sample_name = imzml_name)
  
  message("📊 Computing mean spectrum across all pixels...")
  control_mean <- summarizeFeatures(msi_data, "mean")
  
  message("💾 Saving control mean to database...")
  save_stage_to_mongo(control_mean, run_id, "control_mean",
                      sample_name = imzml_name)
  
  message("✅ Import complete")
  invisible(list(msi_data = msi_data, control_mean = control_mean))
}


process_reference_creation <- function(run_id, control_mean, ref_mz_values, snr,
                                       ref_name, ref_source, sample_name) {
  message("🔬 Building reference spectrum with SNR=", snr, "...")
  message("   Detecting peaks above signal-to-noise threshold...")
  control_SNR_ref <- control_mean %>%
    peakPick(SNR = snr)
  
  n_peaks <- length(mz(control_SNR_ref))
  message("   Found ", n_peaks, " peaks")
  
  message("💾 Saving reference to database...")
  save_stage_to_mongo(
    control_SNR_ref, 
    run_id, 
    "mean_snr_reference",
    sample_name = sample_name,
    params = list(
      snr = snr
    )
  )
  
  message("✅ Reference creation complete")
  invisible(control_SNR_ref)
}


process_binning_and_matrix <- function(run_id, msi_data, control_SNR_ref, 
                                       ref_mz_values, snr, tolerance, 
                                       ref_name, ref_source, sample_name) {
  
  message("🎯 Aligning reference spectrum to ", ref_name, "...")
  message("   Using tolerance: ", tolerance, " Da")
  control_MSI_ref <- control_SNR_ref %>%
    peakAlign(ref = ref_mz_values, tolerance = tolerance, units = "mz") %>%
    subsetFeatures() %>%
    process()
  
  n_aligned <- length(mz(control_MSI_ref))
  message("   Aligned to ", n_aligned, " reference m/z values")
  
  message("📦 Binning MSI data...")
  message("   Processing ", nrow(coord(msi_data)), " pixels...")
  msi_data_binned <- bin(msi_data, ref = mz(control_MSI_ref),
                         tolerance = tolerance, units = "mz", BPPARAM = bpparam()) %>%
    process()
  
  message("🔢 Building feature matrix...")
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
  
  message("   Matrix dimensions: ", nrow(full_df), " pixels × ", 
          ncol(full_df) - 3, " features")
  
  message("💾 Saving binned dataframe to database...")
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
  
  message("✅ Binning complete")
  invisible(full_df)
}


process_msi_pipeline <- function(imzml_path, ibd_path, imzml_name,
                                 ref_mz_values, ref_source, ref_name,
                                 snr, tolerance) {
  
  message("\n═══════════════════════════════════════════════════════")
  message("🚀 Starting MSI Processing Pipeline")
  message("═══════════════════════════════════════════════════════")
  message("Sample: ", imzml_name)
  message("SNR: ", snr, " | Tolerance: ", tolerance, " Da")
  message("Reference: ", ref_name, " (", ref_source, ")")
  message("═══════════════════════════════════════════════════════\n")
  
  # --- STEP 0: Check if EXACT combination already exists ---
  message("🔍 Checking for existing processing with same parameters...")
  existing_binned <- query_artifacts(
    sample_name = imzml_name,
    stage_type = "binned_dataframe"
  )
  
  if (nrow(existing_binned) > 0) {
    message("   Found ", nrow(existing_binned), " existing binned dataset(s)")
    
    for (i in seq_len(nrow(existing_binned))) {
      row <- existing_binned[i, ]
      
      snr_match <- !is.null(row$snr) && 
                   isTRUE(all.equal(as.numeric(row$snr), as.numeric(snr)))
      
      tol_match <- !is.null(row$tolerance) && 
                   isTRUE(all.equal(as.numeric(row$tolerance), as.numeric(tolerance)))
      
      ref_match <- !is.null(row$reference_name) && 
                   identical(as.character(row$reference_name), as.character(ref_name))
      
      if (snr_match && tol_match && ref_match) {
        message("   ⚠️  Exact match found!")
        stop("Processing with identical parameters already exists. No action needed.")
      }
    }
    message("   ✓ No exact match found, proceeding...")
  } else {
    message("   No existing binned datasets found")
  }
  
  # Only NOW fetch cached stages for reuse logic
  existing_stages <- get_existing_stages(imzml_name)
  
  # --- STEP 1: Load or import raw data + control_mean ---
  message("\n─────────────────────────────────────────────────────")
  message("STEP 1: Raw Data Import")
  message("─────────────────────────────────────────────────────")
  
  raw_exists <- !is.null(existing_stages) && 
    any(sapply(existing_stages, function(s) s$stage_type == "raw"))
  
  if (raw_exists) {
    message("♻️  Raw data already exists. Reusing...")
    
    run_id <- find_compatible_run(imzml_name)
    message("   Using run_id: ", run_id)
    
    message("📥 Loading raw data from database...")
    raw_artifact <- query_artifacts(sample_name = imzml_name, stage_type = "raw")
    msi_data <- load_artifact_by_id(raw_artifact$gridfs_id[1])
    
    control_mean_artifact <- query_artifacts(sample_name = imzml_name, stage_type = "control_mean")
    
    if (nrow(control_mean_artifact) > 0) {
      message("📥 Loading existing control mean...")
      control_mean <- load_artifact_by_id(control_mean_artifact$gridfs_id[1])
    } else {
      message("⚠️  Control mean not found, recalculating...")
      control_mean <- summarizeFeatures(msi_data, "mean")
      save_stage_to_mongo(control_mean, run_id, "control_mean", sample_name = imzml_name)
    }
    
  } else {
    message("🆕 No existing data found. Starting full import...")
    run_id <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
    message("   Created new run_id: ", run_id)
    
    step1 <- process_import_and_summary(imzml_path, ibd_path, imzml_name, run_id)
    msi_data <- step1$msi_data
    control_mean <- step1$control_mean
  }
  
  # --- STEP 2: Load or create reference ---
  message("\n─────────────────────────────────────────────────────")
  message("STEP 2: Reference Spectrum")
  message("─────────────────────────────────────────────────────")
  
  message("🔍 Checking for existing reference with SNR=", snr, "...")
  existing_refs <- query_artifacts(
    sample_name = imzml_name,
    stage_type = "mean_snr_reference"
  )
  
  ref_exists <- FALSE
  if (nrow(existing_refs) > 0) {
    message("   Found ", nrow(existing_refs), " existing reference(s)")
    
    for (i in seq_len(nrow(existing_refs))) {
      row <- existing_refs[i, ]
      if (!is.null(row$snr) && 
          isTRUE(all.equal(as.numeric(row$snr), as.numeric(snr)))) {
        ref_exists <- TRUE
        existing_ref_id <- row$gridfs_id
        message("   ✓ Match found for SNR=", snr)
        break
      }
    }
  }
  
  if (ref_exists) {
    message("♻️  Reusing existing reference...")
    message("📥 Loading reference from database...")
    control_SNR_ref <- load_artifact_by_id(existing_ref_id)
  } else {
    message("🆕 Creating new reference...")
    control_SNR_ref <- process_reference_creation(
      run_id, control_mean, ref_mz_values, snr, 
      ref_name, ref_source, imzml_name
    )
  }
  
  # --- STEP 3: Always run binning ---
  message("\n─────────────────────────────────────────────────────")
  message("STEP 3: Binning & Feature Extraction")
  message("─────────────────────────────────────────────────────")
  
  binned_df <- process_binning_and_matrix(
    run_id, msi_data, control_SNR_ref, ref_mz_values, snr, tolerance,
    ref_name, ref_source, imzml_name
  )
  
  message("\n═══════════════════════════════════════════════════════")
  message("✅ Processing Complete!")
  message("═══════════════════════════════════════════════════════")
  message("Run ID: ", run_id)
  message("Dataset: ", nrow(binned_df), " pixels × ", ncol(binned_df) - 3, " features")
  message("═══════════════════════════════════════════════════════\n")
  
  invisible(run_id)
}
