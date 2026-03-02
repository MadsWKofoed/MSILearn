# R/processing_functions.R

load_raw_object_from_mongo <- function(sample_name, workdir,
                                       db_name   = DB_NAME,
                                       mongo_url = MONGO_URL,
                                       resolution = 10) {
  paths <- fetch_raw_pair_from_mongo(
    sample_name = sample_name,
    dest_dir    = workdir,
    db_name     = db_name,
    mongo_url   = mongo_url
  )
  Cardinal::readMSIData(
    paths$imzml,
    memory     = FALSE,
    check      = FALSE,
    resolution = resolution,
    units      = "ppm",
    BPPARAM    = BiocParallel::bpparam()
  )
}

process_msi_pipeline <- function(imzml_path, ibd_path, imzml_name,
                                 ref_mz_values, ref_name,
                                 snr, tolerance, resolution = 10) {

  # Check if exact combination already exists
  existing_binned <- query_legacy_artifacts(
    sample_name = imzml_name,
    stage_type  = "binned_dataframe"
  )

  if (nrow(existing_binned) > 0) {
    for (i in seq_len(nrow(existing_binned))) {
      row <- existing_binned[i, ]
      if (isTRUE(all.equal(as.numeric(row$snr),       as.numeric(snr)))       &&
          isTRUE(all.equal(as.numeric(row$tolerance),  as.numeric(tolerance))) &&
          isTRUE(all.equal(as.numeric(row$resolution), as.numeric(resolution))) &&
          identical(as.character(row$reference_name),  as.character(ref_name))) {
        stop("Processing with identical parameters already exists. No action needed.")
      }
    }
  }

  run_id   <- paste0("run_", format(Sys.time(), "%Y%m%d_%H%M%S"))
  work_dir <- tempfile("msi_run_")
  dir.create(work_dir)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  # Copy uploaded files to throw-away work dir
  base       <- tools::file_path_sans_ext(basename(imzml_name))
  temp_imzml <- file.path(work_dir, paste0(base, ".imzML"))
  temp_ibd   <- file.path(work_dir, paste0(base, ".ibd"))
  file.copy(imzml_path, temp_imzml, overwrite = TRUE)
  file.copy(ibd_path,   temp_ibd,   overwrite = TRUE)

  message("Reading MSI data...")
  msi_data <- Cardinal::readMSIData(
    temp_imzml,
    memory   = FALSE,
    check    = FALSE,
    resolution = resolution,
    units    = "ppm",
    guess.max = 1000L,
    as       = "auto",
    parse.only = FALSE,
    verbose  = Cardinal::getCardinalVerbose(),
    chunkopts = list(),
    BPPARAM  = BiocParallel::bpparam()
  )

  message("Computing mean spectrum...")
  control_mean <- Cardinal::summarizeFeatures(msi_data, "mean")

  message(sprintf("Peak picking (SNR=%.1f) + aligning (tol=%.2f)...", snr, tolerance))
  control_MSI_ref <- control_mean |>
    Cardinal::peakPick(SNR = snr) |>
    Cardinal::peakAlign(ref = ref_mz_values, tolerance = tolerance, units = "mz") |>
    Cardinal::subsetFeatures() |>
    Cardinal::process()

  message(sprintf("Binning full dataset (%d m/z bins)...", nrow(control_MSI_ref)))
  msi_data_binned <- Cardinal::bin(
    msi_data,
    ref       = Cardinal::mz(control_MSI_ref),
    tolerance = tolerance,
    units     = "mz",
    BPPARAM   = BiocParallel::bpparam()
  ) |> Cardinal::process()

  message("Building feature matrix...")
  msi_matrix   <- t(as.matrix(Cardinal::spectra(msi_data_binned)))
  mz_names     <- paste0("mz_", Cardinal::mz(msi_data_binned))
  coords       <- Cardinal::coord(msi_data_binned)
  pixel_names  <- rep(Cardinal::runNames(msi_data_binned), nrow(msi_matrix))

  full_df <- data.frame(
    runNames = pixel_names,
    x        = coords$x,
    y        = coords$y,
    msi_matrix,
    check.names = FALSE
  )
  colnames(full_df) <- c("runNames", "x", "y", mz_names)

  save_stage_to_mongo(
    full_df, run_id, "binned_dataframe",
    sample_name = imzml_name,
    params = list(
      snr            = as.numeric(snr),
      tolerance      = as.numeric(tolerance),
      reference_name = ref_name,
      resolution     = as.numeric(resolution),
      num_features   = length(mz_names),
      num_pixels     = nrow(full_df)
    )
  )

  message(sprintf("✅ Processing complete. Run ID: %s | %d pixels × %d features",
                  run_id, nrow(full_df), length(mz_names)))
  invisible(run_id)
}