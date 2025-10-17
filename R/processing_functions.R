# R/processing_functions.R

process_msi_files <- function(imzml_path, ibd_path, imzml_name, ref_mz_path) {
  message("Copying uploaded files to a temporary directory...")
  temp_dir <- tempfile()
  dir.create(temp_dir)
  
  # use original name to build consistent basename
  base <- tools::file_path_sans_ext(basename(imzml_name))
  temp_imzml <- file.path(temp_dir, paste0(base, ".imzML"))
  temp_ibd   <- file.path(temp_dir, paste0(base, ".ibd"))
  
  file.copy(imzml_path, temp_imzml, overwrite = TRUE)
  file.copy(ibd_path,   temp_ibd,   overwrite = TRUE)
  
  message("Reading MSI data...")
  msi_data <- readImzML(temp_imzml, , memory = FALSE, check = FALSE,
                        mass.range = NULL, resolution = 10, units = c("ppm"),
                        guess.max = 1000L, as = "auto", parse.only=FALSE,
                        verbose = getCardinalVerbose(), chunkopts = list(),
                        BPPARAM = bpparam())
  
  message("Summarizing reference sample...")
  control_mean <- summarizeFeatures(msi_data, "mean")
  ref_mz <- read.csv(ref_mz_path)
  
  control_MSI_ref <- control_mean %>%
    peakPick(SNR = 3) %>%
    peakAlign(ref = as.numeric(ref_mz[, 1]), tolerance = 0.5, units = "mz") %>%
    subsetFeatures() %>%
    process()
  
  message("Binning MSI data...")
  msi_data <- bin(msi_data, ref = mz(control_MSI_ref),
                  tolerance = 0.5, units = "mz", BPPARAM = bpparam()) %>% process()
  
  message("Building feature matrix...")
  msi_matrix <- t(as.matrix(spectra(msi_data)))
  mz_names <- paste0("mz_", mz(msi_data))
  coords <- coord(msi_data)
  run_id <- runNames(msi_data)
  pixel_names <- rep(run_id, nrow(msi_matrix))
  
  full_df <- data.frame(
    runNames = pixel_names,
    x = coords$x,
    y = coords$y,
    msi_matrix
  )
  colnames(full_df) <- c("runNames", "x", "y", mz_names)
  
  full_df
}