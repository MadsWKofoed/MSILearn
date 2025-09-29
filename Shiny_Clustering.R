# file: Shiny_Clustering.R
# Shiny_Clustering.R
library(Cardinal)
library(dplyr)

process_msi_files <- function(imzml_path, ibd_path, ref_mz_path) {
  message("Staging uploaded files…")
  temp_dir <- file.path(tempdir(), "msi_upload")
  dir.create(temp_dir, showWarnings = FALSE, recursive = TRUE)
  
  # Use SAME basename so Cardinal finds the .ibd next to .imzML
  temp_imzml <- file.path(temp_dir, "data.imzML")
  temp_ibd   <- file.path(temp_dir, "data.ibd")
  
  ok1 <- file.copy(imzml_path, temp_imzml, overwrite = TRUE)
  ok2 <- file.copy(ibd_path,   temp_ibd,   overwrite = TRUE)
  if (!ok1 || !file.exists(temp_imzml)) stop("Failed to stage imzML at: ", temp_imzml)  # why: readImzML requires both files
  if (!ok2 || !file.exists(temp_ibd))   stop("Failed to stage IBD at: ", temp_ibd)
  
  if (!is.character(ref_mz_path) || !nzchar(ref_mz_path) || !file.exists(ref_mz_path)) {
    stop("Reference m/z CSV not found at: ", ref_mz_path)  # why: peakAlign needs a reference axis
  }
  
  message("Reading MSI data…")
  msi_data <- readImzML(
    file = basename(temp_imzml),  # "data.imzML"
    path = temp_dir,              # must be a valid directory; not NULL
    memory = FALSE, check = FALSE,
    mass.range = NULL, resolution = 10, units = c("ppm"),
    guess.max = 1000L, as = "auto", parse.only = FALSE,
    verbose = getCardinalVerbose(), chunkopts = list(),
    BPPARAM = getCardinalBPPARAM()
  )
  
  message("Summarizing reference sample…")
  control_mean <- summarizeFeatures(msi_data, "mean")
  ref_mz <- read.csv(ref_mz_path)
  
  control_MSI_ref <- control_mean %>%
    peakPick(SNR = 3) %>%
    peakAlign(ref = as.numeric(ref_mz[, 1]), tolerance = 0.5, units = "mz") %>%
    subsetFeatures() %>%
    process()
  
  message("Binning MSI data…")
  msi_data <- bin(msi_data, ref = mz(control_MSI_ref),
                  tolerance = 0.5, units = "mz") %>%
    process()
  
  message("Building feature matrix…")
  msi_matrix <- t(as.matrix(spectra(msi_data)))
  mz_names <- paste0("mz_", mz(msi_data))
  coords <- coord(msi_data)
  run_id <- runNames(msi_data)
  pixel_names <- rep(run_id, nrow(msi_matrix))
  
  full_df <- data.frame(
    runNames = pixel_names,
    x = coords$x,
    y = coords$y,
    msi_matrix,
    check.names = FALSE,
    stringsAsFactors = FALSE
  )
  colnames(full_df) <- c("runNames", "x", "y", mz_names)
  
  full_df
}

# K-means clustering
run_kmeans <- function(full_df, k = 3) {
  feature_matrix <- as.matrix(full_df[, grep("^mz_", colnames(full_df))])
  km <- kmeans(feature_matrix, centers = k)
  full_df$cluster <- km$cluster
  full_df
}

# Hierarchical clustering
run_hclust <- function(full_df, k = 3) {
  feature_matrix <- as.matrix(full_df[, grep("^mz_", colnames(full_df))])
  d <- dist(feature_matrix)
  hc <- hclust(d, method = "ward.D2")
  clusters <- cutree(hc, k = k)
  full_df$cluster <- clusters
  full_df
}
