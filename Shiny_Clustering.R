# Shiny_Clustering.R
# ------------------------------------------------------------------------------
library(Cardinal)
library(dplyr)

process_msi_files <- function(imzml_path, ibd_path, ref_mz_path = NULL) {
  # Why: preserve pairing; .imzML expects a same-basename .ibd next to it
  temp_dir  <- tempdir()
  imz_dst   <- file.path(temp_dir, basename(imzml_path))
  ibd_dst   <- file.path(temp_dir, basename(ibd_path))
  file.copy(imzml_path, imz_dst, overwrite = TRUE)
  file.copy(ibd_path,   ibd_dst, overwrite = TRUE)
  
  # FIX: removed stray empty argument after imz path
  msi_data <- readImzML(
    imz_dst,
    memory     = FALSE,
    check      = FALSE,
    mass.range = NULL,
    resolution = 10,
    units      = "ppm",
    guess.max  = 1000L,
    as         = "auto",
    parse.only = FALSE,
    verbose    = getCardinalVerbose(),
    chunkopts  = list(),
    BPPARAM    = getCardinalBPPARAM()
  )
  
  # Reference build (robust): use external ref if provided, else fallback
  control_mean <- summarizeFeatures(msi_data, "mean")
  control_ref <- tryCatch({
    if (!is.null(ref_mz_path) && file.exists(ref_mz_path)) {
      ref_mz <- read.csv(ref_mz_path)
      control_mean %>%
        peakPick(SNR = 3) %>%
        peakAlign(ref = as.numeric(ref_mz[[1]]), tolerance = 0.5, units = "mz") %>%
        subsetFeatures() %>%
        process()
    } else {
      control_mean %>% peakPick(SNR = 3) %>% process()
    }
  }, error = function(e) {
    # Why: keep the pipeline running even if alignment fails
    control_mean %>% peakPick(SNR = 3) %>% process()
  })
  
  # Bin to reference m/z grid
  msi_data <- bin(msi_data, ref = mz(control_ref), tolerance = 0.5, units = "mz") %>% process()
  
  # Feature matrix
  m <- t(as.matrix(spectra(msi_data)))
  mz_names <- paste0("mz_", mz(msi_data))  # safe, simple names
  coords   <- coord(msi_data)
  run_id   <- runNames(msi_data)
  
  full_df <- data.frame(
    runNames = rep(run_id, nrow(m)),
    x        = coords$x,
    y        = coords$y,
    m,
    check.names = FALSE
  )
  colnames(full_df) <- c("runNames", "x", "y", mz_names)
  rownames(full_df) <- NULL
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
  d  <- dist(feature_matrix)
  hc <- hclust(d, method = "ward.D2")
  full_df$cluster <- cutree(hc, k = k)
  full_df
}
