# R/clustering_functions.R

# Pixel-wise normalization (TIC/median/RMS) on signal columns only
normalize_pixels <- function(
  data,
  signal_cols,
  spatial_cols = c("x", "y"),
  method = c("tic", "median", "rms"),
  na.rm = TRUE
) {
  method <- match.arg(method)

  signal_cols <- intersect(signal_cols, names(data))
  if (length(signal_cols) == 0) stop("normalize_pixels(): 'signal_cols' matches no columns in data.")

  spatial_keep <- intersect(spatial_cols, names(data))

  X_signal <- as.matrix(data[, signal_cols, drop = FALSE])
  storage.mode(X_signal) <- "numeric"

  X_spatial <- if (length(spatial_keep) > 0) {
    data[, spatial_keep, drop = FALSE]
  } else {
    data.frame()
  }

  denom <- switch(
    method,
    tic = rowSums(X_signal, na.rm = na.rm),
    median = apply(X_signal, 1, median, na.rm = na.rm),
    rms = sqrt(rowMeans(X_signal^2, na.rm = na.rm))
  )

  denom[denom == 0] <- NA_real_

  X_signal_norm <- sweep(X_signal, 1, denom, "/")

  out <- cbind(X_spatial, as.data.frame(X_signal_norm, check.names = FALSE))
  out
}


# Normalization function
normalize_pixels_wrapper <- function(data, method = c("none", "tic", "median", "rms"),
                                     signal_prefix = "^mz_", spatial_cols = c("x", "y"),
                                     na.rm = TRUE) {
  method <- match.arg(method)
  
  if (method == "none") return(data)
  
  signal_cols <- grep(signal_prefix, names(data), value = TRUE)
  if (length(signal_cols) == 0) stop("No signal columns found (expected columns matching '^mz_').")
  
  normalize_pixels(
    data = data,
    signal_cols = signal_cols,
    spatial_cols = spatial_cols,
    method = method,
    na.rm = na.rm
  )
}

# K-means clustering
run_kmeans <- function(full_df, k = 3, normalize_method = c("none", "tic", "median", "rms")) {
  normalize_method <- match.arg(normalize_method)

  mz_cols <- grep("^mz_", colnames(full_df), value = TRUE)
  if (length(mz_cols) == 0) stop("run_kmeans(): No mz_ columns found.")

  df_norm <- if (normalize_method == "none") {
    full_df
  } else {
    df_norm_part <- normalize_pixels_wrapper(
      data = full_df,
      method = normalize_method,
      signal_prefix = "^mz_",
      spatial_cols = c("x", "y")
    )

    # Merge normalized mz_ back onto original full_df (keeping other cols like runNames)
    full_df_out <- full_df
    full_df_out[, mz_cols] <- df_norm_part[, mz_cols, drop = FALSE]
    full_df_out
  }

  feature_matrix <- as.matrix(df_norm[, mz_cols, drop = FALSE])
  km <- kmeans(feature_matrix, centers = k, nstart = 25)

  df_norm$cluster <- km$cluster
  df_norm
}

# VSClust clustering

run_vsclust <- function(full_df, k = 3, normalize_method = c("none", "tic", "median", "rms"),
                        Sds = 1.3, minMem = 0.5) {

  normalize_method <- match.arg(normalize_method)

  # Drop runNames for clustering input (keep x,y)
  msi_df_clust <- full_df[, !names(full_df) %in% "runNames", drop = FALSE]

  if (normalize_method != "none") {
    mz_cols <- grep("^mz_", names(msi_df_clust), value = TRUE)
    df_norm <- normalize_pixels(
      data = msi_df_clust,
      signal_cols = mz_cols,
      spatial_cols = c("x", "y"),
      method = normalize_method
    )

    # ensure same columns/order as msi_df_clust (x,y + mz_)
    msi_df_clust <- df_norm
  }

  fuzz <- determine_fuzz(
    dims = dim(msi_df_clust),
    NClust = k,
    Sds = Sds
  )

  vsclust_alg <- vsclust_algorithm(
    msi_df_clust,
    centers = k,
    iterMax = 100,
    m = fuzz$m
  )

  membership_cols <- paste0("membership_", seq_len(k))
  full_df[, membership_cols] <- vsclust_alg$membership
  full_df$max_membership <- matrixStats::rowMaxs(vsclust_alg$membership)
  full_df$raw_cluster <- vsclust_alg$cluster

  full_df <- apply_minmem_threshold(full_df, minMem)
  full_df
}

# Helper function to apply minMem threshold (can be called independently)
apply_minmem_threshold <- function(df, minMem = 0.5) {
  # Determine cluster based on membership threshold
  cluster_corrected <- ifelse(
    df$max_membership > minMem,
    as.character(df$raw_cluster),
    "No_cluster"
  )
  
  # Renumber remaining clusters to be consecutive
  valid_clusters <- cluster_corrected[cluster_corrected != "No_cluster"]
  
  if (length(valid_clusters) > 0) {
    existing_nums <- sort(unique(as.numeric(valid_clusters)))
    new_nums <- seq_along(existing_nums)
    cluster_map <- setNames(as.character(new_nums), as.character(existing_nums))
    
    cluster_corrected <- ifelse(
      cluster_corrected == "No_cluster",
      "No_cluster",
      cluster_map[cluster_corrected]
    )
  }
  
  df$cluster <- cluster_corrected
  df
}


