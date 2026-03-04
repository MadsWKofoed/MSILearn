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

  # Drop runNames, but keep x/y available in full_df for plotting later
  msi_df_clust <- full_df[, !names(full_df) %in% "runNames", drop = FALSE]

  mz_cols <- grep("^mz_", names(msi_df_clust), value = TRUE)
  if (length(mz_cols) == 0) stop("run_vsclust(): No mz_ columns found.")

  # Normalize only signal columns (pixel-wise), do not include x/y in clustering input
  if (normalize_method != "none") {
    df_norm <- normalize_pixels(
      data = msi_df_clust,
      signal_cols = mz_cols,
      spatial_cols = c("x", "y"),
      method = normalize_method
    )
    # take only signal columns for VSClust
    X_vs <- as.matrix(df_norm[, mz_cols, drop = FALSE])
  } else {
    X_vs <- as.matrix(msi_df_clust[, mz_cols, drop = FALSE])
  }

  fuzz <- determine_fuzz(
    dims = dim(X_vs),
    NClust = k,
    Sds = Sds
  )

  vsclust_alg <- vsclust_algorithm(
    X_vs,
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


# Fast vectorized neighbor correlation
compute_neighbor_cor <- function(dat,
                                 x_col = "x",
                                 y_col = "y",
                                 mz_cols = NULL,
                                 r = 1,
                                 cores = 1) {
  
  if (is.null(mz_cols)) {
    mz_cols <- setdiff(colnames(dat), c(x_col, y_col))
  }
  
  xy <- as.matrix(dat[, c(x_col, y_col)])
  intens <- as.matrix(dat[, mz_cols])
  n <- nrow(dat)
  
  # Build spatial lookup: key -> row index
  key_vec <- paste(xy[, 1], xy[, 2], sep = "_")
  index_lookup <- setNames(seq_len(n), key_vec)
  
  # Generate all offset vectors within radius r (excluding origin)
  offsets <- as.matrix(expand.grid(
    dx = (-r):r,
    dy = (-r):r
  ))
  offsets <- offsets[!(offsets[, 1] == 0 & offsets[, 2] == 0), , drop = FALSE]
  
  # Chebyshev distance for each offset (= weight denominator)
  offset_step <- pmax(abs(offsets[, 1]), abs(offsets[, 2]))
  offset_weight <- 1 / offset_step
  
  weighted_cor_sum <- numeric(n)
  weight_sum <- numeric(n)
  
  for (k in seq_len(nrow(offsets))) {
    nx <- xy[, 1] + offsets[k, 1]
    ny <- xy[, 2] + offsets[k, 2]
    nkey <- paste(nx, ny, sep = "_")
    
    j <- index_lookup[nkey]
    has_neighbor <- !is.na(j)
    
    if (!any(has_neighbor)) next
    
    idx_i <- which(has_neighbor)
    idx_j <- j[has_neighbor]
    
    # Per-pair Pearson correlation, matching cor(v, z, use="pairwise.complete.obs")
    mat_i <- intens[idx_i, , drop = FALSE]
    mat_j <- intens[idx_j, , drop = FALSE]
    
    # Pairwise: exclude positions where either is NA
    valid <- !is.na(mat_i) & !is.na(mat_j)
    
    # Replace invalid with NA so they don't contribute
    mat_i[!valid] <- NA
    mat_j[!valid] <- NA
    
    # Per-row means (only over valid pairs)
    n_valid <- rowSums(valid)
    mean_i <- rowSums(mat_i, na.rm = TRUE) / n_valid
    mean_j <- rowSums(mat_j, na.rm = TRUE) / n_valid
    
    # Center
    mat_i_c <- mat_i - mean_i
    mat_j_c <- mat_j - mean_j
    
    # Dot product and norms
    dot <- rowSums(mat_i_c * mat_j_c, na.rm = TRUE)
    ss_i <- sqrt(rowSums(mat_i_c^2, na.rm = TRUE))
    ss_j <- sqrt(rowSums(mat_j_c^2, na.rm = TRUE))
    
    cors <- dot / (ss_i * ss_j)
    cors[!is.finite(cors) | n_valid < 2] <- NA_real_
    
    w <- offset_weight[k]
    ok <- !is.na(cors)
    
    weighted_cor_sum[idx_i[ok]] <- weighted_cor_sum[idx_i[ok]] + w * cors[ok]
    weight_sum[idx_i[ok]] <- weight_sum[idx_i[ok]] + w
  }
  
  avg_cor <- rep(NA_real_, n)
  has_any <- weight_sum > 0
  avg_cor[has_any] <- weighted_cor_sum[has_any] / weight_sum[has_any]
  
  avg_cor
}


# MSIClust clustering
run_msiclust <- function(full_df, k = 3,
                         normalize_method = c("none", "tic", "median", "rms"),
                         cor_radius = 1, cor_scale = 25, cor_cores = parallel::detectCores() - 1,
                         minMem = 0.5) {
  
  normalize_method <- match.arg(normalize_method)
  t0 <- Sys.time()
  
  mz_cols <- grep("^mz_", names(full_df), value = TRUE)
  if (length(mz_cols) == 0) stop("run_msiclust(): No mz_ columns found.")
  
  message(sprintf("[MSIClust] Start: %d pixels, %d features, k=%d, norm=%s, r=%d, cores=%d",
                  nrow(full_df), length(mz_cols), k, normalize_method, cor_radius, cor_cores))
  
  cor_input_cols <- c("x", "y", mz_cols)
  cor_data <- full_df[, intersect(cor_input_cols, names(full_df)), drop = FALSE]
  
  message("[MSIClust] Computing neighbor correlations...")
  t1 <- Sys.time()
  
  cor_data$avg_corr_neighbors <- compute_neighbor_cor(
    dat = cor_data, x_col = "x", y_col = "y",
    mz_cols = mz_cols, r = cor_radius, cores = cor_cores
  )
  cor_data$avg_corr_neighbors[is.nan(cor_data$avg_corr_neighbors)] <- NA_real_

  t2 <- Sys.time()
  message(sprintf("[MSIClust] Neighbor correlations done (%.1f sec)", as.numeric(t2 - t1, units = "secs")))
  
  cor_data$inv_cor <- 1 - cor_data$avg_corr_neighbors
  inv_cor_scaled <- cor_data$inv_cor * cor_scale
  
  # Remove pixels with no neighbors
  has_neighbors <- !is.na(cor_data$avg_corr_neighbors)
  n_removed <- sum(!has_neighbors)
  cor_data <- cor_data[has_neighbors, ]
  inv_cor_scaled <- inv_cor_scaled[has_neighbors]
  names(inv_cor_scaled) <- NULL
  
  # Filter full_df to match using the SAME logical index — avoids key collision
  full_df  <- full_df[has_neighbors, , drop = FALSE]
  row.names(full_df) <- NULL
  
  if (n_removed > 0) message(sprintf("[MSIClust] Removed %d pixels with no neighbors", n_removed))
  
  # Normalize
  if (normalize_method != "none") {
    message(sprintf("[MSIClust] Normalizing (%s)...", normalize_method))
    t3 <- Sys.time()
    
    cor_data_norm_xy <- normalize_pixels_wrapper(
      data = cor_data[, c("x", "y", mz_cols), drop = FALSE],
      method = normalize_method,
      signal_prefix = "^mz_",
      spatial_cols = c("x", "y")
    )
    
    message(sprintf("[MSIClust] Normalization done (%.1f sec)", as.numeric(Sys.time() - t3, units = "secs")))
  } else {
    cor_data_norm_xy <- cor_data[, c("x", "y", mz_cols), drop = FALSE]
    row.names(cor_data_norm_xy) <- NULL    # ← also reset here
  }
  
  X_clust <- as.matrix(cor_data_norm_xy[, mz_cols, drop = FALSE])
  
  message("[MSIClust] Computing fuzzifiers...")
  t4 <- Sys.time()
  
  fuzz <- determine_fuzz(
    dims = dim(X_clust),
    NClust = k,
    Sds = inv_cor_scaled
  )
  
  message(sprintf("[MSIClust] Fuzzifiers done (%.1f sec), m range: [%.2f, %.2f]",
                  as.numeric(Sys.time() - t4, units = "secs"),
                  min(fuzz$m), max(fuzz$m)))
  
  message("[MSIClust] Running vsclust_algorithm...")
  t5 <- Sys.time()
  
  msiclust_alg <- vsclust_algorithm(
    X_clust,
    centers = k,
    iterMax = 100,
    m = fuzz$m
  )
  
  message(sprintf("[MSIClust] vsclust_algorithm done (%.1f sec)", as.numeric(Sys.time() - t5, units = "secs")))
  
  message(sprintf("[MSIClust] X_clust rows: %d, full_df rows: %d, membership rows: %d",
                  nrow(X_clust), nrow(full_df), nrow(msiclust_alg$membership)))
  
  # Force contiguous row names immediately before assignment
 row.names(full_df) <- NULL
  
  membership_cols <- paste0("membership_", seq_len(k))
  
  message(sprintf("[MSIClust] Assigning membership: full_df rows=%d, membership dim=%s",
                  nrow(full_df), paste(dim(msiclust_alg$membership), collapse="x")))
  
  full_df[, membership_cols] <- msiclust_alg$membership
  
  message(sprintf("[MSIClust] Assigning max_membership and raw_cluster"))
  full_df$max_membership <- matrixStats::rowMaxs(msiclust_alg$membership)
  full_df$raw_cluster    <- msiclust_alg$cluster
  
  message(sprintf("[MSIClust] Calling apply_minmem_threshold, full_df rows=%d", nrow(full_df)))
  full_df <- apply_minmem_threshold(full_df, minMem)
  
  message(sprintf("[MSIClust] apply_minmem_threshold done, full_df rows=%d", nrow(full_df)))
  
  n_no <- sum(full_df$cluster == "No_cluster")
  message(sprintf("[MSIClust] Complete: %.1f sec total, %d/%d pixels assigned (minMem=%.2f)",
                  as.numeric(Sys.time() - t0, units = "secs"),
                  nrow(full_df) - n_no, nrow(full_df), minMem))
  
  full_df
}