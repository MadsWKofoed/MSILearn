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


# Compute distance-weighted neighbor correlation per pixel
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
  
  key_vec <- paste(xy[, 1], xy[, 2], sep = "_")
  index_lookup <- setNames(seq_len(nrow(dat)), key_vec)
  
  neighbor_idx_fun <- function(i, xy, r, index_lookup) {
    x <- xy[i, 1]; y <- xy[i, 2]
    grid <- expand.grid(
      xx = (x - r):(x + r),
      yy = (y - r):(y + r)
    )
    coords <- as.matrix(grid)
    coords <- coords[!(coords[, 1] == x & coords[, 2] == y), , drop = FALSE]
    keys <- paste(coords[, 1], coords[, 2], sep = "_")
    idx <- index_lookup[keys]
    ok <- !is.na(idx)
    if (!any(ok)) return(list(idx = integer(0), step = integer(0)))
    coords_ok <- coords[ok, , drop = FALSE]
    idx_ok <- as.integer(idx[ok])
    dx <- abs(coords_ok[, 1] - x)
    dy <- abs(coords_ok[, 2] - y)
    step <- pmax(dx, dy)
    list(idx = idx_ok, step = step)
  }
  
  pixel_cor_fun <- function(i, xy, intens, r, index_lookup) {
    nei <- neighbor_idx_fun(i, xy, r, index_lookup)
    if (length(nei$idx) == 0) return(NA_real_)
    v <- intens[i, ]
    mats <- intens[nei$idx, , drop = FALSE]
    cors <- apply(mats, 1, function(z) cor(v, z, use = "pairwise.complete.obs"))
    w <- 1 / (nei$step)
    ok <- !is.na(cors) & !is.na(w) & is.finite(w)
    if (!any(ok)) return(NA_real_)
    sum(w[ok] * cors[ok]) / sum(w[ok])
  }
  
  n <- nrow(dat)
  
  if (cores > 1) {
    cl <- parallel::makeCluster(cores)
    on.exit(parallel::stopCluster(cl))
    parallel::clusterExport(
      cl,
      varlist = c("xy", "intens", "index_lookup",
                  "neighbor_idx_fun", "pixel_cor_fun", "r"),
      envir = environment()
    )
    avg_cor <- parallel::parSapply(
      cl, X = seq_len(n), FUN = pixel_cor_fun,
      xy = xy, intens = intens, r = r, index_lookup = index_lookup
    )
  } else {
    avg_cor <- sapply(
      seq_len(n), pixel_cor_fun,
      xy = xy, intens = intens, r = r, index_lookup = index_lookup
    )
  }
  
  avg_cor
}


# MSIClust clustering
run_msiclust <- function(full_df, k = 3,
                         normalize_method = c("none", "tic", "median", "rms"),
                         cor_radius = 1, cor_scale = 25, cor_cores = parallel::detectCores() - 1,
                         minMem = 0.5) {
  
  normalize_method <- match.arg(normalize_method)
  
  mz_cols <- grep("^mz_", names(full_df), value = TRUE)
  if (length(mz_cols) == 0) stop("run_msiclust(): No mz_ columns found.")
  
  # --- 1) Prepare data for correlation (x, y + mz_ only) ---
  cor_input_cols <- c("x", "y", mz_cols)
  cor_data <- full_df[, intersect(cor_input_cols, names(full_df)), drop = FALSE]
  
  # --- 2) Compute per-pixel neighbor correlation ---
  cor_data$avg_corr_neighbors <- compute_neighbor_cor(
    dat = cor_data, x_col = "x", y_col = "y",
    mz_cols = mz_cols, r = cor_radius, cores = cor_cores
  )
  cor_data$inv_cor <- 1 - cor_data$avg_corr_neighbors
  inv_cor_scaled <- cor_data$inv_cor * cor_scale
  
  # --- 3) Remove pixels with no neighbors ---
  has_neighbors <- !is.na(cor_data$avg_corr_neighbors)
  cor_data <- cor_data[has_neighbors, ]
  inv_cor_scaled <- inv_cor_scaled[has_neighbors]
  
  # Also subset full_df to matching pixels
  key_full <- paste(full_df$x, full_df$y)
  key_cor <- paste(cor_data$x, cor_data$y)
  full_df <- full_df[key_full %in% key_cor, ]
  
  # --- 4) Normalize signal columns (pixel-wise) ---
  if (normalize_method != "none") {
    cor_data_norm_xy <- normalize_pixels_wrapper(
      data = cor_data[, c("x", "y", mz_cols), drop = FALSE],
      method = normalize_method,
      signal_prefix = "^mz_",
      spatial_cols = c("x", "y")
    )
  } else {
    cor_data_norm_xy <- cor_data[, c("x", "y", mz_cols), drop = FALSE]
  }
  
  # --- 5) Clustering input: mz_ columns only (no x, y) ---
  X_clust <- as.matrix(cor_data_norm_xy[, mz_cols, drop = FALSE])
  
  # --- 6) Determine per-pixel fuzzifier ---
  fuzz <- determine_fuzz(
    dims = dim(X_clust),
    NClust = k,
    Sds = inv_cor_scaled
  )
  
  # --- 7) Run VSClust algorithm with per-pixel fuzzifiers ---
  msiclust_alg <- vsclust_algorithm(
    X_clust,
    centers = k,
    iterMax = 100,
    m = fuzz$m
  )
  
  # --- 8) Assign results back to full_df ---
  membership_cols <- paste0("membership_", seq_len(k))
  full_df[, membership_cols] <- msiclust_alg$membership
  full_df$max_membership <- matrixStats::rowMaxs(msiclust_alg$membership)
  full_df$raw_cluster <- msiclust_alg$cluster
  
  full_df <- apply_minmem_threshold(full_df, minMem)
  full_df
}