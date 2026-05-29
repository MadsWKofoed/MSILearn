# R/training_functions.R
#
# All model training logic.
#
# Design rules enforced here:
#   1. Training ALWAYS starts from a dataset_id – never from ad-hoc queries.
#   2. load_dataset_for_training() (in mongo_functions.R) is the only function
#      allowed to assemble the feature matrix and labels.
#   3. Hyperparameters are recorded explicitly; random seeds are fixed.
#   4. Every training run is persisted via save_model_run().
#   5. Class weights are computed deterministically from training labels.

if (!exists("standardize_feature_matrix", mode = "function")) {
  source("R/feature_standardization_functions.R")
}

# ---------------------------------------------------------------------------
# Compute class weights from a factor of training labels.
# Returns a named numeric vector.
# ---------------------------------------------------------------------------
compute_class_weights <- function(labels) {
  freq    <- table(labels)
  weights <- 1 / as.numeric(freq)
  weights <- weights / sum(weights)
  setNames(weights, names(freq))
}

# Map class weights to per-observation weight vector.
observation_weights_from_labels <- function(labels, class_weights) {
  class_weights[as.character(labels)]
}

new_class_label_encoder <- function(original_levels, safe_levels = NULL) {
  original_levels <- as.character(original_levels %||% character(0))
  original_levels <- original_levels[!is.na(original_levels)]

  if (is.null(safe_levels)) {
    safe_levels <- make.names(original_levels, unique = TRUE)
  }

  safe_levels <- as.character(safe_levels %||% character(0))

  if (length(original_levels) != length(safe_levels)) {
    stop("Class label encoder requires matching original/safe level lengths.")
  }

  map_df <- data.frame(
    original_label = original_levels,
    safe_label = safe_levels,
    stringsAsFactors = FALSE
  )

  list(
    original_levels = original_levels,
    safe_levels = safe_levels,
    original_to_safe = stats::setNames(safe_levels, original_levels),
    safe_to_original = stats::setNames(original_levels, safe_levels),
    map_df = map_df
  )
}

make_class_label_encoder <- function(labels) {
  original_levels <- if (is.factor(labels)) {
    levels(labels)
  } else {
    unique(as.character(labels))
  }

  new_class_label_encoder(original_levels)
}

identity_class_label_encoder <- function(levels) {
  new_class_label_encoder(levels, safe_levels = levels)
}

normalize_class_label_encoder <- function(raw_map = NULL, fallback_levels = NULL) {
  obj <- raw_map

  repeat {
    if (is.null(obj) || is.data.frame(obj) || !is.list(obj) || length(obj) != 1L) break
    obj <- obj[[1]]
  }

  if (is.list(obj) && !is.data.frame(obj)) {
    if (!is.null(obj$map_df)) {
      obj <- obj$map_df
    } else if (!is.null(obj$original_label) && !is.null(obj$safe_label)) {
      obj <- data.frame(
        original_label = as.character(obj$original_label),
        safe_label = as.character(obj$safe_label),
        stringsAsFactors = FALSE
      )
    } else if (!is.null(obj$original_levels) && !is.null(obj$safe_levels)) {
      obj <- data.frame(
        original_label = as.character(obj$original_levels),
        safe_label = as.character(obj$safe_levels),
        stringsAsFactors = FALSE
      )
    }
  }

  if (is.data.frame(obj) && all(c("original_label", "safe_label") %in% names(obj))) {
    return(new_class_label_encoder(
      original_levels = as.character(obj$original_label),
      safe_levels = as.character(obj$safe_label)
    ))
  }

  if (!is.null(fallback_levels)) {
    return(identity_class_label_encoder(as.character(fallback_levels)))
  }

  NULL
}

sanitize_class_labels <- function(labels, label_encoder) {
  if (is.null(label_encoder)) return(as.factor(labels))

  label_values <- as.character(labels)
  safe_values <- unname(label_encoder$original_to_safe[label_values])

  missing_mask <- !is.na(label_values) & is.na(safe_values)
  if (any(missing_mask)) {
    stop(
      "Missing sanitized class mapping for labels: ",
      paste(unique(label_values[missing_mask]), collapse = ", ")
    )
  }

  factor(safe_values, levels = label_encoder$safe_levels)
}

restore_original_class_labels <- function(labels, label_encoder, as_factor = TRUE) {
  if (is.null(label_encoder)) {
    if (isTRUE(as_factor)) return(as.factor(labels))
    return(as.character(labels))
  }

  label_values <- as.character(labels)
  original_values <- unname(label_encoder$safe_to_original[label_values])
  passthrough_mask <- !is.na(label_values) & (is.na(original_values) | !nzchar(original_values))
  original_values[passthrough_mask] <- label_values[passthrough_mask]

  if (!isTRUE(as_factor)) {
    return(original_values)
  }

  factor(original_values, levels = label_encoder$original_levels)
}

restore_probability_column_names <- function(prob_df, label_encoder) {
  if (is.null(label_encoder) || is.null(prob_df) || !is.data.frame(prob_df)) {
    return(prob_df)
  }

  mapped_cols <- unname(label_encoder$safe_to_original[colnames(prob_df)])
  keep_original <- is.na(mapped_cols) | !nzchar(mapped_cols)
  mapped_cols[keep_original] <- colnames(prob_df)[keep_original]
  colnames(prob_df) <- mapped_cols
  prob_df
}

build_byclass_metrics_table <- function(byclass_obj, label_encoder = NULL) {
  if (is.null(byclass_obj)) return(NULL)

  if (is.null(dim(byclass_obj))) {
    byclass_df <- as.data.frame(t(as.matrix(byclass_obj)), stringsAsFactors = FALSE)
    byclass_df$Class <- "Overall"
  } else {
    byclass_df <- as.data.frame(byclass_obj, stringsAsFactors = FALSE)
    byclass_df$Class <- rownames(byclass_df)
  }

  byclass_df$Class <- sub("^Class: ", "", as.character(byclass_df$Class))
  byclass_df$Class <- restore_original_class_labels(
    byclass_df$Class,
    label_encoder = label_encoder,
    as_factor = FALSE
  )

  byclass_df <- byclass_df[, c("Class", setdiff(names(byclass_df), "Class")), drop = FALSE]
  rownames(byclass_df) <- NULL
  byclass_df
}


# ---------------------------------------------------------------------------
# Pixel-wise normalisation of feature matrices used for training/prediction.
# Same methods as clustering: none / tic / median / rms
# Rows = pixels, columns = mz features
# ---------------------------------------------------------------------------
normalize_feature_matrix <- function(X, method = c("none", "tic", "median", "rms"),
                                     na.rm = TRUE) {
  method <- match.arg(method)

  X <- as.matrix(X)
  storage.mode(X) <- "numeric"

  if (method == "none") return(X)

  denom <- switch(
    method,
    tic    = rowSums(X, na.rm = na.rm),
    median = apply(X, 1, median, na.rm = na.rm),
    rms    = sqrt(rowMeans(X^2, na.rm = na.rm))
  )

  denom[!is.finite(denom) | denom == 0] <- NA_real_

  X_norm <- sweep(X, 1, denom, "/")
  X_norm[!is.finite(X_norm)] <- 0

  colnames(X_norm) <- colnames(X)
  rownames(X_norm) <- rownames(X)
  X_norm
}


# ---------------------------------------------------------------------------
# Moran correlogram diagnostics on training pixels
# ---------------------------------------------------------------------------

.compute_distance_matrix <- function(coords) {
  as.matrix(stats::dist(coords))
}

build_pair_sampling_lag_breaks <- function(max_dist) {
  max_dist <- suppressWarnings(as.numeric(max_dist))
  if (!is.finite(max_dist) || max_dist <= 1) {
    return(c(0, 1))
  }

  br <- c(
    seq(0, min(20, max_dist), by = 1),
    seq(20, min(50, max_dist), by = 2),
    seq(50, min(100, max_dist), by = 5)
  )

  if (max_dist > 100) {
    br <- c(br, seq(100, max_dist, by = 20))
  }

  br <- sort(unique(as.numeric(br)))
  br <- br[is.finite(br)]

  if (length(br) < 2L) {
    br <- c(0, max_dist)
  }

  if (tail(br, 1) < max_dist) {
    br <- c(br, max_dist)
  }

  sort(unique(br))
}

build_sampled_pair_structure <- function(
    coords,
    lag_breaks = NULL,
    max_dist = NULL,
    max_pairs_per_bin = 500L,
    min_pairs = 30L,
    seed = 1234L
) {
  coords <- as.matrix(coords)
  storage.mode(coords) <- "numeric"

  empty_res <- list(
    pair_i = integer(0),
    pair_j = integer(0),
    bins = data.frame(
      bin_id = integer(0),
      distance_min = numeric(0),
      distance_max = numeric(0),
      distance_mid = numeric(0),
      n_pairs = integer(0),
      stringsAsFactors = FALSE
    )
  )

  if (nrow(coords) < 10L) return(empty_res)

  dmat <- .compute_distance_matrix(coords)
  upper_idx <- which(upper.tri(dmat, diag = FALSE), arr.ind = TRUE)

  dists <- as.numeric(dmat[upper.tri(dmat, diag = FALSE)])

  if (is.null(max_dist)) {
    max_dist <- stats::quantile(dists, probs = 0.9, na.rm = TRUE, names = FALSE)
  }

  if (!is.finite(max_dist) || max_dist <= 0) return(empty_res)

  if (is.null(lag_breaks)) {
    lag_breaks <- build_pair_sampling_lag_breaks(max_dist = max_dist)
  }

  lag_breaks <- sort(unique(as.numeric(lag_breaks)))
  lag_breaks <- lag_breaks[is.finite(lag_breaks)]

  if (length(lag_breaks) < 2L) return(empty_res)

  if (tail(lag_breaks, 1) < max_dist) {
    lag_breaks <- c(lag_breaks, max_dist)
  }

  set.seed(as.integer(seed))

  pair_i_all <- integer(0)
  pair_j_all <- integer(0)
  bins_out <- vector("list", length(lag_breaks) - 1L)

  for (b in seq_len(length(lag_breaks) - 1L)) {
    lo <- lag_breaks[b]
    hi <- lag_breaks[b + 1L]

    if (b < (length(lag_breaks) - 1L)) {
      pick <- which(dists >= lo & dists < hi)
    } else {
      pick <- which(dists >= lo & dists <= hi)
    }

    if (length(pick) < min_pairs) {
      bins_out[[b]] <- NULL
      next
    }

    if (length(pick) > max_pairs_per_bin) {
      pick <- sample(pick, size = max_pairs_per_bin, replace = FALSE)
    }

    pair_i_all <- c(pair_i_all, upper_idx[pick, 1])
    pair_j_all <- c(pair_j_all, upper_idx[pick, 2])

    bins_out[[b]] <- data.frame(
      bin_id = b,
      distance_min = lo,
      distance_max = hi,
      distance_mid = (lo + hi) / 2,
      n_pairs = length(pick),
      stringsAsFactors = FALSE
    )
  }

  bins_df <- dplyr::bind_rows(bins_out)

  if (nrow(bins_df) == 0L) return(empty_res)

  list(
    pair_i = pair_i_all,
    pair_j = pair_j_all,
    bins = bins_df
  )
}

compute_sampled_moran_from_pair_structure <- function(
    values,
    pair_structure
) {
  empty_df <- data.frame(
    distance_mid = numeric(0),
    distance_min = numeric(0),
    distance_max = numeric(0),
    moran_i = numeric(0),
    n_pairs = integer(0)
  )

  if (is.null(pair_structure) || length(pair_structure$pair_i) == 0L) {
    return(empty_df)
  }

  values <- as.numeric(values)
  ok <- is.finite(values)
  if (!all(ok)) return(empty_df)

  z <- values - mean(values)
  denom <- sum(z^2)
  if (!is.finite(denom) || denom <= 0) return(empty_df)

  pair_i <- pair_structure$pair_i
  pair_j <- pair_structure$pair_j
  bins_df <- pair_structure$bins

  zij <- z[pair_i] * z[pair_j]

  out <- vector("list", nrow(bins_df))
  offset <- 0L

  for (k in seq_len(nrow(bins_df))) {
    m <- bins_df$n_pairs[k]
    idx <- seq.int(from = offset + 1L, length.out = m)
    offset <- offset + m

    moran_i <- length(z) * mean(zij[idx], na.rm = TRUE) / denom

    out[[k]] <- data.frame(
      distance_mid = bins_df$distance_mid[k],
      distance_min = bins_df$distance_min[k],
      distance_max = bins_df$distance_max[k],
      moran_i = moran_i,
      n_pairs = m
    )
  }

  dplyr::bind_rows(out)
}

estimate_local_decay_distance <- function(
    corr_df_one_feature,
    threshold_fraction = 0.05
) {
  if (!is.data.frame(corr_df_one_feature) || nrow(corr_df_one_feature) == 0L) {
    return(NA_real_)
  }

  corr_df_one_feature <- corr_df_one_feature |>
    dplyr::arrange(distance_mid)

  moran_vals <- as.numeric(corr_df_one_feature$moran_i)
  dist_vals  <- as.numeric(corr_df_one_feature$distance_mid)

  ok <- is.finite(moran_vals) & is.finite(dist_vals)
  moran_vals <- moran_vals[ok]
  dist_vals  <- dist_vals[ok]

  if (length(moran_vals) == 0L) return(NA_real_)

  peak_i <- suppressWarnings(max(moran_vals, na.rm = TRUE))
  if (!is.finite(peak_i) || peak_i <= 0) return(NA_real_)

  threshold_value <- threshold_fraction * peak_i

  idx_below_frac <- which(moran_vals <= threshold_value)
  idx_below_zero <- which(moran_vals <= 0)

  idx_first <- c(
    if (length(idx_below_frac) > 0) min(idx_below_frac) else NA_integer_,
    if (length(idx_below_zero) > 0) min(idx_below_zero) else NA_integer_
  )

  idx_first <- idx_first[is.finite(idx_first)]

  if (length(idx_first) == 0L) {
    return(NA_real_)
  }

  dist_vals[min(idx_first)]
}

compute_global_moran_for_features <- function(
    X,
    meta,
    sample_size = 2000L,
    seed = 1234L,
    k = 8L,
    workers = 20L
) {
  req_cols <- c("x", "y")
  empty_df <- data.frame(
    feature = character(0),
    moran_i = numeric(0),
    variance = numeric(0),
    stringsAsFactors = FALSE
  )

  if (is.null(meta) || !all(req_cols %in% names(meta))) return(empty_df)

  X <- as.matrix(X)
  storage.mode(X) <- "numeric"
  meta <- as.data.frame(meta)

  ok <- stats::complete.cases(meta[, req_cols, drop = FALSE]) &
    rowSums(is.finite(X)) == ncol(X)

  X <- X[ok, , drop = FALSE]
  meta <- meta[ok, , drop = FALSE]

  if (nrow(X) < 20L || ncol(X) < 2L) return(empty_df)

  set.seed(as.integer(seed))
  if (nrow(X) > sample_size) {
    keep <- sort(sample.int(nrow(X), sample_size))
    X <- X[keep, , drop = FALSE]
    meta <- meta[keep, , drop = FALSE]
  }

  coords <- as.matrix(meta[, req_cols, drop = FALSE])

  dmat <- as.matrix(stats::dist(coords))
  diag(dmat) <- Inf

  k_eff <- min(as.integer(k), nrow(coords) - 1L)
  if (k_eff < 1L) return(empty_df)

  nn_idx <- t(apply(dmat, 1, function(v) order(v)[seq_len(k_eff)]))

  feature_indices <- seq_len(ncol(X))

  if (as.integer(workers) > 1L) {
    cl <- parallel::makePSOCKcluster(as.integer(workers))
    on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)

    parallel::clusterExport(
      cl,
      varlist = c("X", "nn_idx"),
      envir = environment()
    )
    parallel::clusterEvalQ(cl, {
      NULL
    })

    out <- parallel::parLapply(cl, feature_indices, function(j) {
      vals <- as.numeric(X[, j])
      vvar <- stats::var(vals, na.rm = TRUE)
      if (!is.finite(vvar) || vvar <= 0) return(NULL)

      z <- vals - mean(vals, na.rm = TRUE)
      denom <- sum(z^2, na.rm = TRUE)
      if (!is.finite(denom) || denom <= 0) return(NULL)

      neigh_mean <- rowMeans(matrix(vals[nn_idx], nrow = nrow(nn_idx)), na.rm = TRUE)
      num <- sum(z * (neigh_mean - mean(vals, na.rm = TRUE)), na.rm = TRUE)
      moran_i <- num / denom

      data.frame(
        feature = colnames(X)[j],
        moran_i = moran_i,
        variance = vvar,
        stringsAsFactors = FALSE
      )
    })
  } else {
    out <- lapply(feature_indices, function(j) {
      vals <- as.numeric(X[, j])
      vvar <- stats::var(vals, na.rm = TRUE)
      if (!is.finite(vvar) || vvar <= 0) return(NULL)

      z <- vals - mean(vals, na.rm = TRUE)
      denom <- sum(z^2, na.rm = TRUE)
      if (!is.finite(denom) || denom <= 0) return(NULL)

      neigh_mean <- rowMeans(matrix(vals[nn_idx], nrow = nrow(nn_idx)), na.rm = TRUE)
      num <- sum(z * (neigh_mean - mean(vals, na.rm = TRUE)), na.rm = TRUE)
      moran_i <- num / denom

      data.frame(
        feature = colnames(X)[j],
        moran_i = moran_i,
        variance = vvar,
        stringsAsFactors = FALSE
      )
    })
  }

  out <- Filter(Negate(is.null), out)
  if (length(out) == 0L) return(empty_df)

  dplyr::bind_rows(out) |>
    dplyr::arrange(dplyr::desc(moran_i))
}


compute_feature_moran_diagnostics <- function(
    X,
    meta,
    max_points = 1000L,
    lag_breaks = NULL,
    max_dist = NULL,
    max_pairs_per_bin = 500L,
    local_decay_threshold = 0.10,
    seed = 1234L,
    workers = 20L
) {
  req_cols <- c("x", "y")
  empty_res <- list(
    feature_moran_summary = data.frame(
      feature = character(0),
      moran_i = numeric(0),
      variance = numeric(0),
      stringsAsFactors = FALSE
    ),
    feature_correlogram = data.frame(
      feature = character(0),
      distance_mid = numeric(0),
      distance_min = numeric(0),
      distance_max = numeric(0),
      moran_i = numeric(0),
      n_pairs = integer(0),
      stringsAsFactors = FALSE
    ),
    feature_range_summary = data.frame(
      feature = character(0),
      range_estimate = numeric(0),
      stringsAsFactors = FALSE
    ),
    recommended_buffer_radius = NA_real_
  )

  if (is.null(meta) || !all(req_cols %in% names(meta))) return(empty_res)

  X <- as.matrix(X)
  storage.mode(X) <- "numeric"
  meta <- as.data.frame(meta)

  ok <- stats::complete.cases(meta[, req_cols, drop = FALSE]) &
    rowSums(is.finite(X)) == ncol(X)

  X <- X[ok, , drop = FALSE]
  meta <- meta[ok, , drop = FALSE]

  if (nrow(X) < 20L || ncol(X) < 2L) return(empty_res)

  keep_cols <- apply(X, 2, function(v) stats::sd(v, na.rm = TRUE) > 0)
  X <- X[, keep_cols, drop = FALSE]

  if (ncol(X) < 2L) return(empty_res)

  moran_tbl <- compute_global_moran_for_features(
    X = X,
    meta = meta,
    sample_size = min(max_points, nrow(X)),
    seed = seed,
    k = 8L
  )

  if (nrow(moran_tbl) == 0L) return(empty_res)

  set.seed(as.integer(seed))
  if (nrow(X) > max_points) {
    keep <- sort(sample.int(nrow(X), size = max_points))
    X_corr <- X[keep, , drop = FALSE]
    meta_corr <- meta[keep, , drop = FALSE]
  } else {
    X_corr <- X
    meta_corr <- meta
  }

  coords <- as.matrix(meta_corr[, req_cols, drop = FALSE])

  pair_structure <- build_sampled_pair_structure(
    coords = coords,
    lag_breaks = lag_breaks,
    max_dist = max_dist,
    max_pairs_per_bin = max_pairs_per_bin,
    min_pairs = 30L,
    seed = seed
  )

  if (length(pair_structure$pair_i) == 0L || nrow(pair_structure$bins) == 0L) {
    return(empty_res)
  }

  candidate_tbl <- moran_tbl |>
    dplyr::filter(
      is.finite(moran_i),
      is.finite(variance),
      variance > 0
    )

  feature_names <- candidate_tbl$feature

  if (as.integer(workers) > 1L) {
    cl <- parallel::makePSOCKcluster(as.integer(workers))
    on.exit(try(parallel::stopCluster(cl), silent = TRUE), add = TRUE)

    parallel::clusterExport(
      cl,
      varlist = c(
        "X_corr",
        "pair_structure",
        "compute_sampled_moran_from_pair_structure"
      ),
      envir = environment()
    )

    corr_list <- parallel::parLapply(cl, feature_names, function(feat) {
      j <- match(feat, colnames(X_corr))
      vals <- X_corr[, j]

      corr <- compute_sampled_moran_from_pair_structure(
        values = vals,
        pair_structure = pair_structure
      )
      if (nrow(corr) == 0L) return(NULL)

      corr$feature <- feat
      corr
    })
  } else {
    corr_list <- lapply(feature_names, function(feat) {
      j <- match(feat, colnames(X_corr))
      vals <- X_corr[, j]

      corr <- compute_sampled_moran_from_pair_structure(
        values = vals,
        pair_structure = pair_structure
      )
      if (nrow(corr) == 0L) return(NULL)

      corr$feature <- feat
      corr
    })
  }

  corr_list <- Filter(Negate(is.null), corr_list)

  corr_df <- if (length(corr_list) > 0L) {
    dplyr::bind_rows(corr_list)
  } else {
    empty_res$feature_correlogram
  }

    range_tbl <- if (nrow(corr_df) > 0L) {
      corr_df |>
        dplyr::group_by(feature) |>
        dplyr::group_modify(~{
          data.frame(
            range_estimate = estimate_local_decay_distance(
              corr_df_one_feature = .x,
              threshold_fraction = local_decay_threshold
            )
          )
        }) |>
        dplyr::ungroup() |>
        dplyr::filter(is.finite(range_estimate))
    } else {
      empty_res$feature_range_summary
    }

    if (nrow(range_tbl) > 0L) {
      upper_trim_cutoff <- stats::quantile(
        range_tbl$range_estimate,
        probs = 0.85,
        na.rm = TRUE,
        names = FALSE
      )

      range_tbl_for_buffer <- range_tbl |>
        dplyr::filter(range_estimate <= upper_trim_cutoff)
    } else {
      range_tbl_for_buffer <- range_tbl
    }

    recommended_buffer_radius <- if (nrow(range_tbl_for_buffer) > 0L) {
      stats::median(range_tbl_for_buffer$range_estimate, na.rm = TRUE)
    } else {
      NA_real_
    }

    recommended_block_size <- if (is.finite(recommended_buffer_radius) && recommended_buffer_radius > 0) {
      max(4, ceiling(recommended_buffer_radius))
    } else {
      NA_real_
    }

    list(
      feature_moran_summary = moran_tbl,
      feature_correlogram = corr_df,
      feature_range_summary = range_tbl,
      recommended_buffer_radius = as.numeric(recommended_buffer_radius),
      recommended_block_size = as.numeric(recommended_block_size)
    )
}


# ---------------------------------------------------------------------------
# CV helper builders for split-aware training
# ---------------------------------------------------------------------------
make_random_cv_indices <- function(n, cv_folds, seed = 1234L) {
  set.seed(as.integer(seed))
  folds <- caret::createFolds(seq_len(n), k = cv_folds, list = TRUE, returnTrain = FALSE)
  indexOut <- folds
  index <- lapply(indexOut, function(te) setdiff(seq_len(n), te))
  list(index = index, indexOut = indexOut)
}

make_loso_cv_indices <- function(train_meta, cv_folds = 10L, seed = 1234L) {
  if (is.null(train_meta) || !is.data.frame(train_meta) || !("sample_id" %in% names(train_meta))) {
    stop("Grouped sample-out CV requires train_meta with column: sample_id")
  }

  samp_tbl <- train_meta |>
    dplyr::count(sample_id, name = "n_pixels")

  n_samples <- nrow(samp_tbl)
  if (n_samples < 2L) {
    stop("Need at least 2 training samples for grouped sample-out CV.")
  }

  n_groups <- min(as.integer(cv_folds), n_samples)
  if (!is.finite(n_groups) || n_groups < 2L) {
    stop("Grouped sample-out CV requires at least 2 folds.")
  }

  set.seed(as.integer(seed))
  samp_tbl <- samp_tbl |>
    dplyr::mutate(rand = stats::runif(dplyr::n())) |>
    dplyr::arrange(dplyr::desc(n_pixels), rand)

  group_load <- rep(0, n_groups)
  group_assign <- integer(nrow(samp_tbl))

  for (i in seq_len(nrow(samp_tbl))) {
    g <- which.min(group_load)
    group_assign[i] <- g
    group_load[g] <- group_load[g] + samp_tbl$n_pixels[i]
  }

  samp_tbl$group_id <- group_assign

  group_map <- setNames(samp_tbl$group_id, as.character(samp_tbl$sample_id))
  pixel_group <- unname(group_map[as.character(train_meta$sample_id)])

  index <- vector("list", n_groups)
  indexOut <- vector("list", n_groups)

  for (g in seq_len(n_groups)) {
    indexOut[[g]] <- which(pixel_group == g)
    index[[g]] <- which(pixel_group != g)
  }

  names(index) <- paste0("sample_group_", seq_along(index))
  names(indexOut) <- names(index)

  list(index = index, indexOut = indexOut)
}



first_scalar <- function(x, default = NULL) {
  if (is.null(x) || length(x) == 0) return(default)
  if (is.data.frame(x)) {
    if (nrow(x) == 0 || ncol(x) == 0) return(default)
    return(first_scalar(x[[1]], default = default))
  }
  if (is.list(x) && !is.atomic(x)) return(first_scalar(x[[1]], default = default))
  x[[1]]
}

normalize_split_info <- function(split_info) {
  if (is.null(split_info)) return(list(strategy = "random"))
  if (is.data.frame(split_info)) split_info <- as.list(split_info[1, , drop = FALSE])
  if (!is.list(split_info)) split_info <- list(strategy = "random")

  list(
    strategy = as.character(first_scalar(split_info$strategy, "random")),
    evaluation_mode = as.character(first_scalar(split_info$evaluation_mode, "cv_plus_test")),
    train_frac = suppressWarnings(as.numeric(first_scalar(split_info$train_frac, NA_real_))),
    seed = suppressWarnings(as.integer(first_scalar(split_info$seed, NA_integer_))),
    cv_folds = suppressWarnings(as.integer(first_scalar(split_info$cv_folds, NA_integer_))),
    block_size = suppressWarnings(as.integer(first_scalar(split_info$block_size, NA_integer_))),
    buffer_radius = suppressWarnings(as.numeric(first_scalar(split_info$buffer_radius, NA_real_))),
    min_pixels_per_block = suppressWarnings(as.integer(first_scalar(split_info$min_pixels_per_block, NA_integer_)))
  )
}

normalize_evaluation_mode <- function(evaluation_mode) {
  mode <- as.character(first_scalar(evaluation_mode, "cv_plus_test"))
  if (!mode %in% c("cv_plus_test", "cv_only")) {
    mode <- "cv_plus_test"
  }
  mode
}

compute_macro_f1_from_predictions <- function(truth, pred, levels_ref = NULL) {
  if (is.null(levels_ref)) {
    levels_ref <- levels(as.factor(truth))
  }

  truth <- factor(truth, levels = levels_ref)
  pred <- factor(pred, levels = levels_ref)

  f1_vals <- vapply(levels_ref, function(cls) {
    tp <- sum(truth == cls & pred == cls, na.rm = TRUE)
    fp <- sum(truth != cls & pred == cls, na.rm = TRUE)
    fn <- sum(truth == cls & pred != cls, na.rm = TRUE)

    precision <- if ((tp + fp) == 0) NA_real_ else tp / (tp + fp)
    recall <- if ((tp + fn) == 0) NA_real_ else tp / (tp + fn)

    if (!is.finite(precision) || !is.finite(recall) || (precision + recall) == 0) {
      return(0)
    }

    2 * precision * recall / (precision + recall)
  }, numeric(1))

  mean(f1_vals, na.rm = TRUE)
}

compute_relative_cm_table <- function(cm_obj) {
  as.data.frame(cm_obj$table) |>
    dplyr::group_by(Reference) |>
    dplyr::mutate(Rel_Freq = Freq / sum(Freq)) |>
    dplyr::ungroup()
}

compute_multiclass_roc_payload <- function(truth, prob_df, class_levels) {
  if (!requireNamespace("pROC", quietly = TRUE)) return(NULL)
  if (is.null(prob_df) || nrow(prob_df) == 0) return(NULL)

  available_levels <- intersect(class_levels, colnames(prob_df))
  if (length(available_levels) == 0) return(NULL)

  roc_data <- lapply(available_levels, function(cls) {
    binary_truth <- as.integer(truth == cls)
    if (length(unique(binary_truth)) < 2L) return(NULL)

    predictor <- suppressWarnings(as.numeric(prob_df[[cls]]))
    if (length(predictor) == 0 || all(!is.finite(predictor))) return(NULL)

    rr <- tryCatch(
      pROC::roc(response = binary_truth, predictor = predictor, quiet = TRUE, direction = "<"),
      error = function(e) NULL
    )

    if (is.null(rr)) return(NULL)

    list(
      class = cls,
      auc = as.numeric(pROC::auc(rr)),
      sensitivities = as.numeric(rr$sensitivities),
      specificities = as.numeric(rr$specificities)
    )
  })

  roc_data <- Filter(Negate(is.null), roc_data)
  if (length(roc_data) == 0) return(NULL)
  roc_data
}

extract_best_cv_predictions <- function(fit, class_levels, label_encoder = NULL) {
  cv_pred_df <- fit$pred
  if (is.null(cv_pred_df) || !is.data.frame(cv_pred_df) || nrow(cv_pred_df) == 0) {
    return(NULL)
  }

  best_tune <- fit$bestTune
  if (is.null(best_tune) || !is.data.frame(best_tune) || nrow(best_tune) == 0) {
    best_tune <- fit$results[which.max(fit$results$Accuracy), , drop = FALSE]
  }

  tune_cols <- intersect(c("mtry", "splitrule", "min.node.size"), names(cv_pred_df))
  for (tc in tune_cols) {
    if (tc %in% names(best_tune)) {
      cv_pred_df <- cv_pred_df[cv_pred_df[[tc]] == best_tune[[tc]][1], , drop = FALSE]
    }
  }

  if (nrow(cv_pred_df) == 0) return(NULL)

  encoded_levels <- if (is.null(label_encoder)) class_levels else label_encoder$safe_levels

  truth_cv <- factor(cv_pred_df$obs, levels = encoded_levels)
  preds_cv <- factor(cv_pred_df$pred, levels = encoded_levels)
  probs_cv <- cv_pred_df[, intersect(encoded_levels, names(cv_pred_df)), drop = FALSE]

  if (!is.null(label_encoder)) {
    truth_cv <- restore_original_class_labels(truth_cv, label_encoder = label_encoder)
    preds_cv <- restore_original_class_labels(preds_cv, label_encoder = label_encoder)
    probs_cv <- restore_probability_column_names(probs_cv, label_encoder = label_encoder)
  } else {
    truth_cv <- factor(truth_cv, levels = class_levels)
    preds_cv <- factor(preds_cv, levels = class_levels)
  }

  list(
    pred_df = cv_pred_df,
    truth = truth_cv,
    pred = preds_cv,
    prob = probs_cv
  )
}

block_merge_threshold <- function(block_size, frac = 0.60) {
  bs <- suppressWarnings(as.numeric(block_size))
  if (!is.finite(bs) || bs < 1) return(1L)
  max(1L, ceiling(frac * (bs^2)))
}

estimate_practical_range <- function(corr_df, threshold_fraction = 0.25) {
  if (!is.data.frame(corr_df) || nrow(corr_df) == 0) return(NA_real_)

  corr_df <- corr_df |>
    dplyr::arrange(feature, distance_mid) |>
    dplyr::group_by(feature) |>
    dplyr::mutate(
      peak_moran = max(moran_i, na.rm = TRUE),
      threshold_value = pmax(0, peak_moran * threshold_fraction)
    ) |>
    dplyr::summarise(
      practical_range = {
        idx <- which(moran_i <= threshold_value)
        if (length(idx) == 0) NA_real_ else distance_mid[min(idx)]
      },
      .groups = "drop"
    ) |>
    dplyr::filter(is.finite(practical_range))

  if (nrow(corr_df) == 0) return(NA_real_)
  stats::median(corr_df$practical_range, na.rm = TRUE)
}

recommend_spatial_params <- function(meta, block_size, merge_frac = 0.60, max_cv_folds = 10L) {
  req_cols <- c("sample_id", "x", "y")
  if (is.null(meta) || !all(req_cols %in% names(meta))) {
    return(list(
      min_pixels_per_block = NA_integer_,
      n_blocks = NA_integer_,
      recommended_cv_folds = NA_integer_
    ))
  }

  meta <- as.data.frame(meta)
  keep <- stats::complete.cases(meta[, req_cols, drop = FALSE])
  meta <- meta[keep, , drop = FALSE]

  if (nrow(meta) == 0) {
    return(list(
      min_pixels_per_block = NA_integer_,
      n_blocks = 0L,
      recommended_cv_folds = 0L
    ))
  }

  block_size <- suppressWarnings(as.numeric(block_size))
  min_pixels_per_block <- block_merge_threshold(block_size, frac = merge_frac)

  parts <- lapply(split(meta, meta$sample_id), function(df_s) {
    x0 <- min(df_s$x, na.rm = TRUE)
    y0 <- min(df_s$y, na.rm = TRUE)
    df_s$bx <- floor((df_s$x - x0) / block_size)
    df_s$by <- floor((df_s$y - y0) / block_size)
    df_s$block_id <- paste(df_s$sample_id, df_s$bx, df_s$by, sep = "::")
    df_s
  })

  part <- dplyr::bind_rows(parts)

  block_tbl <- part |>
    dplyr::group_by(sample_id, bx, by, block_id) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop")

  block_tbl <- merge_small_blocks(
    block_tbl = block_tbl,
    min_pixels_per_block = min_pixels_per_block
  )

  merged_tbl <- block_tbl |>
    dplyr::distinct(merged_block_id, sample_id, merged_n)

  n_blocks <- nrow(merged_tbl)

  recommended_cv_folds <- dplyr::case_when(
    n_blocks < 4  ~ 2L,
    n_blocks < 7  ~ 3L,
    n_blocks < 12 ~ 4L,
    n_blocks < 20 ~ 5L,
    n_blocks < 35 ~ 7L,
    TRUE          ~ min(as.integer(max_cv_folds), 10L)
  )

  list(
    min_pixels_per_block = as.integer(min_pixels_per_block),
    n_blocks = as.integer(n_blocks),
    recommended_cv_folds = as.integer(recommended_cv_folds)
  )
}

merge_small_blocks <- function(block_tbl, min_pixels_per_block = 1L) {
  if (!is.data.frame(block_tbl) || nrow(block_tbl) == 0) {
    return(data.frame())
  }

  block_tbl <- block_tbl |>
    dplyr::mutate(
      merged_block_id = block_id,
      merged_n = n
    )

  repeat {
    merged_units <- block_tbl |>
      dplyr::group_by(sample_id, merged_block_id) |>
      dplyr::summarise(
        merged_n = sum(n),
        bx_cells = list(bx),
        by_cells = list(by),
        .groups = "drop"
      )

    small_units <- merged_units |>
      dplyr::filter(merged_n < min_pixels_per_block)

    if (nrow(small_units) == 0) break

    changed <- FALSE

    for (ii in seq_len(nrow(small_units))) {
      su <- small_units[ii, , drop = FALSE]

      candidates <- merged_units |>
        dplyr::filter(
          sample_id == su$sample_id,
          merged_block_id != su$merged_block_id
        )

      if (nrow(candidates) == 0) next

      bx_this <- unlist(su$bx_cells[[1]])
      by_this <- unlist(su$by_cells[[1]])

      is_neighbor <- vapply(seq_len(nrow(candidates)), function(j) {
        bx_c <- unlist(candidates$bx_cells[[j]])
        by_c <- unlist(candidates$by_cells[[j]])

        any(vapply(seq_along(bx_this), function(k) {
          any((abs(bx_this[k] - bx_c) == 1 & by_this[k] == by_c) |
                (abs(by_this[k] - by_c) == 1 & bx_this[k] == bx_c))
        }, logical(1)))
      }, logical(1))

      candidates <- candidates[is_neighbor, , drop = FALSE]
      if (nrow(candidates) == 0) next

      best_target <- candidates$merged_block_id[which.max(candidates$merged_n)]

      block_tbl$merged_block_id[block_tbl$merged_block_id == su$merged_block_id] <- best_target
      changed <- TRUE
    }

    merged_sizes <- block_tbl |>
      dplyr::group_by(sample_id, merged_block_id) |>
      dplyr::summarise(merged_n = sum(n), .groups = "drop")

    block_tbl <- block_tbl |>
      dplyr::select(-merged_n) |>
      dplyr::left_join(merged_sizes, by = c("sample_id", "merged_block_id"))

    if (!changed) break
  }

  block_tbl
}

assign_blocks_to_folds <- function(block_tbl, cv_folds, seed = 1234L) {
  merged_tbl <- block_tbl |>
    dplyr::distinct(merged_block_id, sample_id, merged_n)

  if (nrow(merged_tbl) < 2L) {
    stop("Need at least 2 merged blocks for spatial CV.")
  }

  set.seed(as.integer(seed))
  merged_tbl$rand <- stats::runif(nrow(merged_tbl))

  merged_tbl <- merged_tbl |>
    dplyr::arrange(dplyr::desc(merged_n), rand)

  k_eff <- min(as.integer(cv_folds), nrow(merged_tbl))
  if (k_eff < 2L) stop("Fewer than 2 effective folds available.")

  fold_blocks <- vector("list", k_eff)
  fold_load <- rep(0, k_eff)

  for (i in seq_len(nrow(merged_tbl))) {
    j <- which.min(fold_load)
    fold_blocks[[j]] <- c(fold_blocks[[j]], merged_tbl$merged_block_id[i])
    fold_load[j] <- fold_load[j] + merged_tbl$merged_n[i]
  }

  fold_blocks
}

get_buffered_block_ids <- function(block_tbl, test_blocks, buffer_radius, block_size) {
  if (!is.finite(buffer_radius) || buffer_radius <= 0) {
    return(unique(test_blocks$merged_block_id))
  }

  buffer_blocks <- ceiling(buffer_radius / block_size)

  out <- unique(unlist(lapply(seq_len(nrow(test_blocks)), function(i) {
    tb <- test_blocks[i, , drop = FALSE]

    hits <- block_tbl |>
      dplyr::filter(
        sample_id == tb$sample_id,
        abs(bx - tb$bx) <= buffer_blocks,
        abs(by - tb$by) <= buffer_blocks
      ) |>
      dplyr::pull(merged_block_id)

    unique(hits)
  })))

  unique(c(out, test_blocks$merged_block_id))
}

make_spatial_block_cv_indices <- function(
    train_meta,
    cv_folds,
    block_size = 25L,
    buffer_radius = 0,
    min_pixels_per_block = NULL,
    seed = 1234L
) {
  cv_folds <- suppressWarnings(as.integer(cv_folds))
  block_size <- suppressWarnings(as.numeric(block_size))
  buffer_radius <- suppressWarnings(as.numeric(buffer_radius))

  if (!is.finite(cv_folds) || cv_folds < 2L) return(NULL)
  if (!is.finite(block_size) || block_size < 2) stop("block_size must be >= 2.")
  if (!is.finite(buffer_radius) || buffer_radius < 0) stop("buffer_radius must be >= 0.")

  if (is.null(min_pixels_per_block) || !is.finite(min_pixels_per_block) || min_pixels_per_block < 1) {
    min_pixels_per_block <- block_merge_threshold(block_size, frac = 0.60)
  } else {
    min_pixels_per_block <- as.integer(min_pixels_per_block)
  }

  req_cols <- c("sample_id", "x", "y")
  if (!all(req_cols %in% names(train_meta))) {
    stop("Spatial CV requires columns: sample_id, x, y.")
  }

  train_meta <- as.data.frame(train_meta)
  train_meta$.orig_row_id <- seq_len(nrow(train_meta))

  keep <- stats::complete.cases(train_meta[, req_cols, drop = FALSE])
  meta_ok <- train_meta[keep, , drop = FALSE]

  if (nrow(meta_ok) < 10L) {
    stop("Too few rows for spatial CV after removing missing coordinates.")
  }

  parts <- lapply(split(meta_ok, meta_ok$sample_id), function(df_s) {
    x0 <- min(df_s$x, na.rm = TRUE)
    y0 <- min(df_s$y, na.rm = TRUE)

    df_s$bx <- floor((as.numeric(df_s$x) - x0) / block_size)
    df_s$by <- floor((as.numeric(df_s$y) - y0) / block_size)
    df_s$block_id <- paste(df_s$sample_id, df_s$bx, df_s$by, sep = "::")
    df_s
  })

  part <- dplyr::bind_rows(parts)

  block_tbl <- part |>
    dplyr::group_by(sample_id, bx, by, block_id) |>
    dplyr::summarise(n = dplyr::n(), .groups = "drop")

  if (nrow(block_tbl) < 2L) {
    stop("Spatial CV failed: fewer than 2 raw blocks.")
  }

  block_tbl <- merge_small_blocks(
    block_tbl = block_tbl,
    min_pixels_per_block = min_pixels_per_block
  )

  part <- part |>
    dplyr::left_join(
      block_tbl[, c("sample_id", "bx", "by", "merged_block_id", "merged_n")],
      by = c("sample_id", "bx", "by")
    )

  merged_tbl <- block_tbl |>
    dplyr::distinct(merged_block_id, sample_id, merged_n)

  if (nrow(merged_tbl) < 2L) {
    stop("Spatial CV failed: fewer than 2 merged blocks.")
  }

  fold_blocks <- assign_blocks_to_folds(
    block_tbl = block_tbl,
    cv_folds = cv_folds,
    seed = seed
  )

  index <- list()
  indexOut <- list()

  for (j in seq_along(fold_blocks)) {
    test_ids <- fold_blocks[[j]]

    test_blocks <- block_tbl |>
      dplyr::filter(merged_block_id %in% test_ids)

    excluded_ids <- get_buffered_block_ids(
      block_tbl = block_tbl,
      test_blocks = test_blocks,
      buffer_radius = buffer_radius,
      block_size = block_size
    )

    te <- sort(unique(part$.orig_row_id[part$merged_block_id %in% test_ids]))
    tr <- sort(unique(part$.orig_row_id[!(part$merged_block_id %in% excluded_ids)]))

    if (length(te) > 0L && length(tr) > 0L) {
      indexOut[[length(indexOut) + 1L]] <- te
      index[[length(index) + 1L]] <- tr
    }
  }

  keep_fold <- vapply(index, length, integer(1)) > 0L &
    vapply(indexOut, length, integer(1)) > 0L

  index <- index[keep_fold]
  indexOut <- indexOut[keep_fold]

  if (length(index) < 2L) {
    stop("Spatial CV failed: fewer than 2 valid folds after buffer exclusion.")
  }

  names(index) <- paste0("block_", seq_along(index))
  names(indexOut) <- names(index)

  attr(index, "min_pixels_per_block") <- min_pixels_per_block
  attr(index, "n_merged_blocks") <- nrow(merged_tbl)

  list(index = index, indexOut = indexOut)
}

build_cv_indices_from_split <- function(train_meta, split_info, cv_folds, seed) {
  split_info <- normalize_split_info(split_info)
  strategy <- split_info$strategy %||% "random"

  cv_folds <- suppressWarnings(as.integer(cv_folds))
  if (!is.finite(cv_folds) || cv_folds <= 1L) return(NULL)

  if (identical(strategy, "leave_one_sample_out")) {
    return(make_loso_cv_indices(train_meta, cv_folds = cv_folds, seed = seed))
  }

  if (identical(strategy, "spatial_block")) {
    return(make_spatial_block_cv_indices(
      train_meta = train_meta,
      cv_folds = cv_folds,
      block_size = split_info$block_size %||% 25L,
      buffer_radius = split_info$buffer_radius %||% 0,
      min_pixels_per_block = split_info$min_pixels_per_block %||% block_merge_threshold(split_info$block_size %||% 25L, 0.60),
      seed = seed
    ))
  }

  make_random_cv_indices(nrow(train_meta), cv_folds, seed)
}

# ---------------------------------------------------------------------------
# train_ranger_from_dataset()
#
# The one authorised entry point for training a Random Forest model.
# All data comes from dataset_id; no direct pipeline_output loading elsewhere.
#
# @param dataset_id       Frozen dataset snapshot _id.
# @param mtry             Number of variables randomly sampled at each split.
# @param splitrule        "gini" (classification) or "extratrees".
# @param min_node_size    Minimum terminal node size.
# @param num_trees        Number of trees.
# @param cv_folds         Number of cross-validation folds (0 = no CV).
# @param seed             Global random seed for full reproducibility.
# @param db / url         MongoDB connection parameters.
#
# @return model_run_id (character).  The fitted model is persisted in MongoDB.
# ---------------------------------------------------------------------------
train_ranger_from_dataset <- function(
    dataset_id,
    normalize_method = c("none", "tic", "median", "rms"),
    feature_standardize = c("none", "sd", "zscore"),
    mtry           = 31L,
    splitrule      = "gini",
    min_node_size  = 10L,
    num_trees      = 500L,
    cv_folds       = 10L,
    seed           = 1234L,
    workers        = NULL,
    num_threads    = 1L,
    db             = DB_NAME,
    url            = MONGO_URL
) {

  stopifnot(
    requireNamespace("caret", quietly = TRUE),
    requireNamespace("ranger", quietly = TRUE),
    requireNamespace("parallel", quietly = TRUE),
    requireNamespace("doParallel", quietly = TRUE),
    requireNamespace("foreach", quietly = TRUE)
  )

  normalize_method <- match.arg(normalize_method)
  feature_standardize <- match.arg(feature_standardize)

  # ── 1. Load dataset ───────────────────────────────────────────────
  message("[train] Loading dataset: ", dataset_id)

  data <- load_dataset_for_training(dataset_id, db, url)

  train_X    <- data$train_X
  train_y    <- data$train_y
  test_X     <- data$test_X
  test_y     <- data$test_y
  train_meta <- data$train_meta %||% NULL
  test_meta  <- data$test_meta %||% NULL
  split_info <- data$split_info %||% list(strategy = "random")
  dataset_meta <- data$dataset_meta %||% NULL

  split_info <- normalize_split_info(split_info)
  evaluation_mode <- normalize_evaluation_mode(split_info$evaluation_mode)
  cv_folds <- split_info$cv_folds %||% cv_folds
  cv_folds <- suppressWarnings(as.integer(cv_folds))
  if (!is.finite(cv_folds)) cv_folds <- as.integer(0)

  has_test_data <-
    !is.null(test_X) &&
    !is.null(test_y) &&
    nrow(train_X) > 0 &&
    nrow(test_X) > 0 &&
    length(test_y) > 0

  if (identical(evaluation_mode, "cv_only") && cv_folds <= 1L) {
    stop("CV-only datasets require cv_folds > 1.")
  }

  message("[train] Applying normalization: ", normalize_method)
  train_X <- normalize_feature_matrix(train_X, normalize_method)
  if (has_test_data) {
    test_X <- normalize_feature_matrix(test_X, normalize_method)
  }

  message("[train] Applying feature standardization: ", feature_standardize)
  feature_standardization <- standardize_feature_matrix(
    train_X,
    method = feature_standardize,
    return_params = TRUE
  )
  train_X <- feature_standardization$data
  if (has_test_data) {
    test_X <- standardize_feature_matrix(
      test_X,
      method = feature_standardize,
      center = feature_standardization$center,
      scale = feature_standardization$scale
    )
  }

  label_encoder <- make_class_label_encoder(train_y)
  train_y_safe <- sanitize_class_labels(train_y, label_encoder = label_encoder)
  test_y_safe <- if (has_test_data) {
    sanitize_class_labels(test_y, label_encoder = label_encoder)
  } else {
    NULL
  }

  message("[train] Train pixels: ", nrow(train_X),
          " | Test pixels: ", if (has_test_data) nrow(test_X) else 0L,
          " | Features: ", ncol(train_X),
          " | Classes: ", nlevels(train_y))
  display_strategy <- if (identical(split_info$strategy, "leave_one_sample_out")) {
    "grouped_sample_out"
  } else {
    split_info$strategy %||% "random"
  }
  message("[train] Split strategy: ", display_strategy)
  message("[train] Evaluation mode: ", evaluation_mode)
  message("[train] CV folds (dataset): ", cv_folds)


  set.seed(seed)

  # ── 2. Compute class weights ──────────────────────────────────────
  cw    <- compute_class_weights(train_y_safe)
  obs_w <- observation_weights_from_labels(train_y_safe, cw)
  cw_display <- stats::setNames(
    as.numeric(cw),
    unname(label_encoder$safe_to_original[names(cw)])
  )

  message("[train] Class weights: ",
          paste(names(cw_display), round(cw_display, 4), sep="=", collapse=", "))


  # ── 2b. Setup parallel backend for CV ─────────────────────────────
  cl <- NULL
  used_parallel <- FALSE

  if (is.null(workers)) {
    max_auto_workers <- if (exists("app_worker_count", mode = "function")) {
      app_worker_count(max_workers = 15L)
    } else {
      15L
    }
    workers <- min(as.integer(cv_folds), max_auto_workers)
  } else {
    workers <- as.integer(workers)
  }

  num_threads <- as.integer(num_threads)

  if (cv_folds > 1L && workers > 1L) {

    cl <- parallel::makePSOCKcluster(workers)
    doParallel::registerDoParallel(cl)

    used_parallel <- TRUE

    message("[train] Parallel CV enabled: workers=",
            workers,
            " | ranger num.threads=",
            num_threads)

  } else {

    message("[train] Parallel CV disabled | ranger num.threads=",
            num_threads)

  }

  on.exit({

    if (!is.null(cl)) {
      try(parallel::stopCluster(cl), silent = TRUE)
    }

    if (used_parallel) {
      try(foreach::registerDoSEQ(), silent = TRUE)
    }

  }, add = TRUE)


  # ── 3. Train model ─────────────────────────────────────────────────

  hyperparams <- list(
    normalize_method = normalize_method,
    feature_standardize = feature_standardize,
    mtry          = as.integer(mtry),
    splitrule     = splitrule,
    min_node_size = as.integer(min_node_size),
    num_trees     = as.integer(num_trees),
    cv_folds      = as.integer(cv_folds),
    seed          = as.integer(seed),
    workers          = if (cv_folds > 1 && !is.null(workers))
                        as.integer(workers)
                      else
                        NA_integer_,
    num_threads   = as.integer(num_threads),
    evaluation_mode = evaluation_mode,
    split_strategy = as.character(split_info$strategy %||% "random"),
    split_block_size = as.integer(split_info$block_size %||% NA_integer_),
    split_buffer_radius = as.numeric(split_info$buffer_radius %||% NA_real_),
    split_cv_folds = as.integer(split_info$cv_folds %||% NA_integer_),
    split_min_pixels_per_block = as.integer(split_info$min_pixels_per_block %||% NA_integer_),
    class_label_map = list(label_encoder$map_df)
  )

  tune_grid <- expand.grid(
    mtry          = mtry,
    splitrule     = splitrule,
    min.node.size = min_node_size
  )

  cv_idx <- NULL
    if (cv_folds > 1L && !is.null(train_meta)) {
      cv_idx <- build_cv_indices_from_split(train_meta, split_info, cv_folds, seed)

      if (!is.null(cv_idx)) {
        message(
          "[train] Using custom CV indices for strategy: ",
          split_info$strategy %||% "random",
          " | folds=", length(cv_idx$index)
        )

        fold_diag <- lapply(seq_along(cv_idx$index), function(i) {
          tr_idx <- cv_idx$index[[i]]
          te_idx <- cv_idx$indexOut[[i]]

          tr_y_i <- train_y[tr_idx]
          te_y_i <- train_y[te_idx]

          data.frame(
            fold = i,
            n_train = length(tr_idx),
            n_test = length(te_idx),
            n_train_classes = length(unique(as.character(tr_y_i))),
            n_test_classes = length(unique(te_y_i)),
            train_class_counts = paste(names(table(tr_y_i)), as.integer(table(tr_y_i)), collapse = "; "),
            test_class_counts  = paste(names(table(te_y_i)), as.integer(table(te_y_i)), collapse = "; "),
            stringsAsFactors = FALSE
          )
        })

        fold_diag_df <- dplyr::bind_rows(fold_diag)
        print(fold_diag_df)

        bad_folds <- fold_diag_df |>
          dplyr::filter(
            n_train < 20 |
            n_test < 5 |
            n_train_classes < nlevels(train_y) |
            n_test_classes < 1
          )

        if (nrow(bad_folds) > 0) {
          stop(
            "Spatial CV produced invalid folds. Check fold sizes/class coverage.\n",
            paste(capture.output(print(bad_folds)), collapse = "\n")
          )
        }
      }
    }

  ctrl <- if (cv_folds > 1L) {

    if (!is.null(cv_idx)) {
      caret::trainControl(
        method          = "cv",
        number          = length(cv_idx$index),
        index           = cv_idx$index,
        indexOut        = cv_idx$indexOut,
        classProbs      = TRUE,
        summaryFunction = caret::multiClassSummary,
        allowParallel   = workers > 1L,
        savePredictions = "final"
      )
    } else {
      caret::trainControl(
        method          = "cv",
        number          = cv_folds,
        classProbs      = TRUE,
        summaryFunction = caret::multiClassSummary,
        allowParallel   = workers > 1L,
        savePredictions = "final"
      )
    }

  } else {

    caret::trainControl(
      method     = "none",
      classProbs = TRUE
    )

  }

  message("[train] Fitting ranger (num.trees=", num_trees,
          ", cv_folds=", cv_folds,
          ", workers=", workers,
          ", num.threads=", num_threads, ")...")

  set.seed(seed)

  fit <- caret::train(
    x            = train_X,
    y            = train_y_safe,
    method       = "ranger",
    trControl    = ctrl,
    tuneGrid     = tune_grid,
    num.trees    = num_trees,
    importance   = "permutation",
    weights      = obs_w,
    num.threads  = num_threads
  )
  fit$msi_class_label_encoder <- list(label_encoder$map_df)
  fit$msi_feature_standardization <- list(
    method = feature_standardize,
    center = feature_standardization$center,
    scale = feature_standardization$scale
  )
  message("[train] Model fitting finished.")
  metrics_scalar <- list(
    evaluation_mode = evaluation_mode,
    has_test_set    = has_test_data,
    test_accuracy  = NA_real_,
    test_kappa     = NA_real_,
    test_acc_lower = NA_real_,
    test_acc_upper = NA_real_,
    n_test         = if (has_test_data) nrow(test_X) else 0L,
    n_train        = nrow(train_X),
    n_classes      = nlevels(train_y),
    n_features     = ncol(train_X),
    split_strategy = as.character(split_info$strategy %||% "random")
  )

  model <- fit$finalModel
  importance_vals <- model$variable.importance %||% NULL
  importance_df <- NULL

  if (!is.null(importance_vals)) {
    importance_vals <- suppressWarnings(as.numeric(importance_vals))
    importance_names <- names(model$variable.importance %||% NULL)

    if (length(importance_vals) > 0 && length(importance_names) == length(importance_vals)) {
      importance_df <- data.frame(
        feature = as.character(importance_names),
        importance = importance_vals,
        stringsAsFactors = FALSE
      )
      importance_df <- importance_df[order(importance_df$importance, decreasing = TRUE), , drop = FALSE]
      rownames(importance_df) <- NULL
    }
  }

  if (is.null(importance_df) || nrow(importance_df) == 0) {
    metrics_scalar$permutation_importance_message <- "Permutation importance is unavailable for this run."
  } else if (all(is.na(importance_df$importance))) {
    metrics_scalar$permutation_importance_message <- "Permutation importance contains only missing values for this run."
  } else {
    metrics_scalar$permutation_importance <- list(importance_df)
  }

  # CV metrics
  if (cv_folds > 1L) {
    best_row <- fit$bestTune
    if (is.null(best_row) || !is.data.frame(best_row) || nrow(best_row) == 0) {
      best_row <- fit$results[which.max(fit$results$Accuracy), , drop = FALSE]
    } else {
      tune_cols <- intersect(names(best_row), names(fit$results))
      best_match <- rep(TRUE, nrow(fit$results))
      for (tc in tune_cols) {
        best_match <- best_match & (fit$results[[tc]] == best_row[[tc]][1])
      }
      best_row <- fit$results[best_match, , drop = FALSE]
      if (nrow(best_row) == 0) {
        best_row <- fit$results[which.max(fit$results$Accuracy), , drop = FALSE]
      }
    }

    metrics_scalar$cv_mean_accuracy <- as.numeric(best_row$Accuracy)
    metrics_scalar$cv_mean_kappa    <- as.numeric(best_row$Kappa)
    metrics_scalar$cv_mean_f1       <- as.numeric(best_row$Mean_F1)
    metrics_scalar$cv_acc_sd        <- as.numeric(best_row$AccuracySD)

    cv_pred_payload <- extract_best_cv_predictions(
      fit,
      class_levels = label_encoder$original_levels,
      label_encoder = label_encoder
    )

    if (is.null(cv_pred_payload)) {
      metrics_scalar$cv_warning <- "Cross-validation plots require savePredictions = 'final' and available fit$pred data."
    } else {
      truth_cv <- cv_pred_payload$truth
      preds_cv <- cv_pred_payload$pred
      probs_cv <- cv_pred_payload$prob

      cm_cv <- caret::confusionMatrix(preds_cv, truth_cv)
      metrics_scalar$cv_cm_table <- list(compute_relative_cm_table(cm_cv))
      metrics_scalar$n_cv_predictions <- nrow(cv_pred_payload$pred_df)
      metrics_scalar$cv_macro_f1_from_predictions <- compute_macro_f1_from_predictions(
        truth = truth_cv,
        pred = preds_cv,
        levels_ref = label_encoder$original_levels
      )

      roc_data_cv <- compute_multiclass_roc_payload(
        truth = truth_cv,
        prob_df = probs_cv,
        class_levels = label_encoder$original_levels
      )
      if (!is.null(roc_data_cv)) {
        metrics_scalar$cv_roc_data <- list(roc_data_cv)
      }
    }
  }

  if (has_test_data) {
    message("[train] Starting prediction on held-out test set...")

    preds <- restore_original_class_labels(
      factor(predict(fit, newdata = test_X), levels = label_encoder$safe_levels),
      label_encoder = label_encoder
    )
    truth_test <- restore_original_class_labels(
      factor(test_y_safe, levels = label_encoder$safe_levels),
      label_encoder = label_encoder
    )
    cm <- caret::confusionMatrix(preds, truth_test)

    metrics_scalar$test_accuracy <- as.numeric(cm$overall["Accuracy"])
    metrics_scalar$test_kappa <- as.numeric(cm$overall["Kappa"])
    metrics_scalar$test_acc_lower <- as.numeric(cm$overall["AccuracyLower"])
    metrics_scalar$test_acc_upper <- as.numeric(cm$overall["AccuracyUpper"])
    metrics_scalar$byclass_table <- list(build_byclass_metrics_table(cm$byClass, label_encoder = label_encoder))

    bc <- as.data.frame(cm$byClass)
    for (col in colnames(bc)) {
      for (cls in rownames(bc)) {
        key <- paste0(
          "byclass_",
          gsub("[^A-Za-z0-9]", "_", col), "__",
          gsub("[^A-Za-z0-9]", "_", gsub("^Class: ", "", cls))
        )
        metrics_scalar[[key]] <- as.numeric(bc[cls, col])
      }
    }

    metrics_scalar$cm_table <- list(compute_relative_cm_table(cm))

    probs_test <- restore_probability_column_names(
      predict(fit, newdata = test_X, type = "prob"),
      label_encoder = label_encoder
    )
    roc_data_test <- compute_multiclass_roc_payload(
      truth = truth_test,
      prob_df = probs_test,
      class_levels = label_encoder$original_levels
    )
    if (!is.null(roc_data_test)) {
      metrics_scalar$roc_data <- list(roc_data_test)
    }

    message("[train] Test accuracy: ", round(metrics_scalar$test_accuracy, 4),
            " | Kappa: ", round(metrics_scalar$test_kappa, 4))
  } else {
    message("[train] No held-out test set configured for this dataset.")
  }

  # ── 6. Save model run ─────────────────────────────────────────────
  run_id <- save_model_run(
    dataset_id  = dataset_id,
    model_type  = "ranger",
    hyperparams = hyperparams,
    metrics     = metrics_scalar,
    model_obj   = fit,
    db          = db,
    url         = url
  )

  message("[train] Done. model_run_id: ", run_id)
  invisible(run_id)
}
