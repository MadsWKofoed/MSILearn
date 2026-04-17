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
# PCA + Moran correlogram diagnostics on training pixels
# ---------------------------------------------------------------------------

.compute_distance_matrix <- function(coords) {
  as.matrix(stats::dist(coords))
}

compute_moran_correlogram <- function(
    coords,
    values,
    n_bins = 150L,
    max_dist = NULL,
    min_pairs = 30L
) {
  stopifnot(nrow(coords) == length(values))

  coords <- as.matrix(coords)
  storage.mode(coords) <- "numeric"
  values <- as.numeric(values)

  ok <- is.finite(values) & stats::complete.cases(coords)
  coords <- coords[ok, , drop = FALSE]
  values <- values[ok]

  if (nrow(coords) < 10L) {
    return(data.frame(
      distance_mid = numeric(0),
      distance_min = numeric(0),
      distance_max = numeric(0),
      moran_i = numeric(0),
      n_pairs = integer(0)
    ))
  }

  z <- values - mean(values)
  denom <- sum(z^2)
  if (!is.finite(denom) || denom <= 0) {
    return(data.frame(
      distance_mid = numeric(0),
      distance_min = numeric(0),
      distance_max = numeric(0),
      moran_i = numeric(0),
      n_pairs = integer(0)
    ))
  }

  dmat <- .compute_distance_matrix(coords)
  upper_idx <- upper.tri(dmat, diag = FALSE)

  dists <- dmat[upper_idx]
  if (is.null(max_dist)) {
    max_dist <- stats::quantile(dists, probs = 0.9, na.rm = TRUE, names = FALSE)
  }

  if (!is.finite(max_dist) || max_dist <= 0) {
    return(data.frame(
      distance_mid = numeric(0),
      distance_min = numeric(0),
      distance_max = numeric(0),
      moran_i = numeric(0),
      n_pairs = integer(0)
    ))
  }

  breaks <- unique(as.numeric(seq(0, max_dist, length.out = n_bins + 1L)))
  if (length(breaks) < 2L) {
    return(data.frame(
      distance_mid = numeric(0),
      distance_min = numeric(0),
      distance_max = numeric(0),
      moran_i = numeric(0),
      n_pairs = integer(0)
    ))
  }

  zi <- outer(z, z, "*")
  zij <- zi[upper_idx]

  out <- lapply(seq_len(length(breaks) - 1L), function(i) {
    lo <- breaks[i]
    hi <- breaks[i + 1L]
    pick <- dists >= lo & dists < hi

    n_pairs_i <- sum(pick)
    if (n_pairs_i < min_pairs) {
      return(NULL)
    }

    s0 <- 2 * n_pairs_i
    num <- 2 * sum(zij[pick], na.rm = TRUE)
    moran_i <- (length(z) / s0) * (num / denom)

    data.frame(
      distance_mid = (lo + hi) / 2,
      distance_min = lo,
      distance_max = hi,
      moran_i = moran_i,
      n_pairs = n_pairs_i
    )
  })

  out <- Filter(Negate(is.null), out)
  if (length(out) == 0L) {
    return(data.frame(
      distance_mid = numeric(0),
      distance_min = numeric(0),
      distance_max = numeric(0),
      moran_i = numeric(0),
      n_pairs = integer(0)
    ))
  }

  do.call(rbind, out)
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
    max_points = 1200L,
    n_bins = 150L,
    local_decay_threshold = 0.9,
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
        "coords",
        "n_bins",
        "compute_moran_correlogram",
        ".compute_distance_matrix"
      ),
      envir = environment()
    )

    corr_list <- parallel::parLapply(cl, feature_names, function(feat) {
      j <- match(feat, colnames(X))
      vals <- X_corr[, j]

      corr <- compute_moran_correlogram(
        coords = coords,
        values = vals,
        n_bins = n_bins
      )
      if (nrow(corr) == 0L) return(NULL)

      corr$feature <- feat
      corr
    })
  } else {
    corr_list <- lapply(feature_names, function(feat) {
      j <- match(feat, colnames(X))
      vals <- X_corr[, j]

      corr <- compute_moran_correlogram(
        coords = coords,
        values = vals,
        n_bins = n_bins
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

make_loso_cv_indices <- function(train_meta) {
  sample_ids <- unique(train_meta$sample_id)
  if (length(sample_ids) < 2) stop("Need at least 2 training samples for leave-one-sample-out CV.")
  indexOut <- lapply(sample_ids, function(sid) which(train_meta$sample_id == sid))
  names(indexOut) <- paste0("sample_", seq_along(indexOut))
  index <- lapply(indexOut, function(te) setdiff(seq_len(nrow(train_meta)), te))
  names(index) <- names(indexOut)
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
    train_frac = suppressWarnings(as.numeric(first_scalar(split_info$train_frac, NA_real_))),
    seed = suppressWarnings(as.integer(first_scalar(split_info$seed, NA_integer_))),
    block_size = suppressWarnings(as.integer(first_scalar(split_info$block_size, NA_integer_))),
    buffer_radius = suppressWarnings(as.numeric(first_scalar(split_info$buffer_radius, NA_real_))),
    min_pixels_per_block = suppressWarnings(as.integer(first_scalar(split_info$min_pixels_per_block, NA_integer_)))
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
    return(make_loso_cv_indices(train_meta))
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
# All data comes from dataset_id; no direct artifact loading elsewhere.
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

  message("[train] Applying normalization: ", normalize_method)
  train_X <- normalize_feature_matrix(train_X, normalize_method)
  test_X  <- normalize_feature_matrix(test_X,  normalize_method)

  message("[train] Train pixels: ", nrow(train_X),
          " | Test pixels: ", nrow(test_X),
          " | Features: ", ncol(train_X),
          " | Classes: ", nlevels(train_y))
  message("[train] Split strategy: ", split_info$strategy %||% "random")


  set.seed(seed)

  # ── 2. Compute class weights ──────────────────────────────────────
  cw    <- compute_class_weights(train_y)
  obs_w <- observation_weights_from_labels(train_y, cw)

  message("[train] Class weights: ",
          paste(names(cw), round(cw, 4), sep="=", collapse=", "))


  # ── 2b. Setup parallel backend for CV ─────────────────────────────
  cl <- NULL
  used_parallel <- FALSE

  if (is.null(workers)) {
    workers <- min(as.integer(cv_folds), 15L)
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
    split_strategy = as.character(split_info$strategy %||% "random"),
    split_block_size = as.integer(split_info$block_size %||% NA_integer_),
    split_buffer_radius = as.numeric(split_info$buffer_radius %||% NA_real_)
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
            n_train_classes = length(unique(tr_y_i)),
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
        allowParallel   = workers > 1L
      )
    } else {
      caret::trainControl(
        method          = "cv",
        number          = cv_folds,
        classProbs      = TRUE,
        summaryFunction = caret::multiClassSummary,
        allowParallel   = workers > 1L
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
    y            = train_y,
    method       = "ranger",
    trControl    = ctrl,
    tuneGrid     = tune_grid,
    num.trees    = num_trees,
    weights      = obs_w,
    num.threads  = num_threads
  )
  message("[train] Model fitting finished.")
  message("[train] Starting prediction on held-out test set...")

  # ── 4. Evaluate on held-out test set ─────────────────────────────
  preds <- predict(fit, newdata = test_X)
  cm    <- caret::confusionMatrix(preds, test_y)

  metrics_scalar <- list(
    test_accuracy  = as.numeric(cm$overall["Accuracy"]),
    test_kappa     = as.numeric(cm$overall["Kappa"]),
    test_acc_lower = as.numeric(cm$overall["AccuracyLower"]),
    test_acc_upper = as.numeric(cm$overall["AccuracyUpper"]),
    n_test         = nrow(test_X),
    n_train        = nrow(train_X),
    n_classes      = nlevels(train_y),
    n_features     = ncol(train_X),
    split_strategy = as.character(split_info$strategy %||% "random")
  )

  # CV metrics
  if (cv_folds > 1L) {
    best_row <- fit$results[which.max(fit$results$Accuracy), ]
    metrics_scalar$cv_mean_accuracy <- as.numeric(best_row$Accuracy)
    metrics_scalar$cv_mean_kappa    <- as.numeric(best_row$Kappa)
    metrics_scalar$cv_mean_f1       <- as.numeric(best_row$Mean_F1)
    metrics_scalar$cv_acc_sd        <- as.numeric(best_row$AccuracySD)
  }

  # Per-class stats — one scalar per class×metric
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

  # Confusion matrix table
  cm_df <- as.data.frame(cm$table) |>
    dplyr::group_by(Reference) |>
    dplyr::mutate(Rel_Freq = Freq / sum(Freq)) |>
    dplyr::ungroup()
  metrics_scalar[["cm_table"]] <- list(cm_df)


  # ROC data
  if (requireNamespace("pROC", quietly = TRUE)) {
    probs <- predict(fit, newdata = test_X, type = "prob")
    class_levels <- levels(test_y)

    roc_data <- lapply(class_levels, function(cls) {
      binary <- as.integer(test_y == cls)
      prob   <- probs[[cls]]

      tryCatch({
        r   <- pROC::roc(binary, prob, quiet = TRUE)
        auc <- as.numeric(pROC::auc(r))
        list(
          class         = cls,
          auc           = auc,
          sensitivities = as.numeric(r$sensitivities),
          specificities = as.numeric(r$specificities)
        )
      }, error = function(e) {
        list(
          class         = cls,
          auc           = NA_real_,
          sensitivities = numeric(0),
          specificities = numeric(0)
        )
      })
    })

    metrics_scalar[["roc_data"]] <- list(roc_data)
  }

  message("[train] Test accuracy: ", round(metrics_scalar$test_accuracy, 4),
          " | Kappa: ", round(metrics_scalar$test_kappa, 4))
    
  
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


