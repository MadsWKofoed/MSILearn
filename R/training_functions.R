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
    n_bins = 15L,
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


compute_global_moran_for_features <- function(
    X,
    meta,
    sample_size = 2000L,
    seed = 1234L,
    k = 8L
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

  out <- lapply(seq_len(ncol(X)), function(j) {
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

  out <- Filter(Negate(is.null), out)
  if (length(out) == 0L) return(empty_df)

  dplyr::bind_rows(out) |>
    dplyr::arrange(dplyr::desc(moran_i))
}


compute_feature_moran_diagnostics <- function(
    X,
    meta,
    top_n_features = 12L,
    max_points = 3000L,
    n_bins = 15L,
    seed = 1234L
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

  top_tbl <- moran_tbl |>
    dplyr::filter(is.finite(moran_i)) |>
    dplyr::slice_head(n = top_n_features)

  coords <- as.matrix(meta[, req_cols, drop = FALSE])

  corr_list <- lapply(top_tbl$feature, function(feat) {
    j <- match(feat, colnames(X))
    vals <- X[, j]

    corr <- compute_moran_correlogram(
      coords = coords,
      values = vals,
      n_bins = n_bins
    )
    if (nrow(corr) == 0L) return(NULL)

    corr$feature <- feat
    corr
  })

  corr_list <- Filter(Negate(is.null), corr_list)

  corr_df <- if (length(corr_list) > 0L) {
    dplyr::bind_rows(corr_list)
  } else {
    empty_res$feature_correlogram
  }

  range_tbl <- if (nrow(corr_df) > 0L) {
    corr_df |>
      dplyr::group_by(feature) |>
      dplyr::arrange(distance_mid, .by_group = TRUE) |>
      dplyr::summarise(
        range_estimate = {
          idx <- which(moran_i <= 0)
          if (length(idx) == 0) NA_real_ else distance_mid[min(idx)]
        },
        .groups = "drop"
      )
  } else {
    empty_res$feature_range_summary
  }

  recommended_buffer_radius <- if (nrow(range_tbl) > 0L) {
    stats::quantile(range_tbl$range_estimate, probs = 0.75, na.rm = TRUE, names = FALSE)
  } else {
    NA_real_
  }

  list(
    feature_moran_summary = moran_tbl,
    feature_correlogram = corr_df,
    feature_range_summary = range_tbl,
    recommended_buffer_radius = as.numeric(recommended_buffer_radius)
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

make_spatial_block_cv_indices <- function(train_meta, cv_folds, block_size = 25L,
                                          buffer_radius = 0, seed = 1234L) {
  block_ids <- assign_spatial_block_ids(train_meta, block_size)
  ublocks <- unique(block_ids)
  set.seed(as.integer(seed))
  shuffled <- sample(ublocks, length(ublocks))
  grp <- split(shuffled, cut(seq_along(shuffled), breaks = cv_folds, labels = FALSE))
  grp <- Filter(length, grp)
  indexOut <- lapply(seq_along(grp), function(i) which(block_ids %in% grp[[i]]))
  names(indexOut) <- paste0("block_", seq_along(indexOut))
  index <- lapply(indexOut, function(te) {
    excl <- compute_buffer_exclusion_idx(train_meta, te, buffer_radius)
    setdiff(seq_len(nrow(train_meta)), unique(c(te, excl)))
  })
  names(index) <- names(indexOut)
  keep <- vapply(index, length, integer(1)) > 0 & vapply(indexOut, length, integer(1)) > 0
  list(index = index[keep], indexOut = indexOut[keep])
}

build_cv_indices_from_split <- function(train_meta, split_info, cv_folds, seed) {
  strategy <- as.character(split_info$strategy %||% "random")
  if (cv_folds <= 1L) return(NULL)
  if (strategy == "leave_one_sample_out") {
    return(make_loso_cv_indices(train_meta))
  }
  if (strategy == "spatial_block") {
    return(make_spatial_block_cv_indices(
      train_meta = train_meta,
      cv_folds = cv_folds,
      block_size = as.integer(split_info$block_size %||% 25L),
      buffer_radius = as.numeric(split_info$buffer_radius %||% 0),
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
      message("[train] Using custom CV indices for strategy: ", split_info$strategy %||% "random",
              " | folds=", length(cv_idx$index))
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


