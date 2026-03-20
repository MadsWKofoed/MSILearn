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

  train_X <- data$train_X
  train_y <- data$train_y
  test_X  <- data$test_X
  test_y  <- data$test_y

  message("[train] Applying normalization: ", normalize_method)
  train_X <- normalize_feature_matrix(train_X, normalize_method)
  test_X  <- normalize_feature_matrix(test_X,  normalize_method)

  message("[train] Train pixels: ", nrow(train_X),
          " | Test pixels: ", nrow(test_X),
          " | Features: ", ncol(train_X),
          " | Classes: ", nlevels(train_y))

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
    num_threads   = as.integer(num_threads)
  )

  tune_grid <- expand.grid(
    mtry          = mtry,
    splitrule     = splitrule,
    min.node.size = min_node_size
  )

  ctrl <- if (cv_folds > 1L) {

    caret::trainControl(
      method          = "cv",
      number          = cv_folds,
      classProbs      = TRUE,
      summaryFunction = caret::multiClassSummary,
      allowParallel   = workers > 1L
    )

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
    n_features     = ncol(train_X)
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


