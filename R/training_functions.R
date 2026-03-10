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
    mtry           = 31L,
    splitrule      = "gini",
    min_node_size  = 10L,
    num_trees      = 500L,
    cv_folds       = 10L,
    seed           = 1234L,
    workers        = NULL,
    num_threads    = 2L,
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

  # ── 1. Load dataset ───────────────────────────────────────────────
  message("[train] Loading dataset: ", dataset_id)

  data <- load_dataset_for_training(dataset_id, db, url)

  train_X <- data$train_X
  train_y <- data$train_y
  test_X  <- data$test_X
  test_y  <- data$test_y

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
    mtry          = as.integer(mtry),
    splitrule     = splitrule,
    min_node_size = as.integer(min_node_size),
    num_trees     = as.integer(num_trees),
    cv_folds      = as.integer(cv_folds),
    seed          = as.integer(seed),
    workers       = as.integer(workers),
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

  # ── 4. Evaluate on test set ───────────────────────────────────────
  preds <- predict(fit, newdata = test_X)

  message("[train] Class prediction finished.")
message("[train] Starting probability prediction...")

  probs <- predict(fit, newdata = test_X, type = "prob")
message("[train] Probability prediction finished.")
message("[train] Building confusion matrix...")

  # ── 5. Compute metrics ────────────────────────────────────────────
  cm <- caret::confusionMatrix(preds, test_y)
  message("[train] Confusion matrix finished.")
message("[train] Preparing metrics...")

  test_accuracy <- unname(cm$overall["Accuracy"])
  test_kappa    <- unname(cm$overall["Kappa"])


  # ── 6. Save model run ─────────────────────────────────────────────
    run_id <- save_model_run(
      dataset_id   = dataset_id,
      model_type   = "ranger",
      hyperparams  = hyperparams,
      metrics      = metrics_scalar,
      model_obj    = fit,
      db           = db,
      url          = url
    )

  message("[train] Model stored with run_id: ", run_id)

  return(run_id)
}


# ---------------------------------------------------------------------------
# predict_from_model_run()
#
# Load a persisted model and predict on new features.
# Feature matrix must come from an artifact loaded via pipeline_id,
# NOT from ad-hoc file loading.
#
# @param run_id      model_run_id (from save_model_run / train_ranger_from_dataset).
# @param new_X       Numeric matrix. Column names must match training features.
#
# @return Factor of predicted class labels.
# ---------------------------------------------------------------------------
predict_from_model_run <- function(run_id, new_X, db = DB_NAME, url = MONGO_URL) {
  message("[predict] Loading model run: ", run_id)
  fit <- load_model_run(run_id, db, url)

  # Guard: column alignment
  train_cols <- colnames(fit$trainingData)[-ncol(fit$trainingData)]   # caret adds .outcome
  missing_cols <- setdiff(train_cols, colnames(new_X))
  extra_cols   <- setdiff(colnames(new_X), train_cols)

  if (length(missing_cols) > 0) {
    stop("[predict] New data is missing columns present during training: ",
         paste(missing_cols, collapse = ", "))
  }
  if (length(extra_cols) > 0) {
    message("[predict] Extra columns in new_X will be dropped: ",
            paste(extra_cols, collapse = ", "))
    new_X <- new_X[, train_cols, drop = FALSE]
  }

  preds <- predict(fit, newdata = new_X)
  message("[predict] Predicted ", length(preds), " pixels.")
  preds
}
