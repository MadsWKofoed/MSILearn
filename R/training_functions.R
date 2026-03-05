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
    db             = DB_NAME,
    url            = MONGO_URL
) {
  # ── 0.  Validate required packages ──────────────────────────────────────
  stopifnot(
    requireNamespace("caret",  quietly = TRUE),
    requireNamespace("ranger", quietly = TRUE)
  )

  # ── 1.  Load data via the frozen dataset snapshot ────────────────────────
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

  # ── 2.  Class weights ────────────────────────────────────────────────────
  set.seed(seed)
  cw   <- compute_class_weights(train_y)
  obs_w <- observation_weights_from_labels(train_y, cw)

  message("[train] Class weights: ",
          paste(names(cw), round(cw, 4), sep = "=", collapse = ", "))

  # ── 3.  Build hyperparameter grid and trainControl ───────────────────────
  hyperparams <- list(
    mtry          = as.integer(mtry),
    splitrule     = splitrule,
    min_node_size = as.integer(min_node_size),
    num_trees     = as.integer(num_trees),
    cv_folds      = as.integer(cv_folds),
    seed          = as.integer(seed)
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
      allowParallel   = TRUE
    )
  } else {
    caret::trainControl(
      method     = "none",
      classProbs = TRUE
    )
  }

  # ── 4.  Train ────────────────────────────────────────────────────────────
  message("[train] Fitting ranger (num.trees=", num_trees,
          ", cv_folds=", cv_folds, ")...")
  set.seed(seed)

  fit <- caret::train(
    x         = train_X,
    y         = train_y,
    method    = "ranger",
    trControl = ctrl,
    tuneGrid  = tune_grid,
    num.trees = num_trees,
    weights   = obs_w
  )

  # ── 5.  Evaluate on held-out test set ────────────────────────────────────
  preds <- predict(fit, newdata = test_X)
  cm    <- caret::confusionMatrix(preds, test_y)

  metrics_scalar <- list(
    # Test set
    test_accuracy = as.numeric(cm$overall["Accuracy"]),
    test_kappa    = as.numeric(cm$overall["Kappa"]),
    test_acc_lower = as.numeric(cm$overall["AccuracyLower"]),
    test_acc_upper = as.numeric(cm$overall["AccuracyUpper"]),
    n_test        = nrow(test_X),
    n_train       = nrow(train_X),
    n_classes     = nlevels(train_y),
    n_features    = ncol(train_X)
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
      key <- paste0("byclass_",
                    gsub("[^A-Za-z0-9]", "_", col), "__",
                    gsub("[^A-Za-z0-9]", "_", gsub("^Class: ", "", cls)))
      metrics_scalar[[key]] <- as.numeric(bc[cls, col])
    }
  }

  message("[train] Test accuracy: ", round(metrics_scalar$test_accuracy, 4),
          " | Kappa: ", round(metrics_scalar$test_kappa, 4))

  # ── 6.  Persist run ──────────────────────────────────────────────────────
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
