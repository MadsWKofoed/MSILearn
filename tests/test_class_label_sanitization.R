source("R/mongo_functions.R")
source("R/training_functions.R")

assert_true <- function(condition, message) {
  if (!isTRUE(condition)) {
    stop(message, call. = FALSE)
  }
}

run_tests <- function() {
  original_levels <- c("good", "very bad", "very good")
  labels <- factor(
    c("good", "very bad", "very good", "very bad", "good"),
    levels = original_levels
  )

  encoder <- make_class_label_encoder(labels)

  assert_true(
    identical(encoder$original_levels, original_levels),
    "Original class levels should be preserved."
  )
  assert_true(
    identical(encoder$safe_levels, c("good", "very.bad", "very.good")),
    "Class levels with spaces should be sanitized with make.names()."
  )

  safe_labels <- sanitize_class_labels(labels, label_encoder = encoder)
  assert_true(
    identical(levels(safe_labels), encoder$safe_levels),
    "Sanitized factor should expose safe training levels."
  )

  restored_labels <- restore_original_class_labels(safe_labels, label_encoder = encoder)
  assert_true(
    identical(as.character(restored_labels), as.character(labels)),
    "Restored labels should match the original user-facing labels."
  )
  assert_true(
    identical(levels(restored_labels), original_levels),
    "Restored factor levels should match the original label order."
  )

  prob_df <- data.frame(
    good = c(0.8, 0.1),
    very.bad = c(0.1, 0.7),
    very.good = c(0.1, 0.2),
    check.names = FALSE
  )
  restored_prob_df <- restore_probability_column_names(prob_df, label_encoder = encoder)
  assert_true(
    identical(colnames(restored_prob_df), original_levels),
    "Probability columns should be renamed back to the original labels."
  )

  mock_fit <- list(
    pred = data.frame(
      obs = c("good", "very.bad", "very.good"),
      pred = c("good", "very.bad", "very.good"),
      good = c(0.9, 0.1, 0.1),
      very.bad = c(0.05, 0.8, 0.1),
      very.good = c(0.05, 0.1, 0.8),
      mtry = c(1L, 1L, 1L),
      splitrule = c("gini", "gini", "gini"),
      min.node.size = c(1L, 1L, 1L),
      stringsAsFactors = FALSE,
      check.names = FALSE
    ),
    bestTune = data.frame(
      mtry = 1L,
      splitrule = "gini",
      min.node.size = 1L,
      stringsAsFactors = FALSE
    ),
    results = data.frame(
      mtry = 1L,
      splitrule = "gini",
      min.node.size = 1L,
      Accuracy = 1,
      stringsAsFactors = FALSE
    )
  )

  cv_payload <- extract_best_cv_predictions(
    mock_fit,
    class_levels = original_levels,
    label_encoder = encoder
  )
  assert_true(
    identical(levels(cv_payload$truth), original_levels),
    "CV truth labels should be restored to original levels."
  )
  assert_true(
    identical(levels(cv_payload$pred), original_levels),
    "CV predicted labels should be restored to original levels."
  )
  assert_true(
    identical(colnames(cv_payload$prob), original_levels),
    "CV probability columns should be restored to original labels."
  )

  if (requireNamespace("caret", quietly = TRUE) && requireNamespace("ranger", quietly = TRUE)) {
    set.seed(42)
    toy_x <- data.frame(
      mz_1 = c(rnorm(8, -2), rnorm(8, 0), rnorm(8, 2)),
      mz_2 = c(rnorm(8, -1), rnorm(8, 1), rnorm(8, 3)),
      stringsAsFactors = FALSE
    )
    toy_y <- factor(rep(original_levels, each = 8), levels = original_levels)
    toy_y_safe <- sanitize_class_labels(toy_y, label_encoder = encoder)

    ctrl <- caret::trainControl(
      method = "cv",
      number = 3,
      classProbs = TRUE,
      summaryFunction = caret::multiClassSummary,
      savePredictions = "final"
    )

    fit <- caret::train(
      x = toy_x,
      y = toy_y_safe,
      method = "ranger",
      trControl = ctrl,
      tuneGrid = data.frame(
        mtry = 1L,
        splitrule = "gini",
        min.node.size = 1L
      ),
      num.trees = 25L,
      importance = "none",
      num.threads = 1L
    )

    toy_preds <- restore_original_class_labels(predict(fit, newdata = toy_x), label_encoder = encoder)
    toy_probs <- restore_probability_column_names(
      predict(fit, newdata = toy_x, type = "prob"),
      label_encoder = encoder
    )

    assert_true(
      identical(levels(toy_preds), original_levels),
      "Toy training predictions should restore original class levels."
    )
    assert_true(
      identical(colnames(toy_probs), original_levels),
      "Toy training probability columns should restore original class labels."
    )
  }

  cat("Class label sanitization tests passed.\n")
}

run_tests()
