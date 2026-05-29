source("R/feature_standardization_functions.R")
source("R/training_functions.R")
source("R/prediction_functions.R")

assert_true <- function(condition, message) {
  if (!isTRUE(condition)) {
    stop(message, call. = FALSE)
  }
}

assert_equal_num <- function(actual, expected, message, tolerance = 1e-10) {
  if (!isTRUE(all.equal(actual, expected, tolerance = tolerance, check.attributes = FALSE))) {
    stop(message, call. = FALSE)
  }
}

run_tests <- function() {
  X <- matrix(
    c(1, 2, 3, 10, 10, 10),
    nrow = 3,
    dimnames = list(NULL, c("mz_1", "mz_2"))
  )

  unchanged <- standardize_feature_matrix(X, method = "none")
  assert_equal_num(unchanged, X, "'none' should leave feature values unchanged.")

  sd_scaled <- standardize_feature_matrix(X, method = "sd", return_params = TRUE)
  assert_equal_num(sd_scaled$data[, "mz_1"], c(1, 2, 3), "'sd' should divide by feature SD.")
  assert_equal_num(sd_scaled$data[, "mz_2"], c(10, 10, 10), "Zero-SD features should use scale 1.")
  assert_equal_num(sd_scaled$scale, c(mz_1 = 1, mz_2 = 1), "SD scales should be stored by feature name.")

  z_scaled <- standardize_feature_matrix(X, method = "zscore", return_params = TRUE)
  assert_equal_num(z_scaled$data[, "mz_1"], c(-1, 0, 1), "'zscore' should center and scale features.")
  assert_equal_num(z_scaled$data[, "mz_2"], c(0, 0, 0), "Constant features should become zero after z-scoring.")
  assert_equal_num(z_scaled$center, c(mz_1 = 2, mz_2 = 10), "Z-score centers should be stored by feature name.")

  new_X <- matrix(
    c(4, 5, 12, 13),
    nrow = 2,
    dimnames = list(NULL, c("mz_1", "mz_2"))
  )
  transformed <- standardize_feature_matrix(
    new_X,
    method = "zscore",
    center = z_scaled$center,
    scale = z_scaled$scale
  )
  assert_equal_num(transformed[, "mz_1"], c(2, 3), "Prediction data should use saved training centers/scales.")
  assert_equal_num(transformed[, "mz_2"], c(2, 3), "Saved zero-SD replacement scale should be reused.")

  df <- data.frame(
    x = c(1, 2, 3),
    y = c(1, 1, 1),
    mz_1 = c(1, 2, 3),
    mz_2 = c(10, 10, 10)
  )
  df_scaled <- standardize_feature_dataframe(df, c("mz_1", "mz_2"), method = "zscore")
  assert_equal_num(df_scaled$x, df$x, "Spatial columns should not be standardised.")
  assert_equal_num(df_scaled$mz_1, c(-1, 0, 1), "Data-frame m/z columns should be standardised.")

  feature_df <- data.frame(
    runNames = c("run1", "run1"),
    x = c(1, 2),
    y = c(1, 1),
    mz_1 = c(4, 5),
    mz_2 = c(12, 13)
  )
  pred_prep <- prepare_prediction_matrix(
    feature_df,
    normalize_method = "none",
    feature_standardize = "zscore",
    feature_center = z_scaled$center,
    feature_scale = z_scaled$scale
  )
  assert_equal_num(pred_prep$X[, "mz_1"], c(2, 3), "Prediction preparation should reuse saved centers/scales.")
  assert_equal_num(pred_prep$X[, "mz_2"], c(2, 3), "Prediction preparation should reuse saved zero-SD replacement scale.")

  cat("Feature standardization tests passed.\n")
}

run_tests()
