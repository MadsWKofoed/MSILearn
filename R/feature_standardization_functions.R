# R/feature_standardization_functions.R

# Feature-wise standardisation for mz_ matrices.
# Rows are pixels/spectra, columns are m/z features.
standardize_feature_matrix <- function(X,
                                       method = c("none", "sd", "zscore"),
                                       center = NULL,
                                       scale = NULL,
                                       na.rm = TRUE,
                                       return_params = FALSE) {
  method <- match.arg(method)

  X <- as.matrix(X)
  storage.mode(X) <- "numeric"

  feature_names <- colnames(X)
  if (is.null(feature_names)) {
    feature_names <- paste0("feature_", seq_len(ncol(X)))
    colnames(X) <- feature_names
  }

  align_param <- function(values, default, param_name) {
    if (is.null(values)) {
      return(stats::setNames(rep(default, length(feature_names)), feature_names))
    }

    values <- as.numeric(values)
    if (!is.null(names(values)) && any(nzchar(names(values)))) {
      values <- values[feature_names]
    }

    if (length(values) != length(feature_names) || anyNA(values)) {
      stop(param_name, " must contain one value for each feature column.")
    }

    stats::setNames(values, feature_names)
  }

  if (method == "none") {
    params <- list(
      method = method,
      center = align_param(center, 0, "center"),
      scale = align_param(scale, 1, "scale")
    )
    return(if (isTRUE(return_params)) c(list(data = X), params) else X)
  }

  center_values <- if (method == "zscore" && is.null(center)) {
    stats::setNames(colMeans(X, na.rm = na.rm), feature_names)
  } else if (method == "zscore") {
    align_param(center, 0, "center")
  } else {
    stats::setNames(rep(0, length(feature_names)), feature_names)
  }

  scale_values <- if (is.null(scale)) {
    stats::setNames(apply(X, 2L, stats::sd, na.rm = na.rm), feature_names)
  } else {
    align_param(scale, 1, "scale")
  }

  center_values[!is.finite(center_values)] <- 0
  scale_values[!is.finite(scale_values) | scale_values == 0] <- 1

  X_std <- X
  if (method == "zscore") {
    X_std <- sweep(X_std, 2L, center_values, "-")
  }
  X_std <- sweep(X_std, 2L, scale_values, "/")
  X_std[!is.finite(X_std)] <- 0

  colnames(X_std) <- feature_names
  rownames(X_std) <- rownames(X)

  params <- list(
    method = method,
    center = center_values,
    scale = scale_values
  )

  if (isTRUE(return_params)) c(list(data = X_std), params) else X_std
}

standardize_feature_dataframe <- function(data,
                                          signal_cols,
                                          method = c("none", "sd", "zscore"),
                                          center = NULL,
                                          scale = NULL,
                                          return_params = FALSE) {
  method <- match.arg(method)
  signal_cols <- intersect(signal_cols, names(data))
  if (length(signal_cols) == 0) {
    stop("standardize_feature_dataframe(): 'signal_cols' matches no columns in data.")
  }

  standardized <- standardize_feature_matrix(
    data[, signal_cols, drop = FALSE],
    method = method,
    center = center,
    scale = scale,
    return_params = TRUE
  )

  data[, signal_cols] <- as.data.frame(standardized$data, check.names = FALSE)

  out <- list(
    data = data,
    method = standardized$method,
    center = standardized$center,
    scale = standardized$scale
  )

  if (isTRUE(return_params)) out else data
}
