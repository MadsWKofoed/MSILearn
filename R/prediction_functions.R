# R/prediction_functions.R

# ---------------------------------------------------------------------------
# Helpers
# ---------------------------------------------------------------------------

list_all_model_runs <- function(db = DB_NAME, url = MONGO_URL) {
  .con("model_runs", db, url)$find(
    "{}",
    fields = '{"_id":1,"dataset_id":1,"model_type":1,"metrics":1,"hyperparams":1,"created_at":1}'
  )
}

get_reference_mz_values <- function(reference_name) {
  vals <- load_alignment_reference(reference_name)$mz_values
  vals <- vals[is.finite(vals)]

  if (length(vals) == 0) {
    stop("Reference list contains no valid m/z values: ", reference_name)
  }

  vals
}

first_scalar <- function(x, default = NA) {
  if (is.null(x) || length(x) == 0) return(default)
  x[[1]]
}

# ---------------------------------------------------------------------------
# Resolve run -> dataset -> pipeline -> params
# ---------------------------------------------------------------------------

get_prediction_context <- function(run_id, db = DB_NAME, url = MONGO_URL) {
  run_row <- get_model_run(run_id, db, url)
  if (is.null(run_row) || nrow(run_row) == 0) {
    stop("Model run not found: ", run_id)
  }

  dataset_id <- as.character(run_row$dataset_id[1])

  ds <- get_dataset(dataset_id, db, url)
  if (is.null(ds) || nrow(ds) == 0) {
    stop("Dataset not found for run: ", dataset_id)
  }

  pipeline_id <- as.character(ds$pipeline_id[1])

  pipe <- get_pipeline(pipeline_id, db, url)
  if (is.null(pipe) || nrow(pipe) == 0) {
    stop("Pipeline not found: ", pipeline_id)
  }

  pipe_params <- extract_params(pipe$params)
  hyperparams <- extract_params(run_row$hyperparams)

  list(
    run_row       = run_row,
    dataset_row   = ds,
    pipeline_row  = pipe,
    dataset_id    = dataset_id,
    pipeline_id   = pipeline_id,
    pipeline_name = as.character(pipe$name[1]),
    pipeline_params = pipe_params,
    hyperparams     = hyperparams
  )
}

# ---------------------------------------------------------------------------
# Process raw uploaded MSI using pipeline tied to selected model run
# ---------------------------------------------------------------------------

process_uploaded_data_for_prediction <- function(imzml_path,
                                                 ibd_path,
                                                 imzml_name,
                                                 pipeline_params,
                                                 db = DB_NAME,
                                                 url = MONGO_URL,
                                                BPPARAM = msi_bpparam) {
  snr        <- as.numeric(first_scalar(pipeline_params$snr))
  tolerance  <- as.numeric(first_scalar(pipeline_params$tolerance))
  resolution <- as.numeric(first_scalar(pipeline_params$resolution, 10))
  ref_name   <- as.character(first_scalar(pipeline_params$reference_name, ""))

  if (!nzchar(ref_name)) stop("Pipeline has no reference_name.")
  ref_mz_values <- get_reference_mz_values(ref_name)

  work_dir <- tempfile("prediction_run_")
  dir.create(work_dir, recursive = TRUE)
  on.exit(unlink(work_dir, recursive = TRUE), add = TRUE)

  base       <- tools::file_path_sans_ext(basename(imzml_name))
  temp_imzml <- file.path(work_dir, paste0(base, ".imzML"))
  temp_ibd   <- file.path(work_dir, paste0(base, ".ibd"))

  ok1 <- file.copy(imzml_path, temp_imzml, overwrite = TRUE)
  ok2 <- file.copy(ibd_path,   temp_ibd,   overwrite = TRUE)
  if (!ok1 || !ok2) {
    stop("Could not copy uploaded files to temporary work directory.")
  }

  message("[predict] Reading MSI data...")
  msi_data <- Cardinal::readMSIData(
    temp_imzml,
    memory     = FALSE,
    check      = FALSE,
    resolution = resolution,
    units      = "ppm",
    guess.max  = 1000L,
    as         = "auto",
    parse.only = FALSE,
    verbose    = Cardinal::getCardinalVerbose(),
    chunkopts  = list(),
    BPPARAM    = BPPARAM
  )

  message("[predict] Computing mean spectrum...")
  control_mean <- Cardinal::summarizeFeatures(msi_data, "mean")

  message("[predict] Peak picking + alignment...")
  control_MSI_ref <- control_mean |>
    Cardinal::peakPick(SNR = snr) |>
    Cardinal::peakAlign(ref = ref_mz_values, tolerance = tolerance, units = "mz") |>
    Cardinal::subsetFeatures() |>
    Cardinal::process()

  message("[predict] Binning full dataset...")
  msi_data_binned <- Cardinal::bin(
    msi_data,
    ref       = Cardinal::mz(control_MSI_ref),
    tolerance = tolerance,
    units     = "mz",
    BPPARAM   = BPPARAM
  ) |>
    Cardinal::process()

  spec_mat <- t(as.matrix(Cardinal::spectra(msi_data_binned)))
  mz_names <- paste0("mz_", Cardinal::mz(msi_data_binned))
  coords   <- Cardinal::coord(msi_data_binned)

  run_vec <- Cardinal::runNames(msi_data_binned)
  if (length(run_vec) == 1L) {
    run_vec <- rep(as.character(run_vec), nrow(spec_mat))
  } else {
    run_vec <- as.character(run_vec)
  }

  full_df <- data.frame(
    runNames = run_vec,
    x        = coords$x,
    y        = coords$y,
    spec_mat,
    check.names = FALSE
  )
  colnames(full_df) <- c("runNames", "x", "y", mz_names)

  full_df
}

# ---------------------------------------------------------------------------
# Prepare feature matrix exactly like training/prediction expects
# ---------------------------------------------------------------------------

prepare_prediction_matrix <- function(feature_df, normalize_method = "none") {
  mz_cols <- grep("^mz_", names(feature_df), value = TRUE)
  if (length(mz_cols) == 0) {
    stop("No mz_ feature columns found in processed prediction data.")
  }

  new_X <- as.matrix(feature_df[, mz_cols, drop = FALSE])

  normalize_method <- as.character(first_scalar(normalize_method, "none"))
  new_X <- normalize_feature_matrix(new_X, method = normalize_method)

  list(
    X = new_X,
    meta = feature_df[, c("runNames", "x", "y"), drop = FALSE]
  )
}

# ---------------------------------------------------------------------------
# End-to-end prediction from selected model run + uploaded raw data
# ---------------------------------------------------------------------------

run_prediction_from_upload <- function(run_id,
                                       imzml_path,
                                       ibd_path,
                                       imzml_name,
                                       db = DB_NAME,
                                       url = MONGO_URL) {
  ctx <- get_prediction_context(run_id, db, url)

  feature_df <- process_uploaded_data_for_prediction(
    imzml_path       = imzml_path,
    ibd_path         = ibd_path,
    imzml_name       = imzml_name,
    pipeline_params  = ctx$pipeline_params,
    db               = db,
    url              = url,
    BPPARAM          = msi_bpparam
  )

  norm_method <- first_scalar(ctx$hyperparams$normalize_method, "none")

  prep <- prepare_prediction_matrix(
    feature_df = feature_df,
    normalize_method = norm_method
  )

  preds <- predict_from_model_run(
    run_id = run_id,
    new_X  = prep$X,
    db     = db,
    url    = url
  )

  prediction_df <- data.frame(
    runNames  = prep$meta$runNames,
    x         = prep$meta$x,
    y         = prep$meta$y,
    Predicted = as.character(preds),
    stringsAsFactors = FALSE
  )

  list(
    prediction_df   = prediction_df,
    feature_df      = feature_df,
    context         = ctx
  )
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

  if (!is.null(fit$coefnames)) {
    train_cols <- fit$coefnames
  } else {
    train_cols <- setdiff(
      colnames(fit$trainingData),
      c(".outcome", ".weights", ".rowIndex")
    )
  }

  missing_cols <- setdiff(train_cols, colnames(new_X))
  extra_cols   <- setdiff(colnames(new_X), train_cols)

  if (length(missing_cols) > 0) {
    stop("[predict] New data is missing columns present during training: ",
         paste(missing_cols, collapse = ", "))
  }

  if (length(extra_cols) > 0) {
    message("[predict] Extra columns in new_X will be dropped: ",
            paste(extra_cols, collapse = ", "))
  }

  new_X <- new_X[, train_cols, drop = FALSE]

  preds <- predict(fit, newdata = new_X)
  message("[predict] Predicted ", length(preds), " pixels.")
  preds
}
