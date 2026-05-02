source("R/processing_functions.R")

assert_true <- function(condition, message) {
  if (!isTRUE(condition)) {
    stop(message, call. = FALSE)
  }
}

make_upload_info <- function(paths) {
  data.frame(
    name = basename(paths),
    datapath = paths,
    stringsAsFactors = FALSE
  )
}

run_tests <- function() {
  if (!requireNamespace("CardinalIO", quietly = TRUE)) {
    stop("CardinalIO is required for raw pair validation tests.", call. = FALSE)
  }

  tmp_dir <- tempfile("raw_pair_validation_tests_")
  dir.create(tmp_dir, recursive = TRUE)
  on.exit(unlink(tmp_dir, recursive = TRUE, force = TRUE), add = TRUE)

  continuous_dir <- system.file("extdata/Example_Continuous_imzML1.1.1", package = "CardinalIO")
  processed_dir <- system.file("extdata/Example_Processed_imzML1.1.1", package = "CardinalIO")

  assert_true(nzchar(continuous_dir), "Continuous CardinalIO example should be available.")
  assert_true(nzchar(processed_dir), "Processed CardinalIO example should be available.")

  continuous_imzml <- list.files(continuous_dir, pattern = "\\.imzML$", full.names = TRUE, ignore.case = TRUE)[1]
  continuous_ibd <- list.files(continuous_dir, pattern = "\\.ibd$", full.names = TRUE, ignore.case = TRUE)[1]
  processed_ibd <- list.files(processed_dir, pattern = "\\.ibd$", full.names = TRUE, ignore.case = TRUE)[1]

  assert_true(file.exists(continuous_imzml), "Continuous example should contain an imzML file.")
  assert_true(file.exists(continuous_ibd), "Continuous example should contain an ibd file.")
  assert_true(file.exists(processed_ibd), "Processed example should contain an ibd file.")

  valid_dir <- file.path(tmp_dir, "valid")
  dir.create(valid_dir)
  valid_imzml <- file.copy(continuous_imzml, file.path(valid_dir, basename(continuous_imzml)), overwrite = TRUE)
  valid_ibd <- file.copy(continuous_ibd, file.path(valid_dir, basename(continuous_ibd)), overwrite = TRUE)
  assert_true(isTRUE(valid_imzml) && isTRUE(valid_ibd), "Valid example files should copy into the temp directory.")

  valid_upload <- make_upload_info(file.path(valid_dir, c(basename(continuous_imzml), basename(continuous_ibd))))
  valid_result <- validate_uploaded_raw_pair(valid_upload)
  assert_true(isTRUE(valid_result$valid), "Matching imzML and ibd files should validate successfully.")

  missing_pair <- validate_uploaded_raw_pair(valid_upload[1, , drop = FALSE], probe_pair = FALSE)
  assert_true(!isTRUE(missing_pair$valid), "A single uploaded file should be rejected.")

  mismatched_name_dir <- file.path(tmp_dir, "mismatched_name")
  dir.create(mismatched_name_dir)
  renamed_ibd <- file.path(mismatched_name_dir, "Different_Name.ibd")
  copied_renamed_ibd <- file.copy(continuous_ibd, renamed_ibd, overwrite = TRUE)
  copied_name_imzml <- file.copy(continuous_imzml, file.path(mismatched_name_dir, basename(continuous_imzml)), overwrite = TRUE)
  assert_true(isTRUE(copied_renamed_ibd) && isTRUE(copied_name_imzml), "Mismatched-name fixture files should copy into the temp directory.")

  mismatched_name_upload <- make_upload_info(c(
    file.path(mismatched_name_dir, basename(continuous_imzml)),
    renamed_ibd
  ))
  mismatched_name_result <- validate_uploaded_raw_pair(mismatched_name_upload, probe_pair = FALSE)
  assert_true(!isTRUE(mismatched_name_result$valid), "Files with different base names should be rejected.")

  mismatched_pair_dir <- file.path(tmp_dir, "mismatched_pair")
  dir.create(mismatched_pair_dir)
  staged_imzml <- file.path(mismatched_pair_dir, basename(continuous_imzml))
  staged_ibd <- file.path(
    mismatched_pair_dir,
    paste0(tools::file_path_sans_ext(basename(continuous_imzml)), ".ibd")
  )
  copied_pair_imzml <- file.copy(continuous_imzml, staged_imzml, overwrite = TRUE)
  copied_pair_ibd <- file.copy(processed_ibd, staged_ibd, overwrite = TRUE)
  assert_true(isTRUE(copied_pair_imzml) && isTRUE(copied_pair_ibd), "Mismatched-pair fixture files should copy into the temp directory.")

  mismatched_pair_upload <- make_upload_info(c(staged_imzml, staged_ibd))
  mismatched_pair_result <- validate_uploaded_raw_pair(mismatched_pair_upload)
  assert_true(!isTRUE(mismatched_pair_result$valid), "Same-name files with different imzML/ibd contents should be rejected.")

  cat("Raw pair validation tests passed.\n")
}

run_tests()
