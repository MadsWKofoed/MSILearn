source("R/mongo_schema.R")
source("R/mongo_functions.R")
source("R/alignment_reference_db.R")
source("R/prediction_functions.R")
source("R/database_management_functions.R")

assert_true <- function(condition, message) {
  if (!isTRUE(condition)) {
    stop(message, call. = FALSE)
  }
}

make_fake_db_state <- function() {
  docs <- list(
    studies = list(
      list(`_id` = "study_1", name = "Study One", description = "Test study", created_at = "2026-05-02T10:00:00Z")
    ),
    samples = list(
      list(`_id` = "sample_1", study_id = "study_1", sample_name = "Sample A", created_at = "2026-05-02T10:01:00Z"),
      list(`_id` = "sample_2", study_id = "study_1", sample_name = "Sample B", created_at = "2026-05-02T10:02:00Z")
    ),
    pipelines = list(
      list(`_id` = "pipe_processing", type = "processing", name = "Processing", params_hash = "pipe_processing", created_at = "2026-05-02T10:03:00Z"),
      list(`_id` = "pipe_clustering", type = "clustering", name = "Clustering", params_hash = "pipe_clustering", created_at = "2026-05-02T10:04:00Z")
    ),
    pipeline_outputs = list(
      list(`_id` = "pipeline_output_1", study_id = "study_1", sample_id = "sample_1", pipeline_id = "pipe_processing", stage_type = "binned_dataframe", gridfs_name = "pipeline_output_1.rds", created_at = "2026-05-02T10:05:00Z"),
      list(`_id` = "pipeline_output_2", study_id = "study_1", sample_id = "sample_2", pipeline_id = "pipe_processing", stage_type = "binned_dataframe", gridfs_name = "pipeline_output_2.rds", created_at = "2026-05-02T10:06:00Z"),
      list(`_id` = "pipeline_output_cluster", study_id = "study_1", sample_id = "sample_1", pipeline_id = "pipe_clustering", stage_type = "clustering_result", gridfs_name = "pipeline_output_cluster.rds", input_pipeline_output_id = "pipeline_output_1", created_at = "2026-05-02T10:07:00Z")
    ),
    annotation_sets = list(
      list(`_id` = "annset_1", study_id = "study_1", name = "Regions", created_at = "2026-05-02T10:08:00Z")
    ),
    annotations = list(
      list(`_id` = "annotation_1", sample_id = "sample_1", annotation_set_id = "annset_1", gridfs_name = "annotation_1.rds", created_at = "2026-05-02T10:09:00Z"),
      list(`_id` = "annotation_2", sample_id = "sample_2", annotation_set_id = "annset_1", gridfs_name = "annotation_2.rds", created_at = "2026-05-02T10:10:00Z")
    ),
    datasets = list(
      list(`_id` = "dataset_1", study_id = "study_1", name = "Dataset One", sample_ids = list("sample_1", "sample_2"), pipeline_id = "pipe_processing", annotation_set_id = "annset_1", stage_type = "binned_dataframe", created_at = "2026-05-02T10:11:00Z")
    ),
    model_runs = list(
      list(`_id` = "run_1", dataset_id = "dataset_1", model_type = "rf", model_gridfs = "run_1_model.rds", created_at = "2026-05-02T10:12:00Z")
    ),
    alignment_references = list(
      list(`_id` = "builtin_ref", reference_name = "builtin_ref", display_name = "Built-in", file_type = "csv", file_size = 10, date_added = "2026-05-01T00:00:00Z", built_in = TRUE, source = "built_in", description = "Protected"),
      list(`_id` = "uploaded_ref", reference_name = "uploaded_ref", display_name = "Uploaded", file_type = "csv", file_size = 10, date_added = "2026-05-01T00:00:00Z", built_in = FALSE, source = "uploaded", description = "User upload")
    ),
    ndpi_registrations = list(
      list(sample_id = "sample_1", pipeline_id = "pipe_processing", ndpi_slide_name = "slide_a.ndpi", created_at = "2026-05-02T10:13:00Z")
    ),
    processing_pipeline_outputs_metadata = list(
      list(sample_name = "Sample A", stage_type = "raw_files", imzml_gridfs_name = "sample_a.imzML", ibd_gridfs_name = "sample_a.ibd", created_at = "2026-05-02T10:14:00Z")
    ),
    clustering_metadata = list(
      list(sample_id = "sample_1", pipeline_id = "pipe_clustering", created_at = "2026-05-02T10:15:00Z")
    )
  )

  state <- new.env(parent = emptyenv())
  state$docs <- docs
  state$gridfs_removed <- character(0)
  state
}

doc_to_row <- function(doc) {
  out <- list()
  for (nm in names(doc)) {
    val <- doc[[nm]]
    if (is.null(val)) {
      next
    }

    if (is.atomic(val) && length(val) == 1) {
      out[[nm]] <- val
    } else {
      out[[nm]] <- I(list(val))
    }
  }
  as.data.frame(out, stringsAsFactors = FALSE, check.names = FALSE)
}

docs_to_df <- function(docs) {
  if (length(docs) == 0) {
    return(data.frame(stringsAsFactors = FALSE))
  }
  rows <- lapply(docs, doc_to_row)
  dplyr::bind_rows(rows)
}

query_to_list <- function(query) {
  if (is.null(query) || identical(query, "{}") || !nzchar(query)) {
    return(list())
  }
  jsonlite::fromJSON(query, simplifyVector = FALSE)
}

matches_query <- function(doc, query) {
  if (length(query) == 0) {
    return(TRUE)
  }

  for (field in names(query)) {
    expected <- query[[field]]
    actual <- doc[[field]]
    actual_vals <- dbm_nonempty_chr(actual)

    if (is.list(expected) && !is.null(expected[["$in"]])) {
      expected_vals <- dbm_nonempty_chr(expected[["$in"]])
      if (length(intersect(actual_vals, expected_vals)) == 0) {
        return(FALSE)
      }
    } else {
      expected_val <- dbm_nonempty_chr(expected)[1] %||% ""
      actual_val <- actual_vals[1] %||% ""
      if (!identical(actual_val, expected_val)) {
        return(FALSE)
      }
    }
  }

  TRUE
}

make_fake_collection <- function(state, collection_name) {
  collection <- new.env(parent = emptyenv())

  collection$find <- function(query = "{}", fields = NULL) {
    q <- query_to_list(query)
    docs <- Filter(function(doc) matches_query(doc, q), state$docs[[collection_name]] %||% list())
    docs_to_df(docs)
  }

  collection$count <- function(query = "{}") {
    nrow(collection$find(query))
  }

  collection$remove <- function(query = "{}") {
    q <- query_to_list(query)
    keep <- vapply(state$docs[[collection_name]] %||% list(), function(doc) !matches_query(doc, q), logical(1))
    state$docs[[collection_name]] <- (state$docs[[collection_name]] %||% list())[keep]
    invisible(TRUE)
  }

  collection$index <- function(spec) invisible(TRUE)
  collection
}

with_fake_db <- function(code) {
  state <- make_fake_db_state()
  original_con <- .con
  original_gridfs <- .gridfs

  .con <<- function(collection, db = DB_NAME, url = MONGO_URL) {
    make_fake_collection(state, collection)
  }

  .gridfs <<- function(db = DB_NAME, url = MONGO_URL) {
    grid <- new.env(parent = emptyenv())
    grid$remove <- function(filename) {
      state$gridfs_removed <- c(state$gridfs_removed, as.character(filename))
      invisible(TRUE)
    }
    grid
  }

  on.exit({
    .con <<- original_con
    .gridfs <<- original_gridfs
  }, add = TRUE)

  force(code)
}

run_tests <- function() {
  with_fake_db({
    study <- dbm_safe_find_list("studies", list(`_id` = "study_1"))
    report <- dbm_delete_record("studies", record = study)
    assert_true(as.integer(report$deleted[["studies"]]) == 1L, "Study deletion should remove the selected study.")
    assert_true(as.integer(report$deleted[["samples"]]) == 2L, "Study deletion should remove linked samples.")
    assert_true(as.integer(report$deleted[["pipeline_outputs"]]) == 3L, "Study deletion should remove linked Pipeline Outputs, including clustering results.")
    assert_true(as.integer(report$deleted[["datasets"]]) == 1L, "Study deletion should remove linked datasets.")
    assert_true(as.integer(report$deleted[["model_runs"]]) == 1L, "Study deletion should remove linked model runs.")
    assert_true(dbm_safe_count("studies") == 0L, "Study should be deleted.")
    assert_true(dbm_safe_count("samples") == 0L, "Samples should be deleted with the study.")
    assert_true(nrow(dbm_filter_index("samples")) == 0L, "Samples from a deleted study should not remain in the filter index.")
  })

  with_fake_db({
    sample <- dbm_safe_find_list("samples", list(`_id` = "sample_1"))
    report <- dbm_delete_record("samples", record = sample)
    assert_true(as.integer(report$deleted[["samples"]]) == 1L, "Sample deletion should remove the selected sample.")
    assert_true(as.integer(report$deleted[["datasets"]]) == 1L, "Sample deletion should remove dependent datasets.")
    assert_true(as.integer(report$deleted[["model_runs"]]) == 1L, "Sample deletion should remove dependent model runs.")
    remaining_samples <- dbm_safe_find("samples")
    assert_true(nrow(remaining_samples) == 1L && identical(as.character(remaining_samples$`_id`[1]), "sample_2"), "Deleting one sample should leave unrelated samples untouched.")
  })

  with_fake_db({
    dataset <- dbm_safe_find_list("datasets", list(`_id` = "dataset_1"))
    report <- dbm_delete_record("datasets", record = dataset)
    assert_true(as.integer(report$deleted[["datasets"]]) == 1L, "Dataset deletion should remove the dataset.")
    assert_true(as.integer(report$deleted[["model_runs"]]) == 1L, "Dataset deletion should remove dependent model runs.")
    assert_true(dbm_safe_count("annotations") == 2L, "Deleting a dataset should not delete source annotations.")
  })

  with_fake_db({
    annotation <- dbm_safe_find_list("annotations", list(`_id` = "annotation_1"))
    report <- dbm_delete_record("annotations", record = annotation)
    assert_true(as.integer(report$deleted[["annotations"]]) == 1L, "Annotation deletion should remove the selected annotation.")
    assert_true(as.integer(report$deleted[["datasets"]]) == 1L, "Annotation deletion should remove dependent datasets.")
    assert_true(as.integer(report$deleted[["model_runs"]]) == 1L, "Annotation deletion should remove dependent model runs.")
  })

  with_fake_db({
    model_run <- dbm_safe_find_list("model_runs", list(`_id` = "run_1"))
    report <- dbm_delete_record("model_runs", record = model_run)
    assert_true(as.integer(report$deleted[["model_runs"]]) == 1L, "Model deletion should remove the selected model run.")
    assert_true(dbm_safe_count("datasets") == 1L, "Deleting a model run should not delete its dataset.")
  })

  with_fake_db({
    clustering_pipeline_output <- dbm_safe_find_list("pipeline_outputs", list(`_id` = "pipeline_output_cluster"))
    report <- dbm_delete_record("pipeline_outputs", record = clustering_pipeline_output)
    assert_true(as.integer(report$deleted[["pipeline_outputs"]]) == 1L, "Clustering result deletion should remove the selected Pipeline Output.")
    assert_true(dbm_safe_count("datasets") == 1L, "Deleting a clustering result should not delete unrelated datasets.")
  })

  with_fake_db({
    uploaded_ref <- dbm_safe_find_list("alignment_references", list(`_id` = "uploaded_ref"))
    report <- dbm_delete_record("alignment_references", record = uploaded_ref)
    assert_true(as.integer(report$deleted[["alignment_references"]]) == 1L, "Uploaded alignment references should be deletable.")
    assert_true(dbm_safe_count("alignment_references") == 1L, "Only the built-in alignment reference should remain after deleting the upload.")
  })

  with_fake_db({
    builtin_ref <- dbm_safe_find_list("alignment_references", list(`_id` = "builtin_ref"))
    report <- dbm_delete_record("alignment_references", record = builtin_ref)
    assert_true(report$total_deleted == 0L, "Built-in alignment references must not be deleted.")
    assert_true(grepl("protected", report$reason), "Built-in alignment reference report should explain why deletion was blocked.")
    assert_true(dbm_safe_count("alignment_references") == 2L, "Protected alignment references should remain in the database.")
  })

  with_fake_db({
    report <- dbm_delete_record("studies", id = "missing_study")
    assert_true(report$total_deleted == 0L, "Deleting a missing object should not delete anything.")
    assert_true(grepl("no longer exists", report$reason), "Deleting a missing object should give a clear explanation.")
  })

  cat("Database management deletion tests passed.\n")
}

run_tests()
