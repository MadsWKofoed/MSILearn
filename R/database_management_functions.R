# R/database_management_functions.R
# Helpers for browsing and deleting records from the MSI platform database.
# This file is designed to sit on top of mongo_functions.R and mongo_schema.R.

library(jsonlite)
library(mongolite)
library(dplyr)

# ---- Collection catalog ----------------------------------------------------

dbm_catalog <- function() {
  data.frame(
    key = c(
      "studies", "samples", "pipelines", "artifacts",
      "annotation_sets", "annotations", "datasets", "model_runs",
      "processing_artifacts_metadata", "clustering_metadata", "ndpi_registrations"
    ),
    label = c(
      "Studies", "Samples", "Pipelines", "Artifacts",
      "Annotation sets", "Annotations", "Datasets", "Model runs",
      "Legacy processing metadata", "Legacy clustering metadata", "NDPI registrations"
    ),
    description = c(
      "Top-level projects/cohorts.",
      "Samples registered under studies.",
      "Deterministic processing/clustering pipelines.",
      "Stored artifacts backed by GridFS.",
      "Allowed label vocabularies.",
      "Pixel-level annotations backed by GridFS.",
      "Frozen training datasets.",
      "Persisted trained models backed by GridFS.",
      "Legacy raw/stage metadata collection.",
      "Legacy clustering commit metadata.",
      "Saved NDPI→MSI registration metadata."
    ),
    stringsAsFactors = FALSE
  )
}

dbm_deleteable_collections <- function() dbm_catalog()$key

dbm_requires_study_filter <- function(collection) {
  collection %in% c("samples", "annotation_sets")
}

# ---- Small generic helpers -------------------------------------------------

dbm_null_to_na <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA)
  x[[1]]
}

dbm_first_chr <- function(x, default = "—") {
  if (is.null(x) || length(x) == 0) return(default)
  out <- as.character(x[[1]])
  if (!nzchar(out)) default else out
}

dbm_safe_find <- function(collection, query = "{}", fields = NULL,
                          db = DB_NAME, url = MONGO_URL) {
  con <- .con(collection, db, url)
  tryCatch(
    {
      if (is.null(fields)) con$find(query) else con$find(query, fields = fields)
    },
    error = function(e) data.frame(stringsAsFactors = FALSE)
  )
}

dbm_safe_count <- function(collection, query = "{}", db = DB_NAME, url = MONGO_URL) {
  tryCatch(.con(collection, db, url)$count(query), error = function(e) 0L)
}

dbm_query_from_list <- function(lst) {
  if (length(lst) == 0) return("{}")
  jsonlite::toJSON(lst, auto_unbox = TRUE, null = "null")
}

dbm_in_query <- function(field, values) {
  values <- unique(as.character(values))
  values <- values[nzchar(values)]
  if (length(values) == 0) return("{}")
  jsonlite::toJSON(setNames(list(list(`$in` = as.list(values))), field), auto_unbox = TRUE)
}

dbm_remove_gridfs_file <- function(filename, db = DB_NAME, url = MONGO_URL) {
  if (is.null(filename) || !length(filename)) return(invisible(FALSE))
  filename <- as.character(filename[[1]])
  if (!nzchar(filename)) return(invisible(FALSE))
  tryCatch({
    .gridfs(db, url)$remove(filename)
    TRUE
  }, error = function(e) FALSE)
}

dbm_candidate_file_fields <- function(df) {
  intersect(
    c("gridfs_name", "model_gridfs", "imzml_gridfs_name", "ibd_gridfs_name", "filename"),
    names(df)
  )
}

dbm_delete_docs_with_files <- function(collection, query = "{}", db = DB_NAME, url = MONGO_URL) {
  con <- .con(collection, db, url)
  docs <- tryCatch(con$find(query), error = function(e) data.frame(stringsAsFactors = FALSE))
  n_docs <- if (is.null(docs) || nrow(docs) == 0) 0L else nrow(docs)

  if (n_docs > 0) {
    file_fields <- dbm_candidate_file_fields(docs)
    if (length(file_fields) > 0) {
      for (ff in file_fields) {
        vals <- docs[[ff]]
        for (i in seq_along(vals)) {
          dbm_remove_gridfs_file(vals[[i]], db = db, url = url)
        }
      }
    }
    tryCatch(con$remove(query), error = function(e) NULL)
  }

  n_docs
}

# ---- Lookup maps for human-readable labels --------------------------------

dbm_study_map <- function(db = DB_NAME, url = MONGO_URL) {
  df <- tryCatch(get_studies(db, url), error = function(e) data.frame())
  if (nrow(df) == 0 || !all(c("_id", "name") %in% names(df))) return(setNames(character(0), character(0)))
  setNames(as.character(df$name), as.character(df$`_id`))
}

dbm_sample_map <- function(db = DB_NAME, url = MONGO_URL) {
  df <- dbm_safe_find("samples", db = db, url = url)
  if (nrow(df) == 0 || !all(c("_id", "sample_name") %in% names(df))) return(setNames(character(0), character(0)))
  setNames(as.character(df$sample_name), as.character(df$`_id`))
}

dbm_pipeline_map <- function(db = DB_NAME, url = MONGO_URL) {
  df <- dbm_safe_find("pipelines", db = db, url = url)
  if (nrow(df) == 0 || !all(c("_id", "name") %in% names(df))) return(setNames(character(0), character(0)))
  setNames(as.character(df$name), as.character(df$`_id`))
}

dbm_annset_map <- function(db = DB_NAME, url = MONGO_URL) {
  df <- dbm_safe_find("annotation_sets", db = db, url = url)
  if (nrow(df) == 0 || !all(c("_id", "name") %in% names(df))) return(setNames(character(0), character(0)))
  setNames(as.character(df$name), as.character(df$`_id`))
}

dbm_dataset_map <- function(db = DB_NAME, url = MONGO_URL) {
  df <- tryCatch(list_datasets(db = db, url = url), error = function(e) data.frame())
  if (nrow(df) == 0 || !all(c("_id", "name") %in% names(df))) return(setNames(character(0), character(0)))
  nm <- ifelse(is.na(df$name) | !nzchar(df$name), as.character(df$`_id`), as.character(df$name))
  setNames(nm, as.character(df$`_id`))
}

# ---- Data retrieval for the management table -------------------------------

dbm_get_collection_data <- function(collection,
                                    study_id = NULL,
                                    sample_id = NULL,
                                    db = DB_NAME,
                                    url = MONGO_URL) {
  stopifnot(collection %in% dbm_catalog()$key)

  sample_name <- NULL
  if (!is.null(sample_id) && nzchar(sample_id)) {
    srow <- dbm_safe_find("samples", sprintf('{"_id": "%s"}', sample_id), db = db, url = url)
    if (nrow(srow) > 0 && "sample_name" %in% names(srow)) {
      sample_name <- as.character(srow$sample_name[1])
    }
  }

  out <- switch(
    collection,
    studies = dbm_safe_find("studies", db = db, url = url),
    samples = {
      if (!is.null(study_id) && nzchar(study_id)) get_samples(study_id, db = db, url = url) else dbm_safe_find("samples", db = db, url = url)
    },
    pipelines = dbm_safe_find("pipelines", db = db, url = url),
    artifacts = {
      query_artifacts(study_id = if (!is.null(study_id) && nzchar(study_id)) study_id else NULL,
                      sample_id = if (!is.null(sample_id) && nzchar(sample_id)) sample_id else NULL,
                      db = db, url = url)
    },
    annotation_sets = {
      if (!is.null(study_id) && nzchar(study_id)) list_annotation_sets(study_id, db = db, url = url) else dbm_safe_find("annotation_sets", db = db, url = url)
    },
    annotations = {
      q <- list()
      if (!is.null(sample_id) && nzchar(sample_id)) q$sample_id <- sample_id
      dbm_safe_find("annotations", dbm_query_from_list(q), db = db, url = url)
    },
    datasets = {
      if (!is.null(study_id) && nzchar(study_id)) list_datasets(study_id, db = db, url = url) else list_datasets(db = db, url = url)
    },
    model_runs = list_all_model_runs(db = db, url = url),
    processing_artifacts_metadata = {
      q <- list()
      if (!is.null(sample_name) && nzchar(sample_name)) q$sample_name <- sample_name
      dbm_safe_find("processing_artifacts_metadata", dbm_query_from_list(q), db = db, url = url)
    },
    clustering_metadata = {
      q <- list()
      if (!is.null(study_id) && nzchar(study_id)) q$study_id <- study_id
      if (!is.null(sample_id) && nzchar(sample_id)) q$sample_id <- sample_id
      dbm_safe_find("clustering_metadata", dbm_query_from_list(q), db = db, url = url)
    },
    ndpi_registrations = {
      q <- list()
      if (!is.null(sample_id) && nzchar(sample_id)) q$sample_id <- sample_id
      dbm_safe_find("ndpi_registrations", dbm_query_from_list(q), db = db, url = url)
    }
  )

  if (is.null(out) || nrow(out) == 0) {
    return(data.frame(stringsAsFactors = FALSE))
  }

  out
}

# ---- Display shaping -------------------------------------------------------

dbm_prepare_display <- function(df, collection, db = DB_NAME, url = MONGO_URL) {
  if (is.null(df) || nrow(df) == 0) return(data.frame(stringsAsFactors = FALSE))

  study_map <- dbm_study_map(db, url)
  sample_map <- dbm_sample_map(db, url)
  pipe_map <- dbm_pipeline_map(db, url)
  annset_map <- dbm_annset_map(db, url)
  dataset_map <- dbm_dataset_map(db, url)

  lookup <- function(ids, mp) {
    ids <- as.character(ids)
    out <- unname(mp[ids])
    out[is.na(out) | !nzchar(out)] <- ids[is.na(out) | !nzchar(out)]
    out
  }

  show <- data.frame(stringsAsFactors = FALSE)

  switch(
    collection,
    studies = {
      show <- data.frame(
        id = as.character(df$`_id`),
        name = as.character(df$name %||% ""),
        description = if ("description" %in% names(df)) as.character(df$description) else "",
        created_at = if ("created_at" %in% names(df)) as.character(df$created_at) else "",
        stringsAsFactors = FALSE
      )
    },
    samples = {
      show <- data.frame(
        id = as.character(df$`_id`),
        sample = as.character(df$sample_name),
        study = lookup(df$study_id, study_map),
        created_at = if ("created_at" %in% names(df)) as.character(df$created_at) else "",
        stringsAsFactors = FALSE
      )
    },
    pipelines = {
      show <- data.frame(
        id = as.character(df$`_id`),
        type = as.character(df$type %||% ""),
        name = as.character(df$name %||% ""),
        code_version = if ("code_version" %in% names(df)) as.character(df$code_version) else "",
        created_at = if ("created_at" %in% names(df)) as.character(df$created_at) else "",
        stringsAsFactors = FALSE
      )
    },
    artifacts = {
      show <- data.frame(
        id = as.character(df$`_id`),
        study = lookup(df$study_id, study_map),
        sample = lookup(df$sample_id, sample_map),
        stage = as.character(df$stage_type),
        pipeline = lookup(df$pipeline_id, pipe_map),
        created_at = if ("created_at" %in% names(df)) as.character(df$created_at) else "",
        stringsAsFactors = FALSE
      )
    },
    annotation_sets = {
      show <- data.frame(
        id = as.character(df$`_id`),
        name = as.character(df$name),
        study = lookup(df$study_id, study_map),
        created_at = if ("created_at" %in% names(df)) as.character(df$created_at) else "",
        stringsAsFactors = FALSE
      )
    },
    annotations = {
      show <- data.frame(
        id = as.character(df$`_id`),
        sample = lookup(df$sample_id, sample_map),
        annotation_set = lookup(df$annotation_set_id, annset_map),
        format = if ("format" %in% names(df)) as.character(df$format) else "",
        created_at = if ("created_at" %in% names(df)) as.character(df$created_at) else "",
        stringsAsFactors = FALSE
      )
    },
    datasets = {
      show <- data.frame(
        id = as.character(df$`_id`),
        name = as.character(df$name %||% ""),
        study = lookup(df$study_id, study_map),
        pipeline = lookup(df$pipeline_id, pipe_map),
        annotation_set = lookup(df$annotation_set_id, annset_map),
        stage = as.character(df$stage_type),
        created_at = if ("created_at" %in% names(df)) as.character(df$created_at) else "",
        stringsAsFactors = FALSE
      )
    },
    model_runs = {
      show <- data.frame(
        id = as.character(df$`_id`),
        dataset = lookup(df$dataset_id, dataset_map),
        model_type = as.character(df$model_type),
        created_at = if ("created_at" %in% names(df)) as.character(df$created_at) else "",
        stringsAsFactors = FALSE
      )
    },
    processing_artifacts_metadata = {
      show <- data.frame(
        id = if ("_id" %in% names(df)) as.character(df$`_id`) else if ("run_id" %in% names(df)) as.character(df$run_id) else seq_len(nrow(df)),
        sample = as.character(df$sample_name %||% ""),
        stage = as.character(df$stage_type %||% ""),
        filename = if ("filename" %in% names(df)) as.character(df$filename) else if ("imzml_gridfs_name" %in% names(df)) as.character(df$imzml_gridfs_name) else "",
        created_at = if ("created_at" %in% names(df)) as.character(df$created_at) else "",
        stringsAsFactors = FALSE
      )
    },
    clustering_metadata = {
      show <- data.frame(
        id = if ("_id" %in% names(df)) as.character(df$`_id`) else seq_len(nrow(df)),
        study = lookup(df$study_id, study_map),
        sample = lookup(df$sample_id, sample_map),
        method = as.character(df$clustering_method %||% ""),
        k = as.character(df$k %||% ""),
        created_at = if ("created_at" %in% names(df)) as.character(df$created_at) else "",
        stringsAsFactors = FALSE
      )
    },
    ndpi_registrations = {
      show <- data.frame(
        id = if ("_id" %in% names(df)) as.character(df$`_id`) else seq_len(nrow(df)),
        sample = lookup(df$sample_id, sample_map),
        pipeline = lookup(df$pipeline_id, pipe_map),
        slide = as.character(df$ndpi_slide_name %||% ""),
        rms = as.character(df$rms %||% ""),
        created_at = if ("created_at" %in% names(df)) as.character(df$created_at) else "",
        stringsAsFactors = FALSE
      )
    }
  )

  show
}

# ---- One-record fetch for details pane ------------------------------------

dbm_fetch_record <- function(collection, id, db = DB_NAME, url = MONGO_URL) {
  if (is.null(id) || !nzchar(id)) return(NULL)
  q <- if (collection %in% c("studies", "samples", "pipelines", "artifacts",
                             "annotation_sets", "annotations", "datasets", "model_runs")) {
    sprintf('{"_id": "%s"}', id)
  } else {
    # legacy collections may not always have _id in old records
    sprintf('{"_id": "%s"}', id)
  }
  df <- dbm_safe_find(collection, q, db = db, url = url)
  if (nrow(df) > 0) return(df[1, , drop = FALSE])

  if (collection == "processing_artifacts_metadata") {
    df <- dbm_safe_find(collection, sprintf('{"run_id": "%s"}', id), db = db, url = url)
    if (nrow(df) > 0) return(df[1, , drop = FALSE])
  }
  NULL
}

# ---- Overview counts -------------------------------------------------------

dbm_collection_counts <- function(db = DB_NAME, url = MONGO_URL) {
  cat <- dbm_catalog()
  counts <- vapply(cat$key, function(k) dbm_safe_count(k, db = db, url = url), numeric(1))
  data.frame(
    collection = cat$label,
    key = cat$key,
    records = counts,
    description = cat$description,
    stringsAsFactors = FALSE
  )
}

# ---- Deletion helpers ------------------------------------------------------

dbm_delete_artifacts_by_sample_ids <- function(sample_ids, db = DB_NAME, url = MONGO_URL) {
  q <- dbm_in_query("sample_id", sample_ids)
  dbm_delete_docs_with_files("artifacts", q, db = db, url = url)
}

dbm_delete_annotations_by_sample_ids <- function(sample_ids, db = DB_NAME, url = MONGO_URL) {
  q <- dbm_in_query("sample_id", sample_ids)
  dbm_delete_docs_with_files("annotations", q, db = db, url = url)
}

dbm_delete_model_runs_by_dataset_ids <- function(dataset_ids, db = DB_NAME, url = MONGO_URL) {
  q <- dbm_in_query("dataset_id", dataset_ids)
  dbm_delete_docs_with_files("model_runs", q, db = db, url = url)
}

dbm_delete_datasets_by_ids <- function(dataset_ids, db = DB_NAME, url = MONGO_URL) {
  q <- dbm_in_query("_id", dataset_ids)
  con <- .con("datasets", db, url)
  docs <- tryCatch(con$find(q), error = function(e) data.frame())
  n <- if (nrow(docs) > 0) nrow(docs) else 0L
  if (n > 0) tryCatch(con$remove(q), error = function(e) NULL)
  n
}

dbm_delete_annotation_sets_by_ids <- function(ids, db = DB_NAME, url = MONGO_URL) {
  q <- dbm_in_query("_id", ids)
  con <- .con("annotation_sets", db, url)
  docs <- tryCatch(con$find(q), error = function(e) data.frame())
  n <- if (nrow(docs) > 0) nrow(docs) else 0L
  if (n > 0) tryCatch(con$remove(q), error = function(e) NULL)
  n
}

dbm_delete_samples_by_ids <- function(ids, db = DB_NAME, url = MONGO_URL) {
  q <- dbm_in_query("_id", ids)
  con <- .con("samples", db, url)
  docs <- tryCatch(con$find(q), error = function(e) data.frame())
  n <- if (nrow(docs) > 0) nrow(docs) else 0L
  if (n > 0) tryCatch(con$remove(q), error = function(e) NULL)
  n
}

dbm_delete_study_by_id <- function(study_id, db = DB_NAME, url = MONGO_URL) {
  samples_df <- dbm_safe_find("samples", sprintf('{"study_id": "%s"}', study_id), db = db, url = url)
  sample_ids <- if (nrow(samples_df) > 0 && "_id" %in% names(samples_df)) as.character(samples_df$`_id`) else character(0)
  sample_names <- if (nrow(samples_df) > 0 && "sample_name" %in% names(samples_df)) as.character(samples_df$sample_name) else character(0)

  annset_df <- dbm_safe_find("annotation_sets", sprintf('{"study_id": "%s"}', study_id), db = db, url = url)
  annset_ids <- if (nrow(annset_df) > 0 && "_id" %in% names(annset_df)) as.character(annset_df$`_id`) else character(0)

  dataset_df <- dbm_safe_find("datasets", sprintf('{"study_id": "%s"}', study_id), db = db, url = url)
  dataset_ids <- if (nrow(dataset_df) > 0 && "_id" %in% names(dataset_df)) as.character(dataset_df$`_id`) else character(0)

  list(
    model_runs = dbm_delete_model_runs_by_dataset_ids(dataset_ids, db, url),
    datasets = dbm_delete_datasets_by_ids(dataset_ids, db, url),
    artifacts = dbm_delete_artifacts_by_sample_ids(sample_ids, db, url),
    annotations = dbm_delete_annotations_by_sample_ids(sample_ids, db, url),
    processing_artifacts_metadata = if (length(sample_names) > 0) dbm_delete_docs_with_files("processing_artifacts_metadata", dbm_in_query("sample_name", sample_names), db, url) else 0L,
    clustering_metadata = if (length(sample_ids) > 0) dbm_delete_docs_with_files("clustering_metadata", dbm_in_query("sample_id", sample_ids), db, url) else 0L,
    ndpi_registrations = if (length(sample_ids) > 0) dbm_delete_docs_with_files("ndpi_registrations", dbm_in_query("sample_id", sample_ids), db, url) else 0L,
    annotation_sets = if (length(annset_ids) > 0) dbm_delete_annotation_sets_by_ids(annset_ids, db, url) else 0L,
    samples = if (length(sample_ids) > 0) dbm_delete_samples_by_ids(sample_ids, db, url) else 0L,
    studies = dbm_delete_docs_with_files("studies", sprintf('{"_id": "%s"}', study_id), db, url)
  )
}

dbm_delete_sample_by_id <- function(sample_id, db = DB_NAME, url = MONGO_URL) {
  sample_df <- dbm_safe_find("samples", sprintf('{"_id": "%s"}', sample_id), db = db, url = url)
  sample_name <- if (nrow(sample_df) > 0 && "sample_name" %in% names(sample_df)) as.character(sample_df$sample_name[1]) else NULL

  dataset_df <- dbm_safe_find("datasets", db = db, url = url)
  dataset_ids <- character(0)
  if (nrow(dataset_df) > 0 && "sample_ids" %in% names(dataset_df)) {
    hit <- vapply(dataset_df$sample_ids, function(x) sample_id %in% unlist(x), logical(1))
    if (any(hit)) dataset_ids <- as.character(dataset_df$`_id`[hit])
  }

  list(
    model_runs = dbm_delete_model_runs_by_dataset_ids(dataset_ids, db, url),
    datasets = dbm_delete_datasets_by_ids(dataset_ids, db, url),
    artifacts = dbm_delete_artifacts_by_sample_ids(sample_id, db, url),
    annotations = dbm_delete_annotations_by_sample_ids(sample_id, db, url),
    processing_artifacts_metadata = if (!is.null(sample_name) && nzchar(sample_name)) dbm_delete_docs_with_files("processing_artifacts_metadata", sprintf('{"sample_name": "%s"}', sample_name), db, url) else 0L,
    clustering_metadata = dbm_delete_docs_with_files("clustering_metadata", sprintf('{"sample_id": "%s"}', sample_id), db, url),
    ndpi_registrations = dbm_delete_docs_with_files("ndpi_registrations", sprintf('{"sample_id": "%s"}', sample_id), db, url),
    samples = dbm_delete_samples_by_ids(sample_id, db, url)
  )
}

dbm_delete_pipeline_by_id <- function(pipeline_id, db = DB_NAME, url = MONGO_URL) {
  artifacts_df <- dbm_safe_find("artifacts", sprintf('{"pipeline_id": "%s"}', pipeline_id), db = db, url = url)
  datasets_df <- dbm_safe_find("datasets", sprintf('{"pipeline_id": "%s"}', pipeline_id), db = db, url = url)
  dataset_ids <- if (nrow(datasets_df) > 0) as.character(datasets_df$`_id`) else character(0)

  list(
    model_runs = dbm_delete_model_runs_by_dataset_ids(dataset_ids, db, url),
    datasets = dbm_delete_datasets_by_ids(dataset_ids, db, url),
    artifacts = if (nrow(artifacts_df) > 0) dbm_delete_docs_with_files("artifacts", sprintf('{"pipeline_id": "%s"}', pipeline_id), db, url) else 0L,
    clustering_metadata = dbm_delete_docs_with_files("clustering_metadata", sprintf('{"pipeline_id": "%s"}', pipeline_id), db, url),
    ndpi_registrations = dbm_delete_docs_with_files("ndpi_registrations", sprintf('{"pipeline_id": "%s"}', pipeline_id), db, url),
    pipelines = dbm_delete_docs_with_files("pipelines", sprintf('{"_id": "%s"}', pipeline_id), db, url)
  )
}

dbm_delete_annotation_set_by_id <- function(annset_id, db = DB_NAME, url = MONGO_URL) {
  ann_df <- dbm_safe_find("annotations", sprintf('{"annotation_set_id": "%s"}', annset_id), db = db, url = url)
  datasets_df <- dbm_safe_find("datasets", sprintf('{"annotation_set_id": "%s"}', annset_id), db = db, url = url)
  dataset_ids <- if (nrow(datasets_df) > 0) as.character(datasets_df$`_id`) else character(0)

  list(
    model_runs = dbm_delete_model_runs_by_dataset_ids(dataset_ids, db, url),
    datasets = dbm_delete_datasets_by_ids(dataset_ids, db, url),
    annotations = if (nrow(ann_df) > 0) dbm_delete_docs_with_files("annotations", sprintf('{"annotation_set_id": "%s"}', annset_id), db, url) else 0L,
    annotation_sets = dbm_delete_docs_with_files("annotation_sets", sprintf('{"_id": "%s"}', annset_id), db, url)
  )
}

dbm_delete_dataset_by_id <- function(dataset_id, db = DB_NAME, url = MONGO_URL) {
  list(
    model_runs = dbm_delete_model_runs_by_dataset_ids(dataset_id, db, url),
    datasets = dbm_delete_datasets_by_ids(dataset_id, db, url)
  )
}

dbm_delete_record <- function(collection, id, db = DB_NAME, url = MONGO_URL) {
  stopifnot(collection %in% dbm_catalog()$key)
  if (is.null(id) || !nzchar(id)) stop("No record selected.")

  switch(
    collection,
    studies = dbm_delete_study_by_id(id, db, url),
    samples = dbm_delete_sample_by_id(id, db, url),
    pipelines = dbm_delete_pipeline_by_id(id, db, url),
    artifacts = list(artifacts = dbm_delete_docs_with_files("artifacts", sprintf('{"_id": "%s"}', id), db, url)),
    annotation_sets = dbm_delete_annotation_set_by_id(id, db, url),
    annotations = list(annotations = dbm_delete_docs_with_files("annotations", sprintf('{"_id": "%s"}', id), db, url)),
    datasets = dbm_delete_dataset_by_id(id, db, url),
    model_runs = list(model_runs = dbm_delete_docs_with_files("model_runs", sprintf('{"_id": "%s"}', id), db, url)),
    processing_artifacts_metadata = {
      # old docs may be addressed by _id or run_id
      deleted <- dbm_delete_docs_with_files("processing_artifacts_metadata", sprintf('{"_id": "%s"}', id), db, url)
      if (deleted == 0) {
        deleted <- dbm_delete_docs_with_files("processing_artifacts_metadata", sprintf('{"run_id": "%s"}', id), db, url)
      }
      list(processing_artifacts_metadata = deleted)
    },
    clustering_metadata = list(clustering_metadata = dbm_delete_docs_with_files("clustering_metadata", sprintf('{"_id": "%s"}', id), db, url)),
    ndpi_registrations = list(ndpi_registrations = dbm_delete_docs_with_files("ndpi_registrations", sprintf('{"_id": "%s"}', id), db, url))
  )
}

dbm_delete_report_text <- function(report) {
  if (is.null(report) || length(report) == 0) return("Nothing was deleted.")
  kept <- report[as.numeric(report) > 0]
  if (length(kept) == 0) return("Nothing was deleted.")
  paste(paste(names(kept), kept, sep = ": "), collapse = " | ")
}

# ---- Summary text for confirmation modal ----------------------------------

dbm_record_title <- function(collection, record) {
  if (is.null(record) || nrow(record) == 0) return(collection)
  r <- record[1, , drop = FALSE]
  switch(
    collection,
    studies = paste0("Study: ", dbm_first_chr(r$name, dbm_first_chr(r$`_id`))),
    samples = paste0("Sample: ", dbm_first_chr(r$sample_name, dbm_first_chr(r$`_id`))),
    pipelines = paste0("Pipeline: ", dbm_first_chr(r$name, dbm_first_chr(r$`_id`))),
    artifacts = paste0("Artifact: ", dbm_first_chr(r$stage_type, dbm_first_chr(r$`_id`))),
    annotation_sets = paste0("Annotation set: ", dbm_first_chr(r$name, dbm_first_chr(r$`_id`))),
    annotations = paste0("Annotation: ", dbm_first_chr(r$`_id`)),
    datasets = paste0("Dataset: ", dbm_first_chr(r$name, dbm_first_chr(r$`_id`))),
    model_runs = paste0("Model run: ", dbm_first_chr(r$`_id`)),
    processing_artifacts_metadata = paste0("Legacy processing record: ", dbm_first_chr(r$stage_type, dbm_first_chr(r$run_id, dbm_first_chr(r$`_id`)))),
    clustering_metadata = paste0("Legacy clustering record: ", dbm_first_chr(r$clustering_method, dbm_first_chr(r$`_id`))),
    ndpi_registrations = paste0("NDPI registration: ", dbm_first_chr(r$ndpi_slide_name, dbm_first_chr(r$`_id`)))
  )
}
