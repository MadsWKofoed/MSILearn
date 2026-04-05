# R/mongo_functions.R
# All MongoDB helper functions.
#
# The file is split into three layers:
#   A. Low-level utilities (sanitisation, GridFS read/write)
#   B. New provenance-aware API  (studies / samples / pipelines /
#      artifacts / annotation_sets / annotations / datasets / model_runs)
#   C. Legacy processing helpers kept for backwards compatibility
#      with processing_module.R and clustering_module.R.
#      These continue to write into "processing_artifacts_metadata" /
#      "clustering_metadata" but callers must now supply explicit IDs;
#      the "most recent" fallback has been removed.

library(digest)
library(jsonlite)
library(mongolite)

DB_NAME   <- "MSI_database_test"
MONGO_URL <- "mongodb://localhost:27018"

# Null-coalescing operator used throughout
`%||%` <- function(a, b) if (!is.null(a)) a else b

# ============================================================================
# A.  LOW-LEVEL UTILITIES
# ============================================================================

sanitize_colnames <- function(nms) {
  nms <- gsub("\\.", "_", nms, perl = TRUE)
  nms <- ifelse(grepl("^\\$", nms), paste0("dollar_", sub("^\\$", "", nms)), nms)
  nms
}

normalize_for_mongo <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  for (col in names(df)) {
    if (is.factor(df[[col]])) df[[col]] <- as.character(df[[col]])
  }
  df
}

sanitize_name <- function(x) gsub("[^A-Za-z0-9._-]+", "_", x)

.con <- function(collection, db = DB_NAME, url = MONGO_URL) {
  mongolite::mongo(collection = collection, db = db, url = url)
}

.gridfs <- function(db = DB_NAME, url = MONGO_URL, prefix = "fs") {
  mongolite::gridfs(db = db, prefix = prefix, url = url)
}

# Insert a document into a mongolite collection, auto-unboxing all scalars so
# that character(1) / numeric(1) / logical(1) become JSON scalars, not arrays.
.insert <- function(col, doc) {
  col$insert(jsonlite::toJSON(doc, auto_unbox = TRUE, null = "null", na = "null"))
}

# Upload an in-memory R object (serialised as RDS) to GridFS.
.upload_rds <- function(obj, filename, db = DB_NAME, url = MONGO_URL) {
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))
  saveRDS(obj, tmp)
  .gridfs(db, url)$upload(tmp, name = filename)
  filename
}

# Download an RDS file from GridFS by filename and return the object.
.download_rds <- function(filename, db = DB_NAME, url = MONGO_URL) {
  tmp <- tempfile(fileext = ".rds")
  on.exit(unlink(tmp))
  .gridfs(db, url)$download(filename, tmp)
  readRDS(tmp)
}


# ============================================================================
# B.  PROVENANCE-AWARE API
# ============================================================================

# ----------------------------------------------------------------------------
# B1.  PIPELINE  (deterministic _id via digest)
# ----------------------------------------------------------------------------

#' Generate or retrieve a deterministic pipeline document.
#'
#' The pipeline _id is SHA-256( type || sorted(params) || code_version ).
#' Two calls with identical arguments always return the same _id.
#'
#' @param type         One of "processing", "features", "clustering".
#' @param name         Human-readable label.
#' @param params       Named list of all parameter values.
#' @param code_version Character. E.g. paste(git_hash, app_version, sep="-").
#'
#' @return pipeline_id (character).
upsert_pipeline <- function(type, name, params,
                            code_version = "dev",
                            db = DB_NAME, url = MONGO_URL) {

  stopifnot(is.character(type), length(type) == 1, is.list(params))

  canonical <- list(
    type         = type,
    params       = params[order(names(params))],
    code_version = code_version
  )
  pipeline_id <- digest::digest(canonical, algo = "sha256", serialize = TRUE)

  col <- .con("pipelines", db, url)
  if (col$count(sprintf('{"_id": "%s"}', pipeline_id)) == 0) {
    .insert(col, list(
      `_id`        = pipeline_id,
      type         = type,
      name         = name,
      params       = params,
      params_hash  = pipeline_id,
      code_version = code_version,
      created_at   = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ))
    message("✓ Pipeline registered: ", pipeline_id, " (", name, ")")
  }
  pipeline_id
}

#' Retrieve a pipeline document by _id.
get_pipeline <- function(pipeline_id, db = DB_NAME, url = MONGO_URL) {
  res <- .con("pipelines", db, url)$find(sprintf('{"_id": "%s"}', pipeline_id))
  if (nrow(res) == 0) stop("Pipeline not found: ", pipeline_id)
  res
}




# ----------------------------------------------------------------------------
# B2.  STUDY
# ----------------------------------------------------------------------------

#' Insert a study (idempotent). _id must be a stable, meaningful string.
upsert_study <- function(study_id, name, description = "",
                         db = DB_NAME, url = MONGO_URL) {
  col <- .con("studies", db, url)
  if (col$count(sprintf('{"_id": "%s"}', study_id)) == 0) {
    .insert(col, list(
      `_id`       = study_id,
      name        = name,
      description = description,
      created_at  = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ))
    message("✓ Study inserted: ", study_id)
  }
  study_id
}

list_studies <- function(db = DB_NAME, url = MONGO_URL) {
  .con("studies", db, url)$find("{}", fields = '{"description":0}')
}


# ----------------------------------------------------------------------------
# B3.  SAMPLE
# ----------------------------------------------------------------------------

#' Insert or retrieve a sample; returns its _id.
#'
#' sample _id is deterministic: digest(study_id, sample_name).
#' Use sample_name only as a label; never as a primary key.
upsert_sample <- function(study_id, sample_name,
                          raw_ref          = list(),
                          acquisition_meta = list(),
                          db = DB_NAME, url = MONGO_URL) {

  sample_id <- digest::digest(
    list(study_id = study_id, sample_name = sample_name),
    algo = "sha256", serialize = TRUE
  )

  col <- .con("samples", db, url)
  if (col$count(sprintf('{"_id": "%s"}', sample_id)) == 0) {
    .insert(col, list(
      `_id`            = sample_id,
      study_id         = study_id,
      sample_name      = sample_name,
      raw_ref          = raw_ref,
      acquisition_meta = acquisition_meta,
      created_at       = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ))
    message("✓ Sample registered: ", sample_id, " (", sample_name, ")")
  }
  sample_id
}

#' Compute sample_id without touching the database.
get_sample_id <- function(study_id, sample_name) {
  digest::digest(
    list(study_id = study_id, sample_name = sample_name),
    algo = "sha256", serialize = TRUE
  )
}

list_samples <- function(study_id, db = DB_NAME, url = MONGO_URL) {
  df <- tryCatch(
    .con("samples", db, url)$find(
      sprintf('{"study_id": "%s"}', study_id),
      fields = '{"sample_name":1,"study_id":1,"created_at":1}'
    ),
    error = function(e) {
      message("[list_samples] ERROR for study_id=", study_id, ": ", e$message)
      data.frame()
    }
  )
  if (!("_id" %in% names(df))) {
    return(data.frame(`_id` = character(), sample_name = character(),
                      study_id = character(), created_at = character(),
                      stringsAsFactors = FALSE, check.names = FALSE))
  }
  df
}


# ----------------------------------------------------------------------------
# B4.  ARTIFACTS
# ----------------------------------------------------------------------------

#' Save a processed R object as a versioned artifact.
#'
#' Enforces the unique index (sample_id, stage_type, pipeline_id).
#' Errors immediately if a duplicate would be created.
#'
#' @param obj         R object to serialise.
#' @param study_id    study _id.
#' @param sample_id   sample _id (from upsert_sample / get_sample_id).
#' @param pipeline_id pipeline _id (from upsert_pipeline).
#' @param stage_type  character, e.g. "binned_dataframe".
#'
#' @return artifact_id (character).
save_artifact <- function(obj, study_id, sample_id, pipeline_id, stage_type,
                          extra_meta = list(),
                          db = DB_NAME, url = MONGO_URL) {

  col   <- .con("artifacts", db, url)
  dup_q <- jsonlite::toJSON(
    list(sample_id = sample_id, stage_type = stage_type, pipeline_id = pipeline_id),
    auto_unbox = TRUE
  )
  if (col$count(dup_q) > 0) {
    stop(
      "Artifact already exists for sample_id=", sample_id,
      ", stage_type=", stage_type, ", pipeline_id=", pipeline_id,
      ". Use load_artifact_by_pipeline() to retrieve it."
    )
  }

  artifact_id <- digest::digest(
    list(sample_id = sample_id, stage_type = stage_type, pipeline_id = pipeline_id),
    algo = "sha256", serialize = TRUE
  )
  filename <- paste0(artifact_id, "_", stage_type, ".rds")
  .upload_rds(obj, filename, db, url)

  .insert(col, c(
    list(
      `_id`       = artifact_id,
      study_id    = study_id,
      sample_id   = sample_id,
      pipeline_id = pipeline_id,
      stage_type  = stage_type,
      gridfs_name = filename,
      created_at  = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ),
    extra_meta
  ))

  message("✓ Artifact saved: ", artifact_id,
          " (", stage_type, " | sample: ", sample_id, ")")
  invisible(artifact_id)
}

#' Load an artifact strictly by (sample_id, stage_type, pipeline_id).
#' Never falls back to "most recent". Errors if not found.
load_artifact_by_pipeline <- function(sample_id, stage_type, pipeline_id,
                                      db = DB_NAME, url = MONGO_URL) {
  col <- .con("artifacts", db, url)
  q   <- jsonlite::toJSON(
    list(sample_id = sample_id, stage_type = stage_type, pipeline_id = pipeline_id),
    auto_unbox = TRUE
  )
  res <- col$find(q)
  if (nrow(res) == 0) {
    stop("No artifact for sample_id=", sample_id,
         ", stage_type=", stage_type, ", pipeline_id=", pipeline_id)
  }
  if (nrow(res) > 1) {
    stop("Integrity error: multiple artifacts match. Check unique index.")
  }
  .download_rds(res$gridfs_name[1], db, url)
}


#' Query artifact metadata (returns data.frame of metadata rows, not the data).
query_artifacts <- function(study_id = NULL, sample_id = NULL,
                            stage_type = NULL, pipeline_id = NULL,
                            db = DB_NAME, url = MONGO_URL) {
  parts <- list()
  if (!is.null(study_id))    parts$study_id    <- study_id
  if (!is.null(sample_id))   parts$sample_id   <- sample_id
  if (!is.null(stage_type))  parts$stage_type  <- stage_type
  if (!is.null(pipeline_id)) parts$pipeline_id <- pipeline_id
  q <- if (length(parts) == 0) "{}" else jsonlite::toJSON(parts, auto_unbox = TRUE)
  .con("artifacts", db, url)$find(q)
}


# ----------------------------------------------------------------------------
# B5.  ANNOTATION SETS & ANNOTATIONS
# ----------------------------------------------------------------------------

#' Create a versioned annotation set (idempotent by study_id + name).
upsert_annotation_set <- function(study_id, name, label_schema,
                                  db = DB_NAME, url = MONGO_URL) {
  ann_id <- digest::digest(
    list(study_id = study_id, name = name),
    algo = "sha256", serialize = TRUE
  )
  col <- .con("annotation_sets", db, url)
  if (col$count(sprintf('{"_id": "%s"}', ann_id)) == 0) {
    .insert(col, list(
      `_id`        = ann_id,
      study_id     = study_id,
      name         = name,
      label_schema = as.list(label_schema),
      created_at   = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ))
    message("✓ Annotation set registered: ", ann_id, " (", name, ")")
  }
  ann_id
}


#' Load annotation data.frame by (sample_id, annotation_set_id). No fallback.
load_annotation <- function(sample_id, annotation_set_id,
                            db = DB_NAME, url = MONGO_URL) {
  col <- .con("annotations", db, url)
  q   <- jsonlite::toJSON(
    list(sample_id = sample_id, annotation_set_id = annotation_set_id),
    auto_unbox = TRUE
  )
  res <- col$find(q)
  if (nrow(res) == 0) {
    stop("No annotation for sample_id=", sample_id,
         ", annotation_set_id=", annotation_set_id)
  }
  .download_rds(res$gridfs_name[1], db, url)
}

list_annotation_sets <- function(study_id, db = DB_NAME, url = MONGO_URL) {
  tryCatch({
    col <- .con("annotation_sets", db, url)
    
    # Check raw count first
    cnt <- col$count(sprintf('{"study_id": "%s"}', study_id))
    message("[list_annotation_sets] count()=", cnt, " for study_id=", study_id)
    
    df <- col$find(
      sprintf('{"study_id": "%s"}', study_id),
      fields = '{"label_schema": 0}'   # exclude the array field
    )
    message("[list_annotation_sets] nrow after find=", nrow(df),
            " cols=", paste(names(df), collapse=","))
    
    if (!("_id" %in% names(df))) {
      return(data.frame(`_id` = character(), name = character(),
                        study_id = character(), created_at = character(),
                        stringsAsFactors = FALSE, check.names = FALSE))
    }
    df
  }, error = function(e) {
    message("[list_annotation_sets] ERROR: ", e$message)
    data.frame(`_id` = character(), name = character(),
               study_id = character(), created_at = character(),
               stringsAsFactors = FALSE, check.names = FALSE)
  })
}

#' Save or REPLACE pixel-level annotations for one sample.
#'
#' This function uses upsert semantics:
#' if an annotation already exists for (sample_id, annotation_set_id) it is
#' deleted and re-written with the new data.  This allows the user to
#' re-commit after re-labelling pixels without errors.
#'
#' annotation_df must contain at least columns: x, y, Class.
upsert_annotation <- function(annotation_df, sample_id, annotation_set_id,
                              format = "dataframe_rds",
                              db = DB_NAME, url = MONGO_URL) {
  col   <- .con("annotations", db, url)
  dup_q <- jsonlite::toJSON(
    list(sample_id = sample_id, annotation_set_id = annotation_set_id),
    auto_unbox = TRUE
  )

  # Remove previous annotation (GridFS file + metadata document) if present
  existing <- col$find(dup_q)
  if (nrow(existing) > 0) {
    old_gridfs <- as.character(existing$gridfs_name[1])
    tryCatch(
      .gridfs(db, url)$remove(old_gridfs),
      error = function(e)
        message("[upsert_annotation] Could not remove old GridFS file (non-fatal): ", e$message)
    )
    col$remove(dup_q)
    message("[upsert_annotation] Replaced existing annotation for sample_id=",
            sample_id, ", annotation_set_id=", annotation_set_id)
  }

  ann_doc_id <- digest::digest(
    list(sample_id = sample_id, annotation_set_id = annotation_set_id),
    algo = "sha256", serialize = TRUE
  )
  filename <- paste0(ann_doc_id, "_annotation.rds")
  .upload_rds(annotation_df, filename, db, url)

  .insert(col, list(
    `_id`             = ann_doc_id,
    sample_id         = sample_id,
    annotation_set_id = annotation_set_id,
    format            = format,
    gridfs_name       = filename,
    created_at        = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  ))
  message("\u2713 Annotation upserted: ", ann_doc_id)
  invisible(ann_doc_id)
}


# ============================================================================
# B.EXTRA  CONVENIENCE WRAPPERS (used by Shiny modules)
# ============================================================================

#' Return all study documents as a data.frame.
get_studies <- function(db = DB_NAME, url = MONGO_URL) {
  df <- tryCatch(
    list_studies(db, url),
    error = function(e) {
      message("[get_studies] ERROR: ", e$message)
      data.frame()
    }
  )
  if (!("_id" %in% names(df))) {
    return(data.frame(`_id` = character(), name = character(),
                      created_at = character(),
                      stringsAsFactors = FALSE, check.names = FALSE))
  }
  df
}

#' Create a new study and return its _id.
#' study_id defaults to a URL-safe slug derived from name + timestamp.
create_study <- function(name, description = "",
                         study_id = NULL,
                         db = DB_NAME, url = MONGO_URL) {
  if (is.null(study_id) || !nzchar(study_id)) {
    slug <- gsub("[^A-Za-z0-9]+", "_", tolower(name))
    study_id <- paste0("study_", slug, "_", format(Sys.time(), "%Y%m%d%H%M%S"))
  }
  upsert_study(study_id, name, description, db, url)
}

#' Return all sample documents for a study as a data.frame.
get_samples <- function(study_id, db = DB_NAME, url = MONGO_URL) {
  list_samples(study_id, db, url)  # already returns a well-formed data.frame
}



#' Check whether a sample_name already exists within a study.
sample_name_exists <- function(study_id, sample_name, db = DB_NAME, url = MONGO_URL) {
  col <- .con("samples", db, url)
  q   <- jsonlite::toJSON(
    list(study_id = study_id, sample_name = sample_name),
    auto_unbox = TRUE
  )
  col$count(q) > 0
}

#' Save a clustering result as a first-class artifact.
#' cluster_pipeline_id must be produced by upsert_pipeline(type="clustering", ...).
#'
#' Upsert semantics: if an artifact already exists for the same
#' (sample_id, stage_type="clustering_result", cluster_pipeline_id), the
#' existing artifact_id is returned and no duplicate is written.  This allows
#' the user to re-commit after adjusting cluster labels without errors.
save_clustering_artifact <- function(clustered_df,
                                     study_id,
                                     sample_id,
                                     input_artifact_id,
                                     cluster_pipeline_id,
                                     db = DB_NAME, url = MONGO_URL) {
  col   <- .con("artifacts", db, url)
  dup_q <- jsonlite::toJSON(
    list(sample_id = sample_id,
         stage_type  = "clustering_result",
         pipeline_id = cluster_pipeline_id),
    auto_unbox = TRUE
  )
  existing <- col$find(dup_q, fields = '{"_id":1}')
  if (nrow(existing) > 0) {
    message("[save_clustering_artifact] Artifact already exists: ",
            existing[["_id"]][1], " — returning existing id.")
    return(invisible(existing[["_id"]][1]))
  }
  save_artifact(
    obj         = clustered_df,
    study_id    = study_id,
    sample_id   = sample_id,
    pipeline_id = cluster_pipeline_id,
    stage_type  = "clustering_result",
    extra_meta  = list(input_artifact_id = input_artifact_id),
    db          = db,
    url         = url
  )
}

#' List all unique pipeline_ids that produced artifacts of a given stage_type
#' for a given sample.
list_available_pipeline_ids <- function(sample_id, stage_type,
                                        db = DB_NAME, url = MONGO_URL) {
  res <- query_artifacts(sample_id = sample_id, stage_type = stage_type, db = db, url = url)
  if (nrow(res) == 0) return(character(0))
  unique(res$pipeline_id)
}

#' Compute (but do not persist) the deterministic pipeline_id for a parameter set.
compute_pipeline_id <- function(type, params, code_version = "dev") {
  canonical <- list(
    type         = type,
    params       = params[order(names(params))],
    code_version = code_version
  )
  digest::digest(canonical, algo = "sha256", serialize = TRUE)
}


# ----------------------------------------------------------------------------
# B6.  DATASETS  (frozen snapshots – critical for reproducible ML)
# ----------------------------------------------------------------------------

#' Create a frozen dataset snapshot.
#'
#' Pins exactly which samples, pipeline, annotation set, and split seed
#' will be used for training.  Cannot be mutated after creation.
#'
#' @param study_id          All sample_ids must belong to this study.
#' @param sample_ids        Character vector of sample _ids.
#' @param pipeline_id       Determines which features to use.
#' @param annotation_set_id Determines which labels to use.
#' @param stage_type        Artifact stage_type to pull features from.
#' @param feature_spec      list(type, version) describing feature extraction.
#' @param split             list(strategy, seed, train_frac).
#' @param name              Human-readable label.
#'
#' @return dataset_id (character).
create_dataset <- function(study_id,
                           sample_ids,
                           pipeline_id,
                           annotation_set_id,
                           stage_type    = "binned_dataframe",
                           feature_spec  = list(type = "mz_bins", version = "1.0"),
                           split         = list(strategy   = "random",
                                                seed       = 42L,
                                                train_frac = 0.8),
                           name          = "",
                           db = DB_NAME, url = MONGO_URL) {

  stopifnot(length(sample_ids) > 0)

  # --- validate: all samples belong to the declared study ---
  samples_col <- .con("samples", db, url)
  found <- samples_col$find(
    jsonlite::toJSON(list(`_id` = list(`$in` = as.list(sample_ids))),
                     auto_unbox = TRUE),
    fields = '{"_id":1,"study_id":1}'
  )
  missing_ids <- setdiff(sample_ids, found[["_id"]])
  if (length(missing_ids) > 0) {
    stop("Samples not found in database: ", paste(missing_ids, collapse = ", "))
  }
  wrong_study <- found$study_id[found$study_id != study_id]
  if (length(wrong_study) > 0) {
    stop("Cross-study mixing detected. Samples from study(ies) ",
         paste(unique(wrong_study), collapse = ", "),
         " cannot be mixed with study_id=", study_id)
  }

  # --- validate: artifact exists for every sample ---
  arts_col <- .con("artifacts", db, url)
  for (sid in sample_ids) {
    q <- jsonlite::toJSON(
      list(sample_id = sid, stage_type = stage_type, pipeline_id = pipeline_id),
      auto_unbox = TRUE
    )
    if (arts_col$count(q) == 0) {
      stop("Missing artifact (", stage_type, ") for sample_id=", sid,
           " with pipeline_id=", pipeline_id)
    }
  }

  # --- validate: annotation exists for every sample ---
  ann_col <- .con("annotations", db, url)
  for (sid in sample_ids) {
    q <- jsonlite::toJSON(
      list(sample_id = sid, annotation_set_id = annotation_set_id),
      auto_unbox = TRUE
    )
    if (ann_col$count(q) == 0) {
      stop("Missing annotation for sample_id=", sid,
           " with annotation_set_id=", annotation_set_id)
    }
  }

  # --- deterministic dataset _id ---
  dataset_id <- digest::digest(
    list(
      study_id          = study_id,
      sample_ids        = sort(sample_ids),
      pipeline_id       = pipeline_id,
      annotation_set_id = annotation_set_id,
      stage_type        = stage_type,
      feature_spec      = feature_spec,
      split             = split
    ),
    algo = "sha256", serialize = TRUE
  )

  ds_col <- .con("datasets", db, url)
  if (ds_col$count(sprintf('{"_id": "%s"}', dataset_id)) > 0) {
    message("Dataset already exists: ", dataset_id)
    return(dataset_id)
  }

  .insert(ds_col, list(
    `_id`             = dataset_id,
    study_id          = study_id,
    name              = name,
    sample_ids        = as.list(sample_ids),
    pipeline_id       = pipeline_id,
    annotation_set_id = annotation_set_id,
    stage_type        = stage_type,
    feature_spec      = feature_spec,
    split             = split,
    created_at        = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  ))

  message("✓ Dataset created: ", dataset_id,
          " (", length(sample_ids), " samples, study: ", study_id, ")")
  dataset_id
}

#' Retrieve a dataset document by _id.
get_dataset <- function(dataset_id, db = DB_NAME, url = MONGO_URL) {
  res <- .con("datasets", db, url)$find(sprintf('{"_id": "%s"}', dataset_id))
  if (nrow(res) == 0) stop("Dataset not found: ", dataset_id)
  res
}

#' List all datasets (summary view, no heavy data).
list_datasets <- function(study_id = NULL, db = DB_NAME, url = MONGO_URL) {
  q <- if (is.null(study_id)) "{}" else sprintf('{"study_id": "%s"}', study_id)
  .con("datasets", db, url)$find(
    q,
    fields = '{"name":1,"study_id":1,"pipeline_id":1,
               "annotation_set_id":1,"stage_type":1,"created_at":1}'
  )
}

#' Materialise a dataset: load features + labels, apply the frozen split.
#'
#' This is the ONLY authorised entry point for assembling training data.
#' All lookups are by exact pipeline_id; no "most recent" fallback exists.
#'
#' @return Named list: train_X, train_y, test_X, test_y,
#'         split_info, dataset_meta, pipeline_meta.
load_dataset_for_training <- function(dataset_id, db = DB_NAME, url = MONGO_URL) {

  ds                <- get_dataset(dataset_id, db, url)
  sample_ids        <- unlist(ds$sample_ids)
  pipeline_id       <- ds$pipeline_id
  annotation_set_id <- ds$annotation_set_id
  stage_type        <- ds$stage_type

  # split params may arrive as a list or as a single-row data.frame
  sp             <- if (is.data.frame(ds$split)) as.list(ds$split[1, ]) else ds$split[[1]]
  split_strategy <- sp$strategy  %||% "random"
  split_seed     <- as.integer(sp$seed %||% 42L)
  split_frac     <- as.numeric(sp$train_frac %||% 0.8)

  message("[dataset] Materialising dataset ", dataset_id,
          " (", length(sample_ids), " samples)")

  all_features <- vector("list", length(sample_ids))
  all_labels   <- vector("list", length(sample_ids))
  all_meta     <- vector("list", length(sample_ids))

  for (i in seq_along(sample_ids)) {
    sid <- sample_ids[i]
    message("[dataset]  [", i, "/", length(sample_ids), "] sample_id: ", sid)

    feat_df <- load_artifact_by_pipeline(sid, stage_type, pipeline_id, db, url)
    mz_cols <- grep("^mz_", colnames(feat_df), value = TRUE)
    if (length(mz_cols) == 0) stop("No mz_ columns in artifact for sample_id=", sid)

    ann_df <- load_annotation(sid, annotation_set_id, db, url)

    merged <- merge(
      feat_df[, c("x", "y", mz_cols)],
      ann_df[,  c("x", "y", "Class")],
      by  = c("x", "y"),
      all = FALSE
    )
    if (nrow(merged) == 0) {
      stop("No pixel overlap after joining features and annotations for sample_id=", sid)
    }

    all_features[[i]] <- as.matrix(merged[, mz_cols, drop = FALSE])
    all_labels[[i]]   <- merged$Class
    all_meta[[i]]     <- data.frame(
      sample_id = sid,
      x = merged$x,
      y = merged$y,
      stringsAsFactors = FALSE
    )
  }

  X <- do.call(rbind, all_features)
  y <- as.factor(do.call(c, all_labels))
  meta <- do.call(rbind, all_meta)

  set.seed(split_seed)
  idx     <- sample(nrow(X))
  n_train <- ceiling(nrow(X) * split_frac)
  tr_idx  <- idx[seq_len(n_train)]
  te_idx  <- idx[(n_train + 1):length(idx)]

  list(
    train_X      = X[tr_idx, , drop = FALSE],
    train_y      = y[tr_idx],
    train_meta   = meta[tr_idx, , drop = FALSE],
    test_X       = X[te_idx, , drop = FALSE],
    test_y       = y[te_idx],
    test_meta    = meta[te_idx, , drop = FALSE],
    split_info   = list(
      strategy   = split_strategy,
      seed       = split_seed,
      train_frac = split_frac,
      n_train    = length(tr_idx),
      n_test     = length(te_idx)
    ),
    dataset_meta  = ds,
    pipeline_meta = get_pipeline(pipeline_id, db, url)
  )
}


# ----------------------------------------------------------------------------
# B7.  MODEL RUNS
# ----------------------------------------------------------------------------

#' Save the result of a training run.
#'
#' @param dataset_id   Dataset _id (validated before save).
#' @param model_type   Character, e.g. "ranger".
#' @param hyperparams  Named list of all hyperparameters used.
#' @param metrics      Named list of evaluation metrics.
#' @param model_obj    Fitted model object (serialised to GridFS).
#'
#' @return model_run_id (character).
save_model_run <- function(dataset_id, model_type, hyperparams, metrics,
                           model_obj,
                           db = DB_NAME, url = MONGO_URL) {
  get_dataset(dataset_id, db, url)   # validate; errors if not found

  run_id   <- paste0("run_",
                     format(Sys.time(), "%Y%m%d_%H%M%S_"),
                     substr(digest::digest(runif(1), algo = "sha256"), 1, 8))
  filename <- paste0(run_id, "_model.rds")
  .upload_rds(model_obj, filename, db, url)

  .insert(.con("model_runs", db, url), list(
    `_id`        = run_id,
    dataset_id   = dataset_id,
    model_type   = model_type,
    hyperparams  = hyperparams,
    metrics      = metrics,
    model_gridfs = filename,
    created_at   = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
  ))

  message("✓ Model run saved: ", run_id,
          " | dataset: ", dataset_id, " | type: ", model_type)
  invisible(run_id)
}

#' Load a fitted model object by model_run_id.
load_model_run <- function(run_id, db = DB_NAME, url = MONGO_URL) {
  res <- .con("model_runs", db, url)$find(sprintf('{"_id": "%s"}', run_id))
  if (nrow(res) == 0) stop("Model run not found: ", run_id)
  .download_rds(res$model_gridfs[1], db, url)
}

#' List all model runs trained on a dataset.
list_model_runs <- function(dataset_id, db = DB_NAME, url = MONGO_URL) {
  .con("model_runs", db, url)$find(
    sprintf('{"dataset_id": "%s"}', dataset_id),
    fields = '{"model_type":1,"metrics":1,"hyperparams":1,"created_at":1}'
  )
}


# ============================================================================
# C.  LEGACY PROCESSING HELPERS
# (backwards-compatible with processing_module.R and clustering_module.R)
# These still write to "processing_artifacts_metadata" / "clustering_metadata".
# The "most recent" fallback has been REMOVED from all loaders.
# ============================================================================

save_raw_pair_to_mongo <- function(sample_name, imzml_path, ibd_path,
                                   db_name   = DB_NAME,
                                   mongo_url = MONGO_URL,
                                   bucket    = "fs") {
  stopifnot(file.exists(imzml_path), file.exists(ibd_path))

  grid <- mongolite::gridfs(db = db_name, prefix = bucket, url = mongo_url)
  meta <- .con("processing_artifacts_metadata", db_name, mongo_url)

  base     <- tools::file_path_sans_ext(basename(sample_name))
  ts       <- format(Sys.time(), "%Y%m%d_%H%M%S")
  imz_name <- sanitize_name(sprintf("%s__%s.imzML", base, ts))
  ibd_name <- sanitize_name(sprintf("%s__%s.ibd",   base, ts))

  message("Uploading imzML to GridFS...")
  imz_id <- unname(grid$upload(imzml_path, name = imz_name))
  message("Uploading ibd to GridFS...")
  ibd_id <- unname(grid$upload(ibd_path, name = ibd_name))

  .insert(meta, list(
    sample_name       = sample_name,
    stage_type        = "raw_files",
    created_at        = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC"),
    imzml_gridfs_id   = as.character(imz_id),
    imzml_gridfs_name = imz_name,
    ibd_gridfs_id     = as.character(ibd_id),
    ibd_gridfs_name   = ibd_name
  ))

  message("✓ Raw files saved to MongoDB")
  invisible(list(imzml_id = as.character(imz_id), ibd_id = as.character(ibd_id)))
}

fetch_raw_pair_from_mongo <- function(sample_name, dest_dir,
                                      db_name   = DB_NAME,
                                      mongo_url = MONGO_URL,
                                      bucket    = "fs") {
  grid      <- mongolite::gridfs(db = db_name, prefix = bucket, url = mongo_url)
  meta      <- .con("processing_artifacts_metadata", db_name, mongo_url)
  artifacts <- meta$find(jsonlite::toJSON(
    list(sample_name = sample_name, stage_type = "raw_files"), auto_unbox = TRUE
  ))
  if (nrow(artifacts) == 0) stop("No raw_files for sample: ", sample_name)
  row <- artifacts[nrow(artifacts), , drop = FALSE]
  dir.create(dest_dir, recursive = TRUE, showWarnings = FALSE)
  base <- tools::file_path_sans_ext(basename(sample_name))
  fi   <- file.path(dest_dir, paste0(base, ".imzML"))
  fd   <- file.path(dest_dir, paste0(base, ".ibd"))
  grid$download(as.character(row$imzml_gridfs_name[1]), fi)
  if (!file.exists(fi)) stop("imzML download failed: ", fi)
  grid$download(as.character(row$ibd_gridfs_name[1]), fd)
  if (!file.exists(fd)) stop("ibd download failed: ", fd)
  list(imzml = fi, ibd = fd)
}

save_stage_to_mongo <- function(msi_object, run_id, stage_type,
                                sample_name,
                                params    = list(),
                                db_name   = DB_NAME,
                                mongo_url = MONGO_URL) {

  if (stage_type %in% c("control_mean", "snr_reference")) {
    meta   <- .con("processing_artifacts_metadata", db_name, mongo_url)
    q_list <- list(sample_name = sample_name, stage_type = stage_type)
    if (stage_type == "snr_reference" && !is.null(params$snr)) q_list$snr <- params$snr
    if (meta$count(jsonlite::toJSON(q_list, auto_unbox = TRUE)) > 0) {
      message("⚠ ", stage_type, " already exists. Skipping save.")
      return(invisible(NULL))
    }
  }

  tmp <- tempfile(pattern = paste0(stage_type, "_"), fileext = ".rds")
  saveRDS(msi_object, tmp)
  on.exit(unlink(tmp))

  grid     <- mongolite::gridfs(db = db_name, prefix = "fs", url = mongo_url)
  filename <- paste0(run_id, "_", stage_type, ".rds")
  grid_res <- grid$upload(tmp, name = filename)
  grid_id  <- as.character(grid_res$id)

  meta <- .con("processing_artifacts_metadata", db_name, mongo_url)
  .insert(meta, c(
    list(
      gridfs_id   = grid_id,
      filename    = filename,
      run_id      = run_id,
      sample_name = sample_name,
      stage_type  = stage_type,
      created_at  = format(Sys.time(), "%Y-%m-%dT%H:%M:%SZ", tz = "UTC")
    ),
    params
  ))

  message("✓ Saved '", stage_type, "' (GridFS ID: ", grid_id, ")")
  invisible(grid_id)
}


# Legacy artifact query (wraps old collection; used by processing_module.R).
query_legacy_artifacts <- function(sample_name    = NULL,
                                   stage_type     = NULL,
                                   snr            = NULL,
                                   tolerance      = NULL,
                                   reference_name = NULL,
                                   run_id         = NULL,
                                   db_name        = DB_NAME,
                                   mongo_url      = MONGO_URL) {
  parts <- list()
  if (!is.null(sample_name))    parts$sample_name    <- sample_name
  if (!is.null(stage_type))     parts$stage_type     <- stage_type
  if (!is.null(snr))            parts$snr            <- as.numeric(snr)
  if (!is.null(tolerance))      parts$tolerance      <- as.numeric(tolerance)
  if (!is.null(reference_name)) parts$reference_name <- reference_name
  if (!is.null(run_id))         parts$run_id         <- run_id
  q <- if (length(parts) == 0) "{}" else jsonlite::toJSON(parts, auto_unbox = TRUE)
  .con("processing_artifacts_metadata", db_name, mongo_url)$find(q)
}



get_model_run <- function(run_id,
                          db = "MSI_database_test",
                          url = "mongodb://localhost:27018") {
  con <- mongolite::mongo(collection = "model_runs", db = db, url = url)
  doc <- con$find(query = sprintf('{"_id": "%s"}', run_id))
  if (nrow(doc) == 0) return(NULL)
  doc[1, , drop = FALSE]
}


extract_params <- function(x) {
  if (is.null(x)) return(list())
  if (is.data.frame(x)) return(as.list(x[1, , drop = FALSE]))
  if (is.list(x) && length(x) == 1 && is.list(x[[1]])) return(x[[1]])
  if (is.list(x)) return(x)
  list()
}