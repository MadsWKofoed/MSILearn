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
      "alignment_references", "ndpi_registrations"
    ),
    label = c(
      "Studies", "Samples", "Pipelines", "Artifacts",
      "Annotation sets", "Annotations", "Datasets", "Model runs",
      "Alignment references", "NDPI registrations"
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
      "Built-in and uploaded alignment reference peak lists.",
      "Saved NDPI→MSI registration metadata."
    ),
    stringsAsFactors = FALSE
  )
}

dbm_deleteable_collections <- function() dbm_catalog()$key

dbm_requires_study_filter <- function(collection) {
  collection %in% c("samples", "annotation_sets")
}

dbm_supports_study_filter <- function(collection) {
  collection %in% c("samples", "annotation_sets", "artifacts", "annotations", "datasets", "ndpi_registrations")
}

dbm_supports_sample_filter <- function(collection) {
  collection %in% c("artifacts", "annotations", "ndpi_registrations")
}

# ---- Small generic helpers -------------------------------------------------

col_or_default <- function(df, col, default = "") {
  n <- nrow(df)
  if (col %in% names(df)) {
    as.character(df[[col]])
  } else {
    rep(default, n)
  }
}

dbm_null_to_na <- function(x) {
  if (is.null(x) || length(x) == 0) return(NA)
  x[[1]]
}

dbm_first_chr <- function(x, default = "—") {
  if (is.null(x) || length(x) == 0) return(default)
  out <- as.character(x[[1]])
  if (!nzchar(out)) default else out
}

dbm_record_id <- function(collection, record) {
  if (is.null(record) || !is.data.frame(record) || nrow(record) == 0) {
    return(NA_character_)
  }

  r <- record[1, , drop = FALSE]
  candidates <- c(
    if ("_id" %in% names(r)) as.character(r$`_id`[1]) else NA_character_,
    if ("id" %in% names(r)) as.character(r$id[1]) else NA_character_,
    switch(
      collection,
      studies = if ("study_id" %in% names(r)) as.character(r$study_id[1]) else NA_character_,
      samples = if ("sample_id" %in% names(r)) as.character(r$sample_id[1]) else NA_character_,
      datasets = if ("dataset_id" %in% names(r)) as.character(r$dataset_id[1]) else NA_character_,
      model_runs = if ("run_id" %in% names(r)) as.character(r$run_id[1]) else NA_character_,
      annotations = if ("annotation_id" %in% names(r)) as.character(r$annotation_id[1]) else NA_character_,
      ndpi_registrations = if ("registration_id" %in% names(r)) as.character(r$registration_id[1]) else NA_character_,
      NA_character_
    )
  )

  candidates <- candidates[!is.na(candidates) & nzchar(candidates)]
  if (length(candidates) == 0) NA_character_ else candidates[1]
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

dbm_filter_index <- function(collection, db = DB_NAME, url = MONGO_URL) {
  empty <- data.frame(
    study_id = character(0),
    study_label = character(0),
    sample_id = character(0),
    sample_label = character(0),
    stringsAsFactors = FALSE
  )

  studies_df <- tryCatch(get_studies(db = db, url = url), error = function(e) data.frame(stringsAsFactors = FALSE))
  study_map <- setNames(character(0), character(0))
  if (nrow(studies_df) > 0 && all(c("_id", "name") %in% names(studies_df))) {
    study_map <- setNames(as.character(studies_df$name), as.character(studies_df$`_id`))
  }

  sample_df <- dbm_safe_find("samples", db = db, url = url)
  sample_meta <- empty
  if (nrow(sample_df) > 0 && all(c("_id", "sample_name", "study_id") %in% names(sample_df))) {
    sample_meta <- data.frame(
      study_id = as.character(sample_df$study_id),
      study_label = unname(study_map[as.character(sample_df$study_id)]),
      sample_id = as.character(sample_df$`_id`),
      sample_label = as.character(sample_df$sample_name),
      stringsAsFactors = FALSE
    )
    sample_meta$study_label[is.na(sample_meta$study_label) | !nzchar(sample_meta$study_label)] <- sample_meta$study_id[is.na(sample_meta$study_label) | !nzchar(sample_meta$study_label)]
    sample_meta$sample_label[is.na(sample_meta$sample_label) | !nzchar(sample_meta$sample_label)] <- sample_meta$sample_id[is.na(sample_meta$sample_label) | !nzchar(sample_meta$sample_label)]
    sample_meta <- unique(sample_meta)
  }

  sample_rows_from_ids <- function(sample_ids, fallback_study_ids = NULL) {
    sample_ids <- as.character(sample_ids %||% character(0))
    sample_ids <- sample_ids[nzchar(sample_ids)]
    if (length(sample_ids) == 0) {
      return(empty)
    }

    out <- data.frame(
      study_id = rep("", length(sample_ids)),
      study_label = rep("", length(sample_ids)),
      sample_id = sample_ids,
      sample_label = sample_ids,
      stringsAsFactors = FALSE
    )

    if (nrow(sample_meta) > 0) {
      idx <- match(sample_ids, sample_meta$sample_id)
      matched <- !is.na(idx)
      out$study_id[matched] <- sample_meta$study_id[idx[matched]]
      out$study_label[matched] <- sample_meta$study_label[idx[matched]]
      out$sample_label[matched] <- sample_meta$sample_label[idx[matched]]
    }

    if (!is.null(fallback_study_ids) && length(fallback_study_ids) == length(sample_ids)) {
      missing_study <- !nzchar(out$study_id)
      out$study_id[missing_study] <- as.character(fallback_study_ids[missing_study])
    }

    missing_study_label <- !nzchar(out$study_label) & nzchar(out$study_id)
    out$study_label[missing_study_label] <- unname(study_map[out$study_id[missing_study_label]])
    out$study_label[!nzchar(out$study_label)] <- out$study_id[!nzchar(out$study_label)]

    unique(out)
  }

  study_rows_from_ids <- function(study_ids) {
    study_ids <- unique(as.character(study_ids %||% character(0)))
    study_ids <- study_ids[nzchar(study_ids)]
    if (length(study_ids) == 0) {
      return(empty)
    }

    out <- data.frame(
      study_id = study_ids,
      study_label = unname(study_map[study_ids]),
      sample_id = rep("", length(study_ids)),
      sample_label = rep("", length(study_ids)),
      stringsAsFactors = FALSE
    )
    out$study_label[is.na(out$study_label) | !nzchar(out$study_label)] <- out$study_id[is.na(out$study_label) | !nzchar(out$study_label)]
    unique(out)
  }

  out <- switch(
    collection,
    studies = study_rows_from_ids(names(study_map)),
    samples = sample_meta,
    annotation_sets = {
      ann_df <- dbm_safe_find("annotation_sets", db = db, url = url)
      study_rows_from_ids(ann_df$study_id)
    },
    artifacts = {
      art_df <- dbm_safe_find("artifacts", db = db, url = url)
      sample_rows_from_ids(art_df$sample_id, fallback_study_ids = art_df$study_id)
    },
    annotations = {
      ann_df <- dbm_safe_find("annotations", db = db, url = url)
      sample_rows_from_ids(ann_df$sample_id)
    },
    datasets = {
      ds_df <- tryCatch(list_datasets(db = db, url = url), error = function(e) data.frame(stringsAsFactors = FALSE))
      study_rows_from_ids(ds_df$study_id)
    },
    ndpi_registrations = {
      ndpi_df <- dbm_safe_find("ndpi_registrations", db = db, url = url)
      sample_rows_from_ids(ndpi_df$sample_id)
    },
    empty
  )

  if (is.null(out) || nrow(out) == 0) {
    return(empty)
  }

  out[] <- lapply(out, function(col) {
    val <- as.character(col)
    val[is.na(val)] <- ""
    val
  })

  unique(out)
}

dbm_alignment_reference_stats <- function(db = DB_NAME, url = MONGO_URL) {
  refs <- tryCatch(
    list_alignment_references(db = db, url = url),
    error = function(e) data.frame(stringsAsFactors = FALSE)
  )

  if (nrow(refs) == 0) {
    return(list(
      total = 0L,
      built_in = 0L,
      uploaded = 0L,
      total_bytes = 0
    ))
  }

  file_sizes <- suppressWarnings(as.numeric(refs$file_size))
  file_sizes[!is.finite(file_sizes)] <- 0

  list(
    total = nrow(refs),
    built_in = sum(refs$built_in %in% TRUE, na.rm = TRUE),
    uploaded = sum(!(refs$built_in %in% TRUE), na.rm = TRUE),
    total_bytes = sum(file_sizes, na.rm = TRUE)
  )
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
    studies = tryCatch(get_studies(db = db, url = url), error = function(e) data.frame()),
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
    alignment_references = list_alignment_references(db = db, url = url),
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
      study_id_vec <- if ("_id" %in% names(df)) {
        as.character(df$`_id`)
      } else if ("study_id" %in% names(df)) {
        as.character(df$study_id)
      } else {
        rep("", nrow(df))
      }

      show <- data.frame(
        id = study_id_vec,
        name = col_or_default(df, "name"),
        description = col_or_default(df, "description"),
        created_at = col_or_default(df, "created_at"),
        stringsAsFactors = FALSE
      )
    },
    samples = {
      show <- data.frame(
        id = col_or_default(df, "_id"),
        sample = col_or_default(df, "sample_name"),
        study = lookup(col_or_default(df, "study_id"), study_map),
        created_at = col_or_default(df, "created_at"),
        stringsAsFactors = FALSE
      )
    },
    pipelines = {
      show <- data.frame(
        id = col_or_default(df, "_id"),
        type = col_or_default(df, "type"),
        name = col_or_default(df, "name"),
        code_version = col_or_default(df, "code_version"),
        created_at = col_or_default(df, "created_at"),
        stringsAsFactors = FALSE
      )
    },
    artifacts = {
      show <- data.frame(
        id = col_or_default(df, "_id"),
        study = lookup(col_or_default(df, "study_id"), study_map),
        sample = lookup(col_or_default(df, "sample_id"), sample_map),
        stage = col_or_default(df, "stage_type"),
        pipeline = lookup(col_or_default(df, "pipeline_id"), pipe_map),
        created_at = col_or_default(df, "created_at"),
        stringsAsFactors = FALSE
      )
    },
    annotation_sets = {
      show <- data.frame(
        id = col_or_default(df, "_id"),
        name = col_or_default(df, "name"),
        study = lookup(col_or_default(df, "study_id"), study_map),
        created_at = col_or_default(df, "created_at"),
        stringsAsFactors = FALSE
      )
    },
    annotations = {
      show <- data.frame(
        id = col_or_default(df, "_id"),
        sample = lookup(col_or_default(df, "sample_id"), sample_map),
        annotation_set = lookup(col_or_default(df, "annotation_set_id"), annset_map),
        format = col_or_default(df, "format"),
        created_at = col_or_default(df, "created_at"),
        stringsAsFactors = FALSE
      )
    },
    datasets = {
      show <- data.frame(
        id = col_or_default(df, "_id"),
        name = col_or_default(df, "name"),
        study = lookup(col_or_default(df, "study_id"), study_map),
        pipeline = lookup(col_or_default(df, "pipeline_id"), pipe_map),
        annotation_set = lookup(col_or_default(df, "annotation_set_id"), annset_map),
        stage = col_or_default(df, "stage_type"),
        created_at = col_or_default(df, "created_at"),
        stringsAsFactors = FALSE
      )
    },
    model_runs = {
      show <- data.frame(
        id = col_or_default(df, "_id"),
        dataset = lookup(col_or_default(df, "dataset_id"), dataset_map),
        model_type = col_or_default(df, "model_type"),
        created_at = col_or_default(df, "created_at"),
        stringsAsFactors = FALSE
      )
    },
    alignment_references = {
      built_in_vec <- if ("built_in" %in% names(df)) as.logical(df$built_in) else rep(FALSE, nrow(df))
      source_vec <- ifelse(
        built_in_vec,
        "Built-in",
        "Uploaded"
      )
      file_size_vec <- if ("file_size" %in% names(df)) suppressWarnings(as.numeric(df$file_size)) else rep(NA_real_, nrow(df))
      file_size_vec[!is.finite(file_size_vec)] <- NA_real_
      file_size_lbl <- vapply(file_size_vec, dbm_bytes_label, character(1))
      name_vec <- if ("display_name" %in% names(df)) as.character(df$display_name) else rep("", nrow(df))
      ref_name_vec <- if ("reference_name" %in% names(df)) as.character(df$reference_name) else rep("", nrow(df))
      name_vec[is.na(name_vec) | !nzchar(name_vec)] <- ref_name_vec[is.na(name_vec) | !nzchar(name_vec)]
      created_vec <- if ("date_added" %in% names(df)) as.character(df$date_added) else rep("", nrow(df))
      uploaded_vec <- if ("uploaded_at" %in% names(df)) as.character(df$uploaded_at) else rep("", nrow(df))
      created_vec[is.na(created_vec) | !nzchar(created_vec)] <- uploaded_vec[is.na(created_vec) | !nzchar(created_vec)]
      show <- data.frame(
        id = col_or_default(df, "_id"),
        name = name_vec,
        source = source_vec,
        file_type = col_or_default(df, "file_type"),
        file_size = file_size_lbl,
        created_at = created_vec,
        stringsAsFactors = FALSE
      )
    },
    ndpi_registrations = {
      show <- data.frame(
        id = if ("_id" %in% names(df)) col_or_default(df, "_id") else as.character(seq_len(nrow(df))),
        sample = lookup(col_or_default(df, "sample_id"), sample_map),
        pipeline = lookup(col_or_default(df, "pipeline_id"), pipe_map),
        slide = col_or_default(df, "ndpi_slide_name"),
        rms = col_or_default(df, "rms"),
        created_at = col_or_default(df, "created_at"),
        stringsAsFactors = FALSE
      )
    }
  )

  show
}

# ---- One-record fetch for details pane ------------------------------------

dbm_fetch_record <- function(collection, id, db = DB_NAME, url = MONGO_URL) {
  if (is.null(id) || !nzchar(id)) return(NULL)
  q <- sprintf('{"_id": "%s"}', id)
  df <- dbm_safe_find(collection, q, db = db, url = url)
  if (nrow(df) > 0) return(df[1, , drop = FALSE])
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

dbm_extract_scalar <- function(x, name) {
  if (is.null(x)) return(NA_real_)

  if (is.data.frame(x) && name %in% names(x) && nrow(x) > 0) {
    return(suppressWarnings(as.numeric(x[[name]][1])))
  }

  if (is.list(x) && !is.null(x[[name]])) {
    return(suppressWarnings(as.numeric(unlist(x[[name]])[1])))
  }

  if (!is.null(names(x)) && name %in% names(x)) {
    return(suppressWarnings(as.numeric(x[[name]][1])))
  }

  NA_real_
}

dbm_database_stats <- function(db = DB_NAME, url = MONGO_URL) {
  stats <- tryCatch(
    .con("studies", db, url)$run('{"dbStats": 1, "scale": 1}'),
    error = function(e) NULL
  )

  list(
    data_size_bytes = dbm_extract_scalar(stats, "dataSize"),
    storage_size_bytes = dbm_extract_scalar(stats, "storageSize"),
    index_size_bytes = dbm_extract_scalar(stats, "indexSize")
  )
}

dbm_gridfs_stats <- function(db = DB_NAME, url = MONGO_URL) {
  files_df <- tryCatch(
    .con("fs.files", db, url)$find("{}", fields = '{"length": 1, "filename": 1}'),
    error = function(e) data.frame(stringsAsFactors = FALSE)
  )

  if (is.null(files_df) || nrow(files_df) == 0 || !("length" %in% names(files_df))) {
    return(list(
      file_count = 0L,
      total_bytes = 0,
      average_bytes = 0
    ))
  }

  file_lengths <- suppressWarnings(as.numeric(files_df$length))
  file_lengths[!is.finite(file_lengths)] <- 0

  list(
    file_count = nrow(files_df),
    total_bytes = sum(file_lengths, na.rm = TRUE),
    average_bytes = if (nrow(files_df) > 0) mean(file_lengths, na.rm = TRUE) else 0
  )
}

dbm_domain_mix <- function(counts_df) {
  if (is.null(counts_df) || !is.data.frame(counts_df) || nrow(counts_df) == 0) {
    return(data.frame(domain = character(0), records = numeric(0), stringsAsFactors = FALSE))
  }

  domain_map <- c(
    studies = "Study setup",
    samples = "Study setup",
    pipelines = "Processing",
    artifacts = "Processing",
    ndpi_registrations = "Processing",
    annotation_sets = "Annotation",
    annotations = "Annotation",
    datasets = "Modeling",
    model_runs = "Modeling",
    alignment_references = "References"
  )

  df <- counts_df
  df$domain <- unname(domain_map[df$key])
  df <- df[!is.na(df$domain) & nzchar(df$domain), c("domain", "records"), drop = FALSE]

  if (nrow(df) == 0) {
    return(data.frame(domain = character(0), records = numeric(0), stringsAsFactors = FALSE))
  }

  out <- stats::aggregate(records ~ domain, data = df, sum, na.rm = TRUE)
  domain_order <- c("Study setup", "Processing", "Annotation", "Modeling", "References")
  out$domain <- factor(out$domain, levels = domain_order)
  out <- out[order(out$domain), , drop = FALSE]
  out$domain <- as.character(out$domain)
  out
}

dbm_overview_stats <- function(db = DB_NAME, url = MONGO_URL) {
  counts <- dbm_collection_counts(db = db, url = url)
  ref_stats <- dbm_alignment_reference_stats(db = db, url = url)
  db_stats <- dbm_database_stats(db = db, url = url)
  gridfs_stats <- dbm_gridfs_stats(db = db, url = url)

  active_collections <- sum(counts$records > 0, na.rm = TRUE)
  total_records <- sum(counts$records, na.rm = TRUE)
  largest_records <- if (nrow(counts) > 0) max(counts$records, na.rm = TRUE) else 0
  data_size <- db_stats$data_size_bytes
  index_size <- db_stats$index_size_bytes
  storage_size <- db_stats$storage_size_bytes
  managed_file_bytes <- gridfs_stats$total_bytes + ref_stats$total_bytes

  total_db_bytes <- if (is.finite(storage_size) && !is.na(storage_size)) {
    storage_size + if (is.finite(index_size) && !is.na(index_size)) index_size else 0
  } else if (is.finite(data_size) && !is.na(data_size)) {
    data_size + if (is.finite(index_size) && !is.na(index_size)) index_size else 0
  } else {
    managed_file_bytes
  }

  list(
    total_records = total_records,
    non_empty_collections = active_collections,
    largest_collection = if (nrow(counts) > 0) counts$collection[which.max(counts$records)][1] else NA_character_,
    largest_collection_records = largest_records,
    largest_collection_share = if (total_records > 0) largest_records / total_records else 0,
    average_records_per_active_collection = if (active_collections > 0) total_records / active_collections else 0,
    total_db_bytes = total_db_bytes,
    data_size_bytes = data_size,
    storage_size_bytes = storage_size,
    index_size_bytes = index_size,
    managed_file_bytes = managed_file_bytes,
    gridfs_file_count = gridfs_stats$file_count,
    gridfs_total_bytes = gridfs_stats$total_bytes,
    alignment_reference_total = ref_stats$total,
    alignment_reference_uploaded = ref_stats$uploaded,
    alignment_reference_built_in = ref_stats$built_in,
    alignment_reference_total_bytes = ref_stats$total_bytes
  )
}

dbm_bytes_label <- function(x) {
  x <- suppressWarnings(as.numeric(x))
  if (!length(x) || is.na(x) || !is.finite(x)) return("—")
  format(structure(x, class = "object_size"), units = "auto")
}

dbm_is_protected_record <- function(collection, record) {
  if (!identical(collection, "alignment_references")) {
    return(FALSE)
  }

  if (is.null(record) || nrow(record) == 0) {
    return(FALSE)
  }

  built_in <- if ("built_in" %in% names(record)) isTRUE(record$built_in[1]) else FALSE
  source <- if ("source" %in% names(record)) as.character(record$source[1]) else ""
  built_in || identical(source, "built_in")
}

dbm_delete_policy <- function(collection, record = NULL) {
  if (dbm_is_protected_record(collection, record)) {
    return(list(
      allowed = FALSE,
      reason = "Built-in alignment references are protected and cannot be deleted.",
      label = "Protected built-in reference"
    ))
  }

  label <- switch(
    collection,
    studies = "Delete selected study",
    alignment_references = "Delete selected reference",
    datasets = "Delete selected dataset",
    model_runs = "Delete selected model",
    annotations = "Delete selected annotation",
    annotation_sets = "Delete selected annotation set",
    artifacts = "Delete selected artifact",
    samples = "Delete selected sample",
    pipelines = "Delete selected pipeline",
    "Delete selected record"
  )

  list(
    allowed = collection %in% dbm_deleteable_collections(),
    reason = if (identical(collection, "alignment_references")) {
      "Uploaded alignment references can be deleted. Built-in defaults remain protected."
    } else {
      ""
    },
    label = label
  )
}

# ---- Deletion helpers ------------------------------------------------------

dbm_internal_collections <- function() {
  c("processing_artifacts_metadata", "clustering_metadata")
}

dbm_collection_label <- function(collection) {
  labels <- c(
    stats::setNames(dbm_catalog()$label, dbm_catalog()$key),
    processing_artifacts_metadata = "Legacy processing metadata",
    clustering_metadata = "Clustering metadata"
  )
  unname(labels[[collection]] %||% collection)
}

dbm_all_delete_collections <- function() {
  c(dbm_catalog()$key, dbm_internal_collections())
}

dbm_nonempty_chr <- function(x) {
  x <- as.character(unlist(x, recursive = TRUE, use.names = FALSE))
  x[!is.na(x) & nzchar(x)]
}

dbm_extract_field_values <- function(df, field) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0 || !(field %in% names(df))) {
    return(character(0))
  }

  vals <- df[[field]]
  if (is.list(vals)) {
    unique(dbm_nonempty_chr(vals))
  } else {
    unique(dbm_nonempty_chr(vals))
  }
}

dbm_compact_selector <- function(selector) {
  if (length(selector) == 0) return(NULL)

  keep <- vapply(selector, function(val) {
    if (is.null(val) || length(val) == 0) return(FALSE)
    if (is.list(val) && !is.data.frame(val)) return(TRUE)
    any(!is.na(as.character(val)) & nzchar(as.character(val)))
  }, logical(1))

  selector <- selector[keep]
  if (length(selector) == 0) return(NULL)

  for (nm in names(selector)) {
    val <- selector[[nm]]
    if (!is.list(val) || is.data.frame(val)) {
      selector[[nm]] <- as.character(val[[1]])
    }
  }

  selector
}

dbm_safe_find_list <- function(collection, query = list(), fields = NULL,
                               db = DB_NAME, url = MONGO_URL) {
  dbm_safe_find(
    collection = collection,
    query = dbm_query_from_list(query),
    fields = fields,
    db = db,
    url = url
  )
}

dbm_record_selector <- function(collection, record) {
  if (is.null(record) || !is.data.frame(record) || nrow(record) == 0) {
    return(NULL)
  }

  r <- record[1, , drop = FALSE]
  if ("_id" %in% names(r)) {
    rid <- as.character(r$`_id`[1])
    if (!is.na(rid) && nzchar(rid)) {
      return(list(`_id` = rid))
    }
  }

  selector <- switch(
    collection,
    studies = list(name = as.character(r$name[1] %||% "")),
    samples = list(
      study_id = as.character(r$study_id[1] %||% ""),
      sample_name = as.character(r$sample_name[1] %||% "")
    ),
    pipelines = list(
      type = as.character(r$type[1] %||% ""),
      params_hash = as.character(r$params_hash[1] %||% "")
    ),
    artifacts = list(
      sample_id = as.character(r$sample_id[1] %||% ""),
      stage_type = as.character(r$stage_type[1] %||% ""),
      pipeline_id = as.character(r$pipeline_id[1] %||% "")
    ),
    annotation_sets = list(
      study_id = as.character(r$study_id[1] %||% ""),
      name = as.character(r$name[1] %||% "")
    ),
    annotations = list(
      sample_id = as.character(r$sample_id[1] %||% ""),
      annotation_set_id = as.character(r$annotation_set_id[1] %||% "")
    ),
    datasets = list(
      study_id = as.character(r$study_id[1] %||% ""),
      pipeline_id = as.character(r$pipeline_id[1] %||% ""),
      annotation_set_id = as.character(r$annotation_set_id[1] %||% ""),
      created_at = as.character(r$created_at[1] %||% "")
    ),
    model_runs = list(
      dataset_id = as.character(r$dataset_id[1] %||% ""),
      created_at = as.character(r$created_at[1] %||% "")
    ),
    alignment_references = list(
      reference_name = as.character(r$reference_name[1] %||% ""),
      display_name = as.character(r$display_name[1] %||% "")
    ),
    ndpi_registrations = list(
      sample_id = as.character(r$sample_id[1] %||% ""),
      pipeline_id = as.character(r$pipeline_id[1] %||% ""),
      ndpi_slide_name = as.character(r$ndpi_slide_name[1] %||% ""),
      created_at = as.character(r$created_at[1] %||% "")
    ),
    processing_artifacts_metadata = list(
      sample_name = as.character(r$sample_name[1] %||% ""),
      stage_type = as.character(r$stage_type[1] %||% ""),
      run_id = as.character(r$run_id[1] %||% ""),
      filename = as.character(r$filename[1] %||% ""),
      imzml_gridfs_name = as.character(r$imzml_gridfs_name[1] %||% ""),
      ibd_gridfs_name = as.character(r$ibd_gridfs_name[1] %||% ""),
      created_at = as.character(r$created_at[1] %||% "")
    ),
    clustering_metadata = list(
      sample_id = as.character(r$sample_id[1] %||% ""),
      pipeline_id = as.character(r$pipeline_id[1] %||% ""),
      created_at = as.character(r$created_at[1] %||% "")
    ),
    NULL
  )

  dbm_compact_selector(selector)
}

dbm_record_key <- function(collection, record) {
  selector <- dbm_record_selector(collection, record)
  if (is.null(selector)) {
    return(NA_character_)
  }
  jsonlite::toJSON(
    list(collection = collection, selector = selector),
    auto_unbox = TRUE,
    null = "null"
  )
}

dbm_dedupe_records <- function(df, collection) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) {
    return(data.frame(stringsAsFactors = FALSE))
  }

  keys <- vapply(seq_len(nrow(df)), function(i) {
    dbm_record_key(collection, df[i, , drop = FALSE])
  }, character(1))

  keep <- !is.na(keys) & nzchar(keys) & !duplicated(keys)
  out <- df[keep, , drop = FALSE]
  out$.dbm_key <- keys[keep]
  out
}

dbm_plan_empty_collections <- function() {
  stats::setNames(rep(list(data.frame(stringsAsFactors = FALSE)), length(dbm_all_delete_collections())),
                  dbm_all_delete_collections())
}

dbm_plan_add_records <- function(plan, collection, records) {
  empty <- data.frame(stringsAsFactors = FALSE)
  deduped <- dbm_dedupe_records(records, collection)
  if (nrow(deduped) == 0) {
    return(list(plan = plan, added = empty))
  }

  existing <- plan$collections[[collection]]
  if (is.null(existing) || !is.data.frame(existing) || nrow(existing) == 0) {
    plan$collections[[collection]] <- deduped
    return(list(plan = plan, added = deduped))
  }

  if (!(".dbm_key" %in% names(existing))) {
    existing <- dbm_dedupe_records(existing, collection)
  }

  new_rows <- deduped[!(deduped$.dbm_key %in% existing$.dbm_key), , drop = FALSE]
  plan$collections[[collection]] <- if (nrow(new_rows) > 0) {
    dplyr::bind_rows(existing, new_rows)
  } else {
    existing
  }

  list(plan = plan, added = new_rows)
}

dbm_dataset_has_sample <- function(dataset_sample_ids, sample_ids) {
  vals <- dbm_nonempty_chr(dataset_sample_ids)
  length(intersect(vals, sample_ids)) > 0
}

dbm_find_datasets <- function(sample_ids = character(0),
                              pipeline_ids = character(0),
                              annotation_set_ids = character(0),
                              stage_types = character(0),
                              study_ids = character(0),
                              db = DB_NAME,
                              url = MONGO_URL) {
  ds <- tryCatch(list_datasets(db = db, url = url), error = function(e) data.frame(stringsAsFactors = FALSE))
  if (nrow(ds) == 0) {
    return(ds)
  }

  keep <- rep(TRUE, nrow(ds))

  if (length(sample_ids) > 0 && "sample_ids" %in% names(ds)) {
    keep <- keep & vapply(ds$sample_ids, dbm_dataset_has_sample, logical(1), sample_ids = sample_ids)
  }

  if (length(pipeline_ids) > 0 && "pipeline_id" %in% names(ds)) {
    keep <- keep & (as.character(ds$pipeline_id) %in% pipeline_ids)
  }

  if (length(annotation_set_ids) > 0 && "annotation_set_id" %in% names(ds)) {
    keep <- keep & (as.character(ds$annotation_set_id) %in% annotation_set_ids)
  }

  if (length(stage_types) > 0 && "stage_type" %in% names(ds)) {
    keep <- keep & (as.character(ds$stage_type) %in% stage_types)
  }

  if (length(study_ids) > 0 && "study_id" %in% names(ds)) {
    keep <- keep & (as.character(ds$study_id) %in% study_ids)
  }

  ds[keep, , drop = FALSE]
}

dbm_collect_datasets_from_artifacts <- function(artifacts_df, db = DB_NAME, url = MONGO_URL) {
  sample_ids <- dbm_extract_field_values(artifacts_df, "sample_id")
  pipeline_ids <- dbm_extract_field_values(artifacts_df, "pipeline_id")
  stage_types <- dbm_extract_field_values(artifacts_df, "stage_type")
  dbm_find_datasets(
    sample_ids = sample_ids,
    pipeline_ids = pipeline_ids,
    stage_types = stage_types,
    db = db,
    url = url
  )
}

dbm_collect_datasets_from_annotations <- function(annotations_df, db = DB_NAME, url = MONGO_URL) {
  sample_ids <- dbm_extract_field_values(annotations_df, "sample_id")
  annset_ids <- dbm_extract_field_values(annotations_df, "annotation_set_id")
  dbm_find_datasets(
    sample_ids = sample_ids,
    annotation_set_ids = annset_ids,
    db = db,
    url = url
  )
}

dbm_dependency_rules <- function() {
  list(
    studies = list(
      list(child = "samples", query = function(df) list(study_id = list(`$in` = as.list(dbm_extract_field_values(df, "_id"))))),
      list(child = "annotation_sets", query = function(df) list(study_id = list(`$in` = as.list(dbm_extract_field_values(df, "_id"))))),
      list(child = "datasets", query = function(df) list(study_id = list(`$in` = as.list(dbm_extract_field_values(df, "_id"))))),
      list(child = "artifacts", query = function(df) list(study_id = list(`$in` = as.list(dbm_extract_field_values(df, "_id")))))
    ),
    samples = list(
      list(child = "artifacts", query = function(df) list(sample_id = list(`$in` = as.list(dbm_extract_field_values(df, "_id"))))),
      list(child = "annotations", query = function(df) list(sample_id = list(`$in` = as.list(dbm_extract_field_values(df, "_id"))))),
      list(child = "ndpi_registrations", query = function(df) list(sample_id = list(`$in` = as.list(dbm_extract_field_values(df, "_id"))))),
      list(child = "processing_artifacts_metadata", query = function(df) {
        sample_names <- dbm_extract_field_values(df, "sample_name")
        if (length(sample_names) == 0) return(NULL)
        list(sample_name = list(`$in` = as.list(sample_names)))
      }),
      list(child = "clustering_metadata", query = function(df) list(sample_id = list(`$in` = as.list(dbm_extract_field_values(df, "_id"))))),
      list(child = "datasets", collect = function(df, db, url) {
        dbm_find_datasets(sample_ids = dbm_extract_field_values(df, "_id"), db = db, url = url)
      })
    ),
    pipelines = list(
      list(child = "artifacts", query = function(df) list(pipeline_id = list(`$in` = as.list(dbm_extract_field_values(df, "_id"))))),
      list(child = "datasets", query = function(df) list(pipeline_id = list(`$in` = as.list(dbm_extract_field_values(df, "_id"))))),
      list(child = "ndpi_registrations", query = function(df) list(pipeline_id = list(`$in` = as.list(dbm_extract_field_values(df, "_id"))))),
      list(child = "clustering_metadata", query = function(df) list(pipeline_id = list(`$in` = as.list(dbm_extract_field_values(df, "_id")))))
    ),
    artifacts = list(
      list(child = "datasets", collect = function(df, db, url) {
        dbm_collect_datasets_from_artifacts(df, db = db, url = url)
      })
    ),
    annotation_sets = list(
      list(child = "annotations", query = function(df) list(annotation_set_id = list(`$in` = as.list(dbm_extract_field_values(df, "_id"))))),
      list(child = "datasets", query = function(df) list(annotation_set_id = list(`$in` = as.list(dbm_extract_field_values(df, "_id")))))
    ),
    annotations = list(
      list(child = "datasets", collect = function(df, db, url) {
        dbm_collect_datasets_from_annotations(df, db = db, url = url)
      })
    ),
    datasets = list(
      list(child = "model_runs", query = function(df) list(dataset_id = list(`$in` = as.list(dbm_extract_field_values(df, "_id")))))
    )
  )
}

dbm_collect_child_records <- function(rule, parent_records, db = DB_NAME, url = MONGO_URL) {
  if (!is.null(rule$collect)) {
    return(rule$collect(parent_records, db = db, url = url))
  }

  query <- rule$query(parent_records)
  if (is.null(query) || length(query) == 0) {
    return(data.frame(stringsAsFactors = FALSE))
  }

  dbm_safe_find_list(rule$child, query = query, db = db, url = url)
}

dbm_plan_counts <- function(plan) {
  cols <- names(plan$collections %||% list())
  if (length(cols) == 0) {
    return(setNames(numeric(0), character(0)))
  }

  counts <- vapply(cols, function(collection) {
    df <- plan$collections[[collection]]
    if (is.null(df) || !is.data.frame(df)) 0L else nrow(df)
  }, numeric(1))
  counts[counts > 0]
}

dbm_plan_summary_table <- function(plan) {
  counts <- dbm_plan_counts(plan)
  if (length(counts) == 0) {
    return(data.frame(collection = character(0), label = character(0), count = integer(0), stringsAsFactors = FALSE))
  }

  data.frame(
    collection = names(counts),
    label = vapply(names(counts), dbm_collection_label, character(1)),
    count = as.integer(counts),
    stringsAsFactors = FALSE
  )
}

dbm_plan_delete_order <- function() {
  c(
    "model_runs",
    "datasets",
    "annotations",
    "artifacts",
    "ndpi_registrations",
    "processing_artifacts_metadata",
    "clustering_metadata",
    "annotation_sets",
    "samples",
    "pipelines",
    "studies",
    "alignment_references"
  )
}

dbm_plan_deletion <- function(collection, id = NULL, record = NULL,
                              db = DB_NAME, url = MONGO_URL) {
  stopifnot(collection %in% dbm_catalog()$key)

  if (is.null(record) || !is.data.frame(record) || nrow(record) == 0) {
    if (!is.null(id) && nzchar(id)) {
      record <- dbm_fetch_record(collection, id, db = db, url = url)
    }
  }

  requested_id <- if (!is.null(id) && nzchar(id)) id else dbm_record_id(collection, record)
  requested_title <- dbm_record_title(collection, record)

  plan <- list(
    requested = list(
      collection = collection,
      collection_label = dbm_collection_label(collection),
      id = requested_id %||% "",
      title = requested_title %||% collection,
      found = !is.null(record) && is.data.frame(record) && nrow(record) > 0
    ),
    collections = dbm_plan_empty_collections(),
    skipped = data.frame(
      collection = character(0),
      label = character(0),
      reason = character(0),
      stringsAsFactors = FALSE
    ),
    errors = character(0),
    allowed = TRUE,
    reason = NULL
  )

  if (!isTRUE(plan$requested$found)) {
    plan$allowed <- FALSE
    plan$reason <- "The selected object no longer exists in the database."
    return(plan)
  }

  if (dbm_is_protected_record(collection, record)) {
    plan$allowed <- FALSE
    plan$reason <- "Built-in alignment references are protected and cannot be deleted."
    plan$skipped <- data.frame(
      collection = collection,
      label = requested_title,
      reason = plan$reason,
      stringsAsFactors = FALSE
    )
    return(plan)
  }

  added <- dbm_plan_add_records(plan, collection, record)
  plan <- added$plan
  queue <- list(list(collection = collection, records = added$added))
  rules <- dbm_dependency_rules()

  while (length(queue) > 0) {
    current <- queue[[1]]
    queue <- queue[-1]

    current_rules <- rules[[current$collection]]
    if (is.null(current_rules) || nrow(current$records) == 0) {
      next
    }

    for (rule in current_rules) {
      inspect_error <- NULL
      child_df <- tryCatch(
        dbm_collect_child_records(rule, current$records, db = db, url = url),
        error = function(e) {
          inspect_error <<- paste0("Failed to inspect ", dbm_collection_label(rule$child), ": ", conditionMessage(e))
          data.frame(stringsAsFactors = FALSE)
        }
      )
      if (!is.null(inspect_error)) {
        plan$errors <- c(plan$errors, inspect_error)
      }

      result <- dbm_plan_add_records(plan, rule$child, child_df)
      plan <- result$plan
      if (nrow(result$added) > 0) {
        queue[[length(queue) + 1L]] <- list(collection = rule$child, records = result$added)
      }
    }
  }

  plan
}

dbm_delete_single_record <- function(collection, record, db = DB_NAME, url = MONGO_URL) {
  selector <- dbm_record_selector(collection, record)
  label <- dbm_record_title(collection, record)

  if (is.null(selector)) {
    return(list(
      status = "error",
      count = 0L,
      label = label,
      reason = "No reliable selector could be built for this record."
    ))
  }

  if (dbm_is_protected_record(collection, record)) {
    return(list(
      status = "skipped",
      count = 0L,
      label = label,
      reason = "Built-in alignment references are protected and cannot be deleted."
    ))
  }

  docs <- dbm_safe_find_list(collection, selector, db = db, url = url)
  if (nrow(docs) == 0) {
    return(list(
      status = "missing",
      count = 0L,
      label = label,
      reason = "The object was already missing when deletion ran."
    ))
  }

  file_fields <- dbm_candidate_file_fields(docs)
  if (length(file_fields) > 0) {
    for (ff in file_fields) {
      vals <- docs[[ff]]
      for (i in seq_along(vals)) {
        dbm_remove_gridfs_file(vals[[i]], db = db, url = url)
      }
    }
  }

  tryCatch(
    .con(collection, db, url)$remove(dbm_query_from_list(selector)),
    error = function(e) stop(e)
  )

  list(
    status = "deleted",
    count = nrow(docs),
    label = label,
    reason = NULL
  )
}

dbm_execute_deletion_plan <- function(plan, db = DB_NAME, url = MONGO_URL) {
  report <- list(
    requested = plan$requested,
    planned = dbm_plan_summary_table(plan),
    deleted = stats::setNames(numeric(0), character(0)),
    skipped = plan$skipped,
    errors = plan$errors %||% character(0),
    total_deleted = 0L,
    reason = plan$reason %||% NULL
  )

  if (!isTRUE(plan$requested$found)) {
    report$reason <- plan$reason %||% "The requested object could not be found."
    return(report)
  }

  if (!isTRUE(plan$allowed)) {
    report$reason <- plan$reason %||% "Deletion is blocked for this object."
    return(report)
  }

  order <- dbm_plan_delete_order()
  present <- names(plan$collections)[vapply(plan$collections, nrow, numeric(1)) > 0]
  order <- c(order[order %in% present], setdiff(present, order))

  for (collection in order) {
    docs <- plan$collections[[collection]]
    if (is.null(docs) || !is.data.frame(docs) || nrow(docs) == 0) {
      next
    }

    deleted_here <- 0L

    for (i in seq_len(nrow(docs))) {
      result <- tryCatch(
        dbm_delete_single_record(collection, docs[i, , drop = FALSE], db = db, url = url),
        error = function(e) list(
          status = "error",
          count = 0L,
          label = dbm_record_title(collection, docs[i, , drop = FALSE]),
          reason = conditionMessage(e)
        )
      )

      if (identical(result$status, "deleted")) {
        deleted_here <- deleted_here + as.integer(result$count %||% 0L)
      } else if (identical(result$status, "skipped")) {
        report$skipped <- dplyr::bind_rows(
          report$skipped,
          data.frame(
            collection = collection,
            label = result$label %||% dbm_collection_label(collection),
            reason = result$reason %||% "Skipped.",
            stringsAsFactors = FALSE
          )
        )
      } else if (!is.null(result$reason) && nzchar(result$reason)) {
        report$errors <- c(
          report$errors,
          paste0(dbm_collection_label(collection), " (", result$label %||% "record", "): ", result$reason)
        )
      }
    }

    if (deleted_here > 0) {
      report$deleted[[collection]] <- deleted_here
    }
  }

  report$total_deleted <- sum(as.numeric(report$deleted), na.rm = TRUE)

  if (report$total_deleted == 0 && !length(report$errors) && nrow(report$skipped) == 0) {
    report$reason <- "No matching documents were found to delete."
  }

  report
}

dbm_delete_record <- function(collection, id = NULL, record = NULL,
                              db = DB_NAME, url = MONGO_URL) {
  plan <- dbm_plan_deletion(
    collection = collection,
    id = id,
    record = record,
    db = db,
    url = url
  )
  dbm_execute_deletion_plan(plan, db = db, url = url)
}

dbm_delete_report_lines <- function(report) {
  if (is.null(report) || length(report) == 0) {
    return("No deletion report is available.")
  }

  requested <- report$requested %||% list()
  requested_collection <- requested$collection_label %||% dbm_collection_label(requested$collection %||% "record")
  requested_title <- requested$title %||% "Unknown object"
  requested_id <- as.character(requested$id %||% "")

  lines <- c(
    paste0("Requested type: ", requested_collection),
    paste0("Requested object: ", requested_title),
    paste0("Requested ID: ", if (nzchar(requested_id)) requested_id else "—")
  )

  deleted <- report$deleted %||% numeric(0)
  if (length(deleted) > 0) {
    delete_lines <- paste0(
      "Deleted from ",
      vapply(names(deleted), dbm_collection_label, character(1)),
      ": ",
      as.integer(deleted)
    )
    lines <- c(lines, delete_lines)
  } else {
    lines <- c(lines, paste0("Deleted objects: 0", if (!is.null(report$reason) && nzchar(report$reason)) paste0(" (", report$reason, ")") else ""))
  }

  skipped <- report$skipped
  if (is.data.frame(skipped) && nrow(skipped) > 0) {
    skip_lines <- paste0(
      "Skipped ",
      vapply(skipped$collection, dbm_collection_label, character(1)),
      ": ",
      skipped$label,
      " (",
      skipped$reason,
      ")"
    )
    lines <- c(lines, skip_lines)
  }

  if (length(report$errors %||% character(0)) > 0) {
    lines <- c(lines, paste0("Error: ", report$errors))
  }

  lines
}

dbm_delete_report_text <- function(report) {
  paste(dbm_delete_report_lines(report), collapse = " | ")
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
    alignment_references = paste0("Alignment reference: ", dbm_first_chr(r$display_name, dbm_first_chr(r$reference_name, dbm_first_chr(r$`_id`)))),
    ndpi_registrations = paste0("NDPI registration: ", dbm_first_chr(r$ndpi_slide_name, dbm_first_chr(r$`_id`)))
  )
}
