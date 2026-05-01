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

dbm_delete_alignment_reference_by_id <- function(reference_id, db = DB_NAME, url = MONGO_URL) {
  ref_df <- dbm_safe_find("alignment_references", sprintf('{"_id": "%s"}', reference_id), db = db, url = url)
  if (dbm_is_protected_record("alignment_references", ref_df)) {
    stop("Built-in alignment references are protected and cannot be deleted.")
  }

  list(
    alignment_references = dbm_delete_docs_with_files(
      "alignment_references",
      sprintf('{"_id": "%s"}', reference_id),
      db,
      url
    )
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
    alignment_references = dbm_delete_alignment_reference_by_id(id, db, url),
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
    alignment_references = paste0("Alignment reference: ", dbm_first_chr(r$display_name, dbm_first_chr(r$reference_name, dbm_first_chr(r$`_id`)))),
    ndpi_registrations = paste0("NDPI registration: ", dbm_first_chr(r$ndpi_slide_name, dbm_first_chr(r$`_id`)))
  )
}
