# R/mongo_schema.R
# MongoDB schema initialisation and index enforcement.
# Call `initialise_schema()` once at application startup (idempotent).

DB_NAME  <- "MSI_database"
MONGO_URL <- "mongodb://localhost:27018"

# ---------------------------------------------------------------------------
# Internal connection helper (opens a fresh connection per call; mongolite
# connections are not safe to share across reactive contexts).
# ---------------------------------------------------------------------------
.con <- function(collection, db = DB_NAME, url = MONGO_URL) {
  mongolite::mongo(collection = collection, db = db, url = url)
}

# ---------------------------------------------------------------------------
# Schema initialisation: create all collections and enforce unique indexes.
# Safe to call repeatedly (indexes are created only if they do not exist).
# ---------------------------------------------------------------------------
initialise_schema <- function(db = DB_NAME, url = MONGO_URL) {

  # ── studies ──────────────────────────────────────────────────────────────
  # _id is set explicitly as a stable string, e.g. "study_SSC_v1".
  # No extra unique index needed beyond _id.

  # ── samples ──────────────────────────────────────────────────────────────
  # Unique per (study_id, sample_name).
  samples <- .con("samples", db, url)
  samples$index(
    add = '{"study_id": 1, "sample_name": 1}',
    options = '{"unique": true, "name": "uniq_study_sample"}'
  )

  # ── pipelines ─────────────────────────────────────────────────────────────
  # _id is the deterministic digest hash; unique by definition.
  # Additional unique index on (type, params_hash) guards against hash
  # collisions producing duplicate logical entries.
  pipelines <- .con("pipelines", db, url)
  pipelines$index(
    add = '{"type": 1, "params_hash": 1}',
    options = '{"unique": true, "name": "uniq_pipeline_type_hash"}'
  )

  # ── artifacts ─────────────────────────────────────────────────────────────
  # Unique per (sample_id, stage_type, pipeline_id).
  artifacts <- .con("artifacts", db, url)
  artifacts$index(
    add = '{"sample_id": 1, "stage_type": 1, "pipeline_id": 1}',
    options = '{"unique": true, "name": "uniq_artifact"}'
  )
  artifacts$index(
    add = '{"study_id": 1}',
    options = '{"name": "idx_artifact_study"}'
  )

  # ── annotation_sets ──────────────────────────────────────────────────────
  # Unique per (study_id, name).
  ann_sets <- .con("annotation_sets", db, url)
  ann_sets$index(
    add = '{"study_id": 1, "name": 1}',
    options = '{"unique": true, "name": "uniq_annset_study_name"}'
  )

  # ── annotations ──────────────────────────────────────────────────────────
  # Unique per (sample_id, annotation_set_id).
  annotations <- .con("annotations", db, url)
  annotations$index(
    add = '{"sample_id": 1, "annotation_set_id": 1}',
    options = '{"unique": true, "name": "uniq_annotation"}'
  )

  # ── datasets ──────────────────────────────────────────────────────────────
  # Frozen snapshots – no uniqueness constraint beyond _id, but index on
  # (study_id, pipeline_id, annotation_set_id) for fast lookup.
  datasets <- .con("datasets", db, url)
  datasets$index(
    add = '{"study_id": 1, "pipeline_id": 1, "annotation_set_id": 1}',
    options = '{"name": "idx_dataset_provenance"}'
  )

  # ── model_runs ────────────────────────────────────────────────────────────
  # Index on dataset_id for fast lookup of all runs trained on a dataset.
  model_runs <- .con("model_runs", db, url)
  model_runs$index(
    add = '{"dataset_id": 1}',
    options = '{"name": "idx_modelrun_dataset"}'
  )

  message("✓ MSI_database schema initialised (indexes enforced)")
  invisible(TRUE)
}
