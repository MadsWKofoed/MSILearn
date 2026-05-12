# R/mongo_schema.R
# MongoDB schema initialisation and index enforcement.
# Call `initialise_schema()` once at application startup (idempotent).

DB_NAME  <- "MSI_DB"
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
  .con("samples", db, url)$index('{"study_id": 1, "sample_name": 1}')

  # ── pipelines ─────────────────────────────────────────────────────────────
  .con("pipelines", db, url)$index('{"type": 1, "params_hash": 1}')

  # ── pipeline_outputs ─────────────────────────────────────────────────────────────
  # Unique per (sample_id, stage_type, pipeline_id).
  .con("pipeline_outputs", db, url)$index('{"sample_id": 1, "stage_type": 1, "pipeline_id": 1}')
  .con("pipeline_outputs", db, url)$index('{"study_id": 1}')

  # ── ndpi_images ───────────────────────────────────────────────────────────
  .con("ndpi_images", db, url)$index('{"sample_id": 1}')
  .con("ndpi_images", db, url)$index('{"study_id": 1}')

  # ── annotation_sets ──────────────────────────────────────────────────────
  .con("annotation_sets", db, url)$index('{"study_id": 1, "name": 1}')

  # ── annotations ──────────────────────────────────────────────────────────
  .con("annotations", db, url)$index('{"sample_id": 1, "annotation_set_id": 1}')

  # ── datasets ──────────────────────────────────────────────────────────────
  .con("datasets", db, url)$index('{"study_id": 1, "pipeline_id": 1, "annotation_set_id": 1}')

  # ── model_runs ────────────────────────────────────────────────────────────
  .con("model_runs", db, url)$index('{"dataset_id": 1}')

  # ── alignment_references ─────────────────────────────────────────────────
  .con("alignment_references", db, url)$index('{"reference_name": 1}')
  .con("alignment_references", db, url)$index('{"built_in": 1, "display_name": 1}')

  message(sprintf("✓ %s schema initialised (indexes enforced)", db))
  invisible(TRUE)
}
