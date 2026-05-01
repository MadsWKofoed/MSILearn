source("R/mongo_schema.R")
source("R/mongo_functions.R")
source("R/alignment_reference_db.R")

assert_true <- function(condition, message) {
  if (!isTRUE(condition)) {
    stop(message, call. = FALSE)
  }
}

make_fake_collection <- function() {
  collection <- new.env(parent = emptyenv())
  collection$docs <- list()

  scalar_chr <- function(x, default = NA_character_) {
    if (is.null(x) || length(x) == 0) default else as.character(x[[1]])
  }

  scalar_num <- function(x, default = NA_real_) {
    if (is.null(x) || length(x) == 0) default else as.numeric(x[[1]])
  }

  scalar_lgl <- function(x, default = FALSE) {
    if (is.null(x) || length(x) == 0) default else as.logical(x[[1]])
  }

  collection$count <- function(query) {
    q <- if (identical(query, "{}")) list() else jsonlite::fromJSON(query, simplifyVector = TRUE)
    sum(vapply(collection$docs, function(doc) {
      if (length(q) == 0) {
        return(TRUE)
      }
      identical(as.character(doc$reference_name), as.character(q$reference_name))
    }, logical(1)))
  }

  collection$find <- function(query = "{}", fields = NULL) {
    q <- if (identical(query, "{}")) list() else jsonlite::fromJSON(query, simplifyVector = TRUE)
    docs <- Filter(function(doc) {
      if (length(q) == 0) {
        return(TRUE)
      }
      identical(as.character(doc$reference_name), as.character(q$reference_name))
    }, collection$docs)

    if (length(docs) == 0) {
      return(data.frame(stringsAsFactors = FALSE))
    }

    rows <- lapply(docs, function(doc) {
      row <- data.frame(
        `_id` = scalar_chr(doc$`_id`),
        reference_name = scalar_chr(doc$reference_name),
        display_name = scalar_chr(doc$display_name),
        filename = scalar_chr(doc$filename),
        original_filename = scalar_chr(doc$original_filename),
        file_type = scalar_chr(doc$file_type),
        file_size = scalar_num(doc$file_size),
        date_added = scalar_chr(doc$date_added),
        uploaded_at = scalar_chr(doc$uploaded_at),
        built_in = scalar_lgl(doc$built_in),
        source = scalar_chr(doc$source),
        description = scalar_chr(doc$description, ""),
        stringsAsFactors = FALSE,
        check.names = FALSE
      )
      row$mz_values <- I(list(as.numeric(unlist(doc$mz_values))))
      row$file_contents <- scalar_chr(doc$file_contents, "")
      row
    })

    do.call(rbind, rows)
  }

  collection$insert <- function(json) {
    doc <- jsonlite::fromJSON(json, simplifyVector = FALSE)
    collection$docs[[length(collection$docs) + 1L]] <- doc
    invisible(TRUE)
  }

  collection
}

write_reference_csv <- function(path, mz_values, col_name = "mz") {
  df <- setNames(data.frame(mz_values, check.names = FALSE), col_name)
  utils::write.csv(df, path, row.names = FALSE)
}

run_tests <- function() {
  tmp_dir <- tempfile("alignment_reference_tests_")
  dir.create(tmp_dir, recursive = TRUE)

  original_resource_dir <- alignment_reference_resource_dir
  on.exit({
    alignment_reference_resource_dir <<- original_resource_dir
    unlink(tmp_dir, recursive = TRUE)
  }, add = TRUE)

  collection <- make_fake_collection()

  default_files <- c(
    file.path(tmp_dir, "default_a.csv"),
    file.path(tmp_dir, "default_b.csv"),
    file.path(tmp_dir, "default_c.csv")
  )
  write_reference_csv(default_files[1], c(100.1, 200.2, 300.3))
  write_reference_csv(default_files[2], c(110.1, 210.2, 310.3), col_name = "x")
  write_reference_csv(default_files[3], c(120.1, 220.2, 320.3))

  manifest <- data.frame(
    reference_name = c("builtin_a", "builtin_b", "builtin_c"),
    display_name = c("Built-in A", "Built-in B", "Built-in C"),
    filename = basename(default_files),
    file_type = c("csv", "csv", "csv"),
    date_added = c("2026-05-01T00:00:00Z", "2026-05-01T00:00:00Z", "2026-05-01T00:00:00Z"),
    built_in = c(TRUE, TRUE, TRUE),
    description = c("A", "B", "C"),
    stringsAsFactors = FALSE
  )

  alignment_reference_resource_dir <<- function() tmp_dir

  seed_result <- seed_default_alignment_references(manifest = manifest, collection = collection)
  assert_true(nrow(seed_result) == 3L, "Fresh seed should insert three default references.")
  assert_true(sum(seed_result$status == "inserted") == 3L, "Fresh seed should mark all defaults as inserted.")

  seed_again <- seed_default_alignment_references(manifest = manifest, collection = collection)
  assert_true(sum(seed_again$status == "exists") == 3L, "Seeding twice should not duplicate defaults.")
  assert_true(nrow(list_alignment_references(collection = collection)) == 3L, "Collection should still contain three defaults after reseeding.")

  upload_path <- file.path(tmp_dir, "uploaded.csv")
  write_reference_csv(upload_path, c(400.4, 500.5, 600.6))
  uploaded <- save_uploaded_alignment_reference(
    upload_info = data.frame(name = "uploaded.csv", datapath = upload_path, stringsAsFactors = FALSE),
    display_name = "User Upload",
    collection = collection
  )
  assert_true(identical(uploaded$display_name, "User Upload"), "Uploaded reference should preserve its display name.")
  assert_true(nrow(list_alignment_references(collection = collection)) == 4L, "Uploaded reference should be saved and listed.")

  invalid_path <- file.path(tmp_dir, "empty.csv")
  file.create(invalid_path)
  invalid <- validate_alignment_reference_upload(
    data.frame(name = "empty.csv", datapath = invalid_path, stringsAsFactors = FALSE)
  )
  assert_true(!isTRUE(invalid$valid), "Empty upload should be rejected.")

  choices <- alignment_reference_choices(list_alignment_references(collection = collection))
  assert_true(any(grepl("\\[built-in\\]$", names(choices))), "Dropdown choices should label built-in references.")
  assert_true(any(grepl("\\[uploaded\\]$", names(choices))), "Dropdown choices should label uploaded references.")

  cat("Alignment reference tests passed.\n")
}

run_tests()
