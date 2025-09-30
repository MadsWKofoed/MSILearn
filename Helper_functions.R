# --- Helpers (drop-in) ---------------------------------------------------------
sanitize_colnames <- function(nms) {
  nms <- gsub("\\.", "_", nms, perl = TRUE)
  nms <- ifelse(grepl("^\\$", nms), paste0("dollar_", sub("^\\$", "", nms)), nms)
  nms
}

normalize_for_mongo <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  # factors -> character
  is_fac  <- vapply(df, is.factor, logical(1))
  if (any(is_fac)) df[is_fac] <- lapply(df[is_fac], as.character)
  # list-cols -> scalar character
  is_list <- vapply(df, is.list, logical(1))
  if (any(is_list)) {
    df[is_list] <- lapply(df[is_list], function(col)
      vapply(col, function(x) if (length(x) == 0) NA_character_ else as.character(x[[1]]), character(1))
    )
  }
  # NaN/Inf -> NA
  is_num <- vapply(df, is.numeric, logical(1))
  if (any(is_num)) {
    df[is_num] <- lapply(df[is_num], function(x) { x[is.nan(x) | is.infinite(x)] <- NA_real_; x })
  }
  rownames(df) <- NULL
  names(df) <- sanitize_colnames(names(df))
  df
}

stream_import_to_mongo <- function(mongo_con, df) {
  # Write NDJSON file, then mongo$import(path). Avoids arg name collision with 'con'
  tmp <- tempfile(fileext = ".json")
  out <- file(tmp, open = "wt")                 # text-mode, no encoding arg
  on.exit({ try(close(out), silent = TRUE); unlink(tmp) }, add = TRUE)
  
  stopifnot(inherits(out, "connection"))        # ensure connection object
  jsonlite::stream_out(df, con = out, pagesize = 1000, verbose = FALSE)
  close(out)                                    # flush before import
  mongo_con$import(tmp)                         # path string, not a connection
}
