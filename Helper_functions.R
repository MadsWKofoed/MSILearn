normalize_annotation_df <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  # Factors -> character
  is_fac <- vapply(df, is.factor, logical(1))
  if (any(is_fac)) df[is_fac] <- lapply(df[is_fac], as.character)
  # Numeric: NaN/Inf -> NA
  is_num <- vapply(df, is.numeric, logical(1))
  if (any(is_num)) {
    df[is_num] <- lapply(df[is_num], function(x) { x[is.nan(x) | is.infinite(x)] <- NA_real_; x })
  }
  rownames(df) <- NULL
  df
}

df_rows_to_json <- function(df) {
  vapply(seq_len(nrow(df)), function(i) {
    jsonlite::toJSON(
      as.list(df[i, , drop = FALSE]),
      auto_unbox = TRUE, na = "null", null = "null", POSIXt = "ISO8601", digits = NA
    )
  }, character(1))
}

insert_dataframe_chunked <- function(con, df, chunk = 20000) {
  n <- nrow(df); if (!n) return(invisible(NULL))
  idx <- split(seq_len(n), ceiling(seq_len(n)/chunk))
  for (ids in idx) con$insert(df[ids, , drop = FALSE])
  invisible(NULL)
}

insert_json_chunked <- function(con, json_vec, chunk = 20000) {
  n <- length(json_vec); if (!n) return(invisible(NULL))
  idx <- split(seq_len(n), ceiling(seq_len(n)/chunk))
  for (ids in idx) con$insert(json_vec[ids])
  invisible(NULL)
}