# file: Helper_functions.R
# --- Helpers (drop-in) ---------------------------------------------------------
null_if_na <- function(x) {
  # Why: mongolite prefers NULL over NA inside lists
  if (length(x) == 1 && (is.na(x) || is.nan(x) || is.infinite(x))) return(NULL)
  x
}

normalize_scalar_cols <- function(df) {
  df <- as.data.frame(df, stringsAsFactors = FALSE, check.names = FALSE)
  is_fac <- vapply(df, is.factor, logical(1))
  if (any(is_fac)) df[is_fac] <- lapply(df[is_fac], as.character)
  is_num <- vapply(df, is.numeric, logical(1))
  if (any(is_num)) {
    df[is_num] <- lapply(df[is_num], function(x) { x[is.nan(x) | is.infinite(x)] <- NA_real_; x })
  }
  rownames(df) <- NULL
  df
}

to_docs <- function(df_small) {
  # list of named lists, NA -> NULL (per-field)
  lapply(seq_len(nrow(df_small)), function(i) {
    r <- as.list(df_small[i, , drop = FALSE])
    for (nm in names(r)) r[[nm]] <- null_if_na(r[[nm]])
    r
  })
}

insert_docs_batched <- function(con, docs, batch_size = 5000) {
  n <- length(docs); if (!n) return(invisible(NULL))
  starts <- seq(1, n, by = batch_size)
  for (s in starts) {
    e <- min(s + batch_size - 1, n)
    con$insert(docs[s:e])  # list of documents
  }
  invisible(NULL)
}
