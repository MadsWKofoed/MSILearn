# R/mongo_functions.R

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


