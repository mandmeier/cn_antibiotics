# Helpers so pipeline CSV writes can byte-match prior artifacts.

# Rscript attaches default packages after .Rprofile, leaving stats::filter ahead of
# conflicted's .conflicts. Re-assert before any bare filter() calls in pipeline scripts.
if (requireNamespace("conflicted", quietly = TRUE) && "package:conflicted" %in% search()) {
  suppressMessages(conflicted::conflicts_prefer(dplyr::filter))
}

# Format thresholds the way base R does under default scipen (e.g. 1e+06).
format_sci <- function(x) {
  format(x, scientific = TRUE, digits = 1, trim = TRUE)
}

# TRUE when new_df matches old_df for pipeline purposes (exact non-numerics;
# doubles compared with a tight relative tolerance).
dataframe_equivalent <- function(new_df, old_df, tol = 1e-9, ignore_cols = NULL) {
  if (!is.null(ignore_cols)) {
    keep_new <- setdiff(names(new_df), ignore_cols)
    keep_old <- setdiff(names(old_df), ignore_cols)
    new_df <- new_df[keep_new]
    old_df <- old_df[intersect(keep_old, keep_new)]
  }

  if (nrow(new_df) != nrow(old_df) || !identical(names(new_df), names(old_df))) {
    return(FALSE)
  }

  for (col in names(new_df)) {
    a <- new_df[[col]]
    b <- old_df[[col]]

    if (is.numeric(a) || is.numeric(b) || is.factor(a) || is.factor(b)) {
      a_num <- suppressWarnings(as.numeric(as.character(a)))
      b_num <- suppressWarnings(as.numeric(as.character(b)))
      # Fall back to character compare when not numeric (e.g. factor labels).
      if (all(is.na(a_num) == is.na(a)) && all(is.na(b_num) == is.na(b)) &&
            !all(is.na(a_num)) && !all(is.na(b_num))) {
        if (!isTRUE(all.equal(a_num, b_num, tolerance = tol, check.attributes = FALSE))) {
          return(FALSE)
        }
        next
      }
    }

    a_chr <- as.character(a)
    b_chr <- as.character(b)
    a_chr[is.na(a)] <- NA_character_
    b_chr[is.na(b)] <- NA_character_
    if (!identical(a_chr, b_chr)) {
      return(FALSE)
    }
  }

  TRUE
}

# Write CSV, but leave an existing file untouched when content is equivalent so
# float serialization stays byte-stable across re-runs.
# ignore_cols: columns present in an existing file that this writer does not own.
write_csv_reproducible <- function(df, path, ..., ignore_cols = NULL) {
  if (file.exists(path)) {
    old_df <- tryCatch(
      readr::read_csv(path, show_col_types = FALSE),
      error = function(e) NULL
    )
    if (!is.null(old_df) && dataframe_equivalent(df, old_df, ignore_cols = ignore_cols)) {
      return(invisible(path))
    }
  }

  readr::write_csv(df, path, ...)
}
