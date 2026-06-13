# Compare pathway scoring output against sandbox reference CSV.

suppressPackageStartupMessages({
  library(readr)
})

source("R/utils/cluster_pathways_constants.R")

sandbox_path <- "sandbox/paper A R script/pathway_scoring_all_candidates_R.csv"
candidate_path <- file.path(
  cp_step_dir("08_pathway_discovery"),
  "pathway_scoring_all_candidates.csv"
)

if (!file.exists(sandbox_path)) {
  stop("Sandbox reference not found: ", sandbox_path)
}
if (!file.exists(candidate_path)) {
  stop("Candidate output not found: ", candidate_path)
}

sandbox <- read_csv(sandbox_path, show_col_types = FALSE)
candidate <- read_csv(candidate_path, show_col_types = FALSE)

key_cols <- c("scenario", "matrix", "env_class", "endpoint")
skip_cols <- c(
  "rf_driver_support", "elastic_net_driver_support",
  "multivariable_env_support", "bootstrap_sign_stability"
)

sandbox$key <- do.call(paste, c(sandbox[key_cols], sep = "||"))
candidate$key <- do.call(paste, c(candidate[key_cols], sep = "||"))

sandbox_map <- split(sandbox, sandbox$key)
candidate_map <- split(candidate, candidate$key)

same_value <- function(a, b) {
  if (is.na(a) && is.na(b)) return(TRUE)
  if (is.numeric(a) || is.numeric(b)) {
    a_num <- suppressWarnings(as.numeric(a))
    b_num <- suppressWarnings(as.numeric(b))
    if (is.finite(a_num) && is.finite(b_num)) {
      abs_diff <- abs(a_num - b_num)
      if (abs_diff < 1e-9) return(TRUE)
      denom <- max(abs(a_num), abs(b_num), 1)
      return(abs_diff / denom < 1e-9)
    }
  }
  identical(as.character(a), as.character(b))
}

compare_cols <- setdiff(intersect(names(sandbox), names(candidate)), c(key_cols, "key", skip_cols))
diff_counts <- setNames(rep(0L, length(compare_cols)), compare_cols)
exact_rows <- 0L

for (key in names(sandbox_map)) {
  if (!key %in% names(candidate_map)) next
  sb_row <- sandbox_map[[key]][1, , drop = FALSE]
  nw_row <- candidate_map[[key]][1, , drop = FALSE]
  row_exact <- TRUE
  for (col in compare_cols) {
    if (!same_value(sb_row[[col]], nw_row[[col]])) {
      diff_counts[[col]] <- diff_counts[[col]] + 1L
      row_exact <- FALSE
    }
  }
  if (row_exact) exact_rows <- exact_rows + 1L
}

class_match <- sum(
  vapply(
    names(sandbox_map),
    function(key) {
      if (!key %in% names(candidate_map)) return(FALSE)
      identical(
        as.character(sandbox_map[[key]]$pathway_class[1]),
        as.character(candidate_map[[key]]$pathway_class[1])
      )
    },
    logical(1)
  ),
  na.rm = TRUE
)

cat("# Sandbox parity check\n\n")
cat("Exact rows:", exact_rows, "/", nrow(sandbox), "\n")
cat("Pathway class matches:", class_match, "/", nrow(sandbox), "\n\n")
cat("Pathway class counts (sandbox):\n")
print(sort(table(sandbox$pathway_class), decreasing = TRUE))
cat("\nPathway class counts (candidate):\n")
print(sort(table(candidate$pathway_class), decreasing = TRUE))
cat("\nTop differing columns:\n")
print(head(sort(diff_counts, decreasing = TRUE), 12))
