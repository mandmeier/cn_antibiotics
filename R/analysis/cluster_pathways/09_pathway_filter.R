# Step 09: apply fixed pathway filters and write analysis-ready shortlist.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

source("R/utils/cluster_pathways_constants.R")
source("R/utils/cluster_pathways_discovery.R")

out_dir <- cp_step_dir("09_pathway_filter")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

pathways <- cp_read_csv(file.path(cp_step_dir("08_pathway_discovery"), "pathways_all_candidates.csv"))
filtered <- apply_pathway_filters(pathways)
summary_tbl <- build_filter_summary(pathways)
matrix_counts <- filtered %>%
  dplyr::count(.data$matrix, name = "n_surviving") %>%
  dplyr::arrange(.data$matrix)

cp_write_csv(filtered, file.path(out_dir, "pathways_filtered.csv"))
cp_write_csv(summary_tbl, file.path(out_dir, "filter_summary.csv"))
cp_write_csv(matrix_counts, file.path(out_dir, "filter_counts_by_matrix.csv"))

filter_md <- c(
  "# Pathway filter summary",
  "",
  "Figure 3 simple screening logic (aligned with `build_figure3_simple_screening_block.R`).",
  "",
  paste0(
    "- Primary scenario: `", CP_PATHWAY_SCENARIO, "`",
    "\n- Matrices: `", paste(CP_FIGURE3_MATRICES, collapse = "`, `"), "`",
    "\n- Exclude `", paste(CP_EXCLUDED_PATHWAY_ENV_CLASSES, collapse = "`, `"), "`",
    "\n- Class-concordant pairs only",
    "\n- `n_provinces >= ", CP_MIN_PROVINCES, "`",
    "\n- Rank by `n_provinces`, then `|env_amr_rho|`, then `|partial_env_amr_rho|`, then LOO label",
    "\n- Top `", CP_SIMPLE_FIG3_ROWS_PER_MATRIX, "` per matrix **(disabled; full pool exported)**"
  ),
  "",
  paste0("- All candidates: **", nrow(pathways), "**"),
  paste0("- Class-concordant (after env/matrix gates): **", nrow(apply_pathway_base_filters(pathways)), "**"),
  paste0("- Filter pool (+ ", CP_MIN_PROVINCES, "+ provinces): **", nrow(build_filter_pool(pathways)), "**"),
  paste0("- Filtered pathways: **", nrow(filtered), "**"),
  "",
  "## Survivors per matrix",
  "",
  apply(
    matrix_counts,
    1,
    function(row) paste0("- ", row[["matrix"]], ": **", row[["n_surviving"]], "**"),
    simplify = TRUE
  ),
  ""
)
writeLines(filter_md, file.path(out_dir, "filter_summary.md"))

message(
  "Wrote ", nrow(filtered), " filtered pathways (of ", nrow(pathways),
  " candidates) to ", out_dir
)
