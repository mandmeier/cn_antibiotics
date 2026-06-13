# Step 08: discover and score env-AMR pathway candidates.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

source("R/utils/cluster_pathways_constants.R")
source("R/utils/cluster_pathways_scoring.R")

out_dir <- cp_step_dir("08_pathway_discovery")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

driver_table <- cp_read_csv(file.path(cp_step_dir("04_driver_table_2024"), "driver_table_2024.csv"))
amr <- cp_read_csv(file.path(cp_step_dir("05_amr_endpoint_summary"), "amr_endpoint_2019_2024_summary.csv"))

preferred_drivers <- intersect(CP_INTERPRETABLE_PATHWAY_DRIVERS, names(driver_table))

pathways_all <- lapply(c("sample_year_ge_2012", "all_years"), function(scenario) {
  env_features <- cp_read_csv(
    file.path(
      cp_step_dir("07_environmental_features"),
      paste0("environmental_matrix_class_features_", scenario, ".csv")
    )
  )
  discover_pathways(env_features, amr, driver_table, scenario, preferred_drivers)
})

pathways <- bind_rows(pathways_all)
pathways <- pathways %>%
  mutate(
    rf_driver_support = "",
    elastic_net_driver_support = "",
    multivariable_env_support = "",
    bootstrap_sign_stability = NA_real_
  )

pathways <- sort_pathways(pathways)

sandbox_col_order <- c(
  "scenario", "matrix", "env_class", "endpoint", "driver_metric",
  "n_provinces", "n_clusters", "env_records_total", "env_refs_total", "n_units",
  "env_year_min", "env_year_max", "env_amr_rho", "env_amr_p",
  "partial_env_amr_rho", "partial_env_amr_p", "cluster_contrast_p",
  "cluster_contrast_epsilon", "driver_env_rho", "driver_env_p",
  "driver_amr_rho", "driver_amr_p", "coherent_direction", "loo_sign_stability",
  "loo_influence_ratio", "loo_most_influential_province", "coverage_score",
  "interpretability_score", "cluster_contrast_score", "loo_sign_stability_score",
  "driver_env_amr_coherence_score", "env_amr_q_fdr", "partial_env_amr_q_fdr",
  "matrix_consistency_score", "pathway_total_score", "pathway_class",
  "pathway_class_rank", "rf_driver_support", "elastic_net_driver_support",
  "multivariable_env_support", "bootstrap_sign_stability"
)
pathways <- pathways[, intersect(sandbox_col_order, names(pathways)), drop = FALSE]

shortlist <- pathways %>%
  filter(.data$pathway_class %in% c("Manuscript-worthy", "Strong lead"))

cp_write_csv(pathways, file.path(out_dir, "pathway_scoring_all_candidates.csv"))
cp_write_csv(shortlist, file.path(out_dir, "pathway_shortlist_manuscript_worthy.csv"))

message("Wrote pathway discovery results to ", out_dir)
