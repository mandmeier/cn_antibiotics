# Step 09: multivariable support modeling for shortlisted pathways.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

source("R/utils/cluster_pathways_constants.R")
source("R/utils/cluster_pathways_scoring.R")

out_dir <- cp_step_dir("09_pathway_multivariable_support")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

pathways <- cp_read_csv(file.path(cp_step_dir("08_pathway_discovery"), "pathway_scoring_all_candidates.csv"))
driver_table <- cp_read_csv(file.path(cp_step_dir("04_driver_table_2024"), "driver_table_2024.csv"))
amr <- cp_read_csv(file.path(cp_step_dir("05_amr_endpoint_summary"), "amr_endpoint_2019_2024_summary.csv"))
feature_consensus <- cp_read_csv(
  file.path(cp_step_dir("06_cluster_feature_models"), "cluster_feature_importance_consensus.csv")
)
env_features <- cp_read_csv(
  file.path(
    cp_step_dir("07_environmental_features"),
    "environmental_matrix_class_features_sample_year_ge_2012.csv"
  )
)

support_pack <- build_pathway_multivariable_support(
  env_features, amr, driver_table, pathways, feature_consensus
)

pathways_with_support <- sort_pathways(attach_pathway_support(pathways, support_pack$support))

cp_write_csv(support_pack$selected, file.path(out_dir, "pathway_modeling_shortlist.csv"))
cp_write_csv(support_pack$support, file.path(out_dir, "pathway_multivariable_regression_support.csv"))
cp_write_csv(pathways_with_support, file.path(out_dir, "pathway_scoring_with_support.csv"))
writeLines(support_pack$summary, file.path(out_dir, "pathway_modeling_summary.md"))

message("Wrote pathway multivariable support to ", out_dir)
