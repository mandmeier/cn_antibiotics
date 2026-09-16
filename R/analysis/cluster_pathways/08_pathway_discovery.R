# Step 08: discover env-AMR pathway candidates (all_years).

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

source("R/utils/cluster_pathways_constants.R")
source("R/utils/cluster_pathways_discovery.R")

out_dir <- cp_step_dir("08_pathway_discovery")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

driver_table <- cp_read_csv(file.path(cp_step_dir("04_driver_table_2024"), "driver_table_2024.csv"))
amr <- cp_read_csv(file.path(cp_step_dir("05_amr_endpoint_summary"), "amr_endpoint_2019_2024_summary.csv"))
env_features <- cp_read_csv(cp_env_features_path(CP_PATHWAY_SCENARIO))

preferred_drivers <- intersect(CP_INTERPRETABLE_PATHWAY_DRIVERS, names(driver_table))

pathways <- discover_pathways(
  env_features,
  amr,
  driver_table,
  CP_PATHWAY_SCENARIO,
  preferred_drivers
)

cp_write_csv(pathways, file.path(out_dir, "pathways_all_candidates.csv"))
message("Wrote ", nrow(pathways), " pathway candidates to ", out_dir)
