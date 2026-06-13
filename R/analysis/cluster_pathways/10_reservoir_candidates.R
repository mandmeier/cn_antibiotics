# Step 10: matrix-separated trends and descriptive reservoir candidates.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

source("R/utils/cluster_pathways_constants.R")
source("R/utils/cluster_pathways_scoring.R")

out_dir <- cp_step_dir("10_reservoir_candidates")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

pathways <- cp_read_csv(file.path(cp_step_dir("09_pathway_multivariable_support"), "pathway_scoring_with_support.csv"))
env_features <- cp_read_csv(
  file.path(
    cp_step_dir("07_environmental_features"),
    "environmental_matrix_class_features_sample_year_ge_2012.csv"
  )
)

matrix_trends <- build_matrix_trends(env_features, pathways)
reservoir <- build_reservoir_table(matrix_trends, pathways)

cp_write_csv(matrix_trends, file.path(out_dir, "matrix_separated_trend_table.csv"))
cp_write_csv(reservoir, file.path(out_dir, "reservoir_candidates_with_sparse_flag.csv"))

message("Wrote reservoir candidates to ", out_dir)
