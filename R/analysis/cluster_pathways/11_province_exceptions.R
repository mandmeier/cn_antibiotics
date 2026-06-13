# Step 11: flag province-level pathway exceptions.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

source("R/utils/cluster_pathways_constants.R")
source("R/utils/cluster_pathways_scoring.R")

out_dir <- cp_step_dir("11_province_exceptions")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

pathways <- cp_read_csv(file.path(cp_step_dir("09_pathway_multivariable_support"), "pathway_scoring_with_support.csv"))
env_features <- cp_read_csv(
  file.path(
    cp_step_dir("07_environmental_features"),
    "environmental_matrix_class_features_sample_year_ge_2012.csv"
  )
)

exceptions <- build_exception_table(env_features, pathways)

cp_write_csv(exceptions, file.path(out_dir, "province_exception_flags.csv"))

message("Wrote province exceptions to ", out_dir)
