# Step 10: supplementary sensitivity bar charts for filter calibration.

source("renv/activate.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(ggplot2)
  library(readr)
  library(tidyr)
})

source("R/utils/cluster_pathways_constants.R")
source("R/utils/cluster_pathways_supplementary_figures.R")

out_dir <- cp_step_dir("10_supplementary_figures")
pathways <- cp_read_csv(file.path(cp_step_dir("08_pathway_discovery"), "pathways_all_candidates.csv"))

write_pathway_supplementary_figures(pathways, out_dir)
message("Wrote supplementary pathway figures to ", out_dir)
