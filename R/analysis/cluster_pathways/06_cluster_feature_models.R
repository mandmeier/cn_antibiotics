# Step 06: random forest and elastic-net cluster identity feature models.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

source("R/utils/cluster_pathways_constants.R")
source("R/utils/cluster_pathways_feature_models.R")

out_dir <- cp_step_dir("06_cluster_feature_models")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

groups <- cp_read_csv(file.path(cp_step_dir("01_cluster_provenance"), "province_groups_with_clusters.csv"))
screen <- cp_read_csv(file.path(cp_step_dir("02_yearbook_cluster_screen"), "yearbook_2024_cluster_discriminators.csv"))

yb <- cp_read_csv("data/cleaned/yearbook_province_year_panel.csv") %>%
  filter(.data$province %in% CP_PROVINCES) %>%
  mutate(value = suppressWarnings(as.numeric(.data$value)))

models <- build_cluster_feature_models(yb, groups, screen)

cp_write_csv(models$performance, file.path(out_dir, "cluster_identity_model_performance.csv"))
cp_write_csv(models$rf_importance, file.path(out_dir, "cluster_feature_importance_random_forest.csv"))
cp_write_csv(models$elastic_net, file.path(out_dir, "cluster_feature_selection_elastic_net.csv"))
cp_write_csv(models$consensus, file.path(out_dir, "cluster_feature_importance_consensus.csv"))

message("Wrote cluster feature models to ", out_dir)
