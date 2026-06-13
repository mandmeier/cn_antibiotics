# Run the full cluster-pathways Module 1.2 pipeline (steps 01-12).

steps <- c(
  "R/analysis/cluster_pathways/01_cluster_provenance.R",
  "R/analysis/cluster_pathways/02_yearbook_cluster_screen.R",
  "R/analysis/cluster_pathways/03_yearbook_trends.R",
  "R/analysis/cluster_pathways/04_driver_table_2024.R",
  "R/analysis/cluster_pathways/05_amr_endpoint_summary.R",
  "R/analysis/cluster_pathways/06_cluster_feature_models.R",
  "R/analysis/cluster_pathways/07_environmental_features.R",
  "R/analysis/cluster_pathways/08_pathway_discovery.R",
  "R/analysis/cluster_pathways/09_pathway_multivariable_support.R",
  "R/analysis/cluster_pathways/10_reservoir_candidates.R",
  "R/analysis/cluster_pathways/11_province_exceptions.R",
  "R/analysis/cluster_pathways/12_signal_validation.R"
)

for (step in steps) {
  message("Running ", step)
  source(step, local = new.env(parent = globalenv()))
}

message("Cluster pathways pipeline complete.")
