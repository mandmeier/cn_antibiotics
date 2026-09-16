# Step 01: document fixed k=3 provincial cluster assignments.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

source("R/utils/cluster_pathways_constants.R")

out_dir <- cp_step_dir("01_cluster_provenance")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

groups <- load_province_groups()

provenance <- groups %>%
  arrange(.data$cluster_id, .data$province) %>%
  group_by(.data$cluster_id) %>%
  summarise(
    cluster_name = .data$cluster_name[1],
    n_provinces = n(),
    provinces = paste(.data$province, collapse = "; "),
    fixed_assignment_source = "data/cleaned/province_groups.csv:k3_groups",
    module1_2_role = "fixed policy-facing stratum for descriptive signal discovery",
    interpretation_caution = paste(
      "not reclustered and not treated as causal or immutable latent type"
    ),
    .groups = "drop"
  )

counts <- groups %>%
  count(.data$cluster_id, name = "n") %>%
  arrange(.data$cluster_id)

cp_write_csv(provenance, file.path(out_dir, "cluster_provenance.csv"))
cp_write_csv(counts, file.path(out_dir, "frozen_k3_counts.csv"))
cp_write_csv(groups, file.path(out_dir, "province_groups_with_clusters.csv"))

message("Wrote cluster provenance to ", out_dir)
