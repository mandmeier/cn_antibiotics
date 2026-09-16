# Step 05: weighted AMR endpoint summaries (2019-2024) by province.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

source("R/utils/cluster_pathways_constants.R")
source("R/utils/cluster_pathways_endpoints.R")

out_dir <- cp_step_dir("05_amr_endpoint_summary")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

groups <- cp_read_csv(file.path(cp_step_dir("01_cluster_provenance"), "province_groups_with_clusters.csv"))

res <- cp_read_csv("data/cleaned/resistance_clean.csv") %>%
  filter(
    .data$province %in% CP_PROVINCES,
    .data$year >= 2019,
    .data$year <= 2024
  ) %>%
  mutate(
    endpoint = paste(.data$bacteria_species, .data$antibiotic, sep = " | "),
    total_n_strains = suppressWarnings(as.numeric(.data$total_n_strains)),
    resistant_percent = suppressWarnings(as.numeric(.data$resistant_percent))
  ) %>%
  filter(is.finite(.data$resistant_percent), is.finite(.data$total_n_strains), .data$total_n_strains > 0)

amr <- res %>%
  group_by(.data$province, .data$endpoint) %>%
  summarise(
    bacteria_species = .data$bacteria_species[1],
    amr_antibiotic = .data$antibiotic[1],
    amr_class = endpoint_lookup_class(.data$antibiotic[1]),
    amr_mean_2019_2024 = stats::weighted.mean(.data$resistant_percent, .data$total_n_strains),
    amr_2019_2024_change = {
      v2019 <- .data$resistant_percent[.data$year == 2019]
      v2024 <- .data$resistant_percent[.data$year == 2024]
      if (length(v2019) && length(v2024)) v2024[1] - v2019[1] else NA_real_
    },
    total_strains_2019_2024 = sum(.data$total_n_strains),
    n_years = dplyr::n_distinct(.data$year),
    sentinel_endpoint = .data$endpoint[1] %in% CP_SENTINEL_ENDPOINTS,
    .groups = "drop"
  ) %>%
  inner_join(groups %>% select("province", "cluster_id", "cluster_name"), by = "province")

cp_write_csv(amr, file.path(out_dir, "amr_endpoint_2019_2024_summary.csv"))

message("Wrote AMR endpoint summary to ", out_dir)
