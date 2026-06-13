# Step 04: build 2024 driver table (yearbook wide + stage-1 driver indices).

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
})

source("R/utils/cluster_pathways_constants.R")
source("R/utils/cluster_pathways_stats.R")

out_dir <- cp_step_dir("04_driver_table_2024")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

groups <- cp_read_csv(file.path(cp_step_dir("01_cluster_provenance"), "province_groups_with_clusters.csv"))

yb <- cp_read_csv("data/cleaned/yearbook_province_year_panel.csv") %>%
  filter(.data$province %in% CP_PROVINCES, .data$year == 2024) %>%
  mutate(value = suppressWarnings(as.numeric(.data$value)))

stage1_candidates <- c(
  "data/reference/stage1_outputs/DriverIndices_by_province.csv",
  "sandbox/paper A R script/outputs/module 1/stage1_outputs/DriverIndices_by_province.csv",
  file.path(out_dir, "driver_indices_by_province.csv")
)
stage1_path <- stage1_candidates[file.exists(stage1_candidates)][1]

metrics <- sort(unique(yb$metric[!is.na(yb$metric)]))
yb24 <- yb %>%
  select("province", "metric", "value")
agg <- yb24 %>%
  group_by(.data$province, .data$metric) %>%
  summarise(value = stats::median(.data$value, na.rm = TRUE), .groups = "drop")
wide_raw <- agg %>%
  pivot_wider(names_from = "metric", values_from = "value")
missing_metrics <- setdiff(metrics, names(wide_raw))
for (metric in missing_metrics) {
  wide_raw[[metric]] <- NA_real_
}
wide_raw <- wide_raw[, c("province", metrics), drop = FALSE]

if (!is.na(stage1_path)) {
  stage1_drivers <- cp_read_csv(stage1_path)
  driver_table <- merge(
    wide_raw,
    stage1_drivers,
    by = "province",
    all.x = TRUE,
    suffixes = c("", "_stage1")
  )
  message("Merged stage-1 driver indices from ", stage1_path)
} else {
  driver_table <- wide_raw
  for (metric in names(CP_DRIVER_METRIC_ALIASES)) {
    alias <- CP_DRIVER_METRIC_ALIASES[[metric]]
    if (metric %in% names(driver_table)) {
      driver_table[[alias]] <- driver_table[[metric]]
    }
  }
  driver_table <- driver_table %>%
    build_composite_index(CP_WASTEWATER_HEALTH_COMPONENTS, "WastewaterHealthIndex") %>%
    build_composite_index(CP_LIVESTOCK_AQUACULTURE_COMPONENTS, "LivestockAquacultureIndex") %>%
    build_composite_index(CP_ECONOMIC_SCALE_COMPONENTS, "EconomicScaleIndex")
  message(
    "No stage-1 driver indices found; using yearbook-derived aliases and composite indices."
  )
}

driver_table <- driver_table %>%
  inner_join(groups %>% select("province", "cluster_id", "cluster_name"), by = "province")

composite_doc <- bind_rows(
  tibble(composite_index = "WastewaterHealthIndex", component_metric = CP_WASTEWATER_HEALTH_COMPONENTS),
  tibble(composite_index = "LivestockAquacultureIndex", component_metric = CP_LIVESTOCK_AQUACULTURE_COMPONENTS),
  tibble(composite_index = "EconomicScaleIndex", component_metric = CP_ECONOMIC_SCALE_COMPONENTS)
)

cp_write_csv(composite_doc, file.path(out_dir, "driver_composites.csv"))
cp_write_csv(driver_table, file.path(out_dir, "driver_table_2024.csv"))

message("Wrote driver table to ", out_dir)
