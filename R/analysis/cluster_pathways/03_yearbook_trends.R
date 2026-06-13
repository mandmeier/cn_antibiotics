# Step 03: province and cluster yearbook trends (2019-2024).

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

source("R/utils/cluster_pathways_constants.R")

out_dir <- cp_step_dir("03_yearbook_trends")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

groups <- cp_read_csv(file.path(cp_step_dir("01_cluster_provenance"), "province_groups_with_clusters.csv"))
screen <- cp_read_csv(file.path(cp_step_dir("02_yearbook_cluster_screen"), "yearbook_2024_cluster_discriminators.csv"))

yb <- cp_read_csv("data/cleaned/yearbook_province_year_panel.csv") %>%
  filter(
    .data$province %in% CP_PROVINCES,
    .data$year >= 2019,
    .data$year <= 2024
  ) %>%
  mutate(value = suppressWarnings(as.numeric(.data$value))) %>%
  filter(is.finite(.data$value))

split_rows <- split(yb, paste(yb$province, yb$metric, sep = "||"))
prov_rows <- list()
for (nm in names(split_rows)) {
  g <- split_rows[[nm]]
  g <- g[order(g$year), , drop = FALSE]
  if (length(unique(g$year)) < 3) next
  years <- as.numeric(g$year)
  vals <- as.numeric(g$value)
  slope <- if (length(unique(years)) >= 2) stats::coef(stats::lm(vals ~ years))[2] else NA_real_
  start <- vals[1]
  end <- vals[length(vals)]
  pct_change <- if (abs(start) > 1e-12) (end - start) / abs(start) else NA_real_
  diffs <- diff(vals)
  slope_sign <- if (is.finite(slope)) sign(slope) else 0
  consistency <- if (slope_sign == 0 || !length(diffs)) {
    NA_real_
  } else {
    nonzero <- diffs[diffs != 0]
    if (length(nonzero)) mean(sign(nonzero) == slope_sign) else NA_real_
  }
  prov_rows[[length(prov_rows) + 1]] <- data.frame(
    province = g$province[1],
    metric = g$metric[1],
    n_years = length(unique(g$year)),
    year_min = min(g$year),
    year_max = max(g$year),
    mean_2019_2024 = mean(vals),
    slope_per_year = slope,
    percent_change_first_last = pct_change,
    direction = if (isTRUE(slope > 0)) "increasing" else if (isTRUE(slope < 0)) "decreasing" else "flat",
    direction_consistency = consistency,
    stringsAsFactors = FALSE
  )
}

prov_trends <- if (length(prov_rows)) {
  bind_rows(prov_rows) %>%
    inner_join(groups %>% select("province", "cluster_id", "cluster_name"), by = "province")
} else {
  tibble()
}

trend <- prov_trends %>%
  group_by(.data$metric, .data$cluster_id, .data$cluster_name) %>%
  summarise(
    n_provinces = dplyr::n_distinct(.data$province),
    median_mean_2019_2024 = stats::median(.data$mean_2019_2024, na.rm = TRUE),
    median_slope_per_year = stats::median(.data$slope_per_year, na.rm = TRUE),
    median_percent_change = stats::median(.data$percent_change_first_last, na.rm = TRUE),
    dominant_direction = names(sort(table(.data$direction), decreasing = TRUE))[1],
    median_direction_consistency = stats::median(.data$direction_consistency, na.rm = TRUE),
    .groups = "drop"
  )

top_map <- stats::setNames(screen$top_cluster_id, screen$metric)
disagreement_rows <- list()
if (nrow(trend) && nrow(screen)) {
  for (metric in unique(trend$metric)) {
    tsub <- trend %>% filter(.data$metric == !!metric)
    slopes <- tsub$median_slope_per_year
    names(slopes) <- tsub$cluster_id
    slopes <- slopes[is.finite(slopes)]
    top <- top_map[[metric]]
    if (!is.finite(top) || !length(slopes) || !(as.character(top) %in% names(slopes))) next
    trend_top <- as.integer(names(slopes)[which.max(slopes)])
    trend_bottom <- as.integer(names(slopes)[which.min(slopes)])
    if (as.integer(top) != trend_top && abs(max(slopes) - min(slopes)) > 1e-12) {
      disagreement_rows[[length(disagreement_rows) + 1]] <- data.frame(
        metric = metric,
        identity_top_cluster_id_2024 = as.integer(top),
        identity_top_cluster_name_2024 = unname(CP_CLUSTER_NAMES[as.character(as.integer(top))]),
        trend_fastest_increase_cluster_id = trend_top,
        trend_fastest_increase_cluster_name = unname(CP_CLUSTER_NAMES[as.character(trend_top)]),
        trend_fastest_decrease_cluster_id = trend_bottom,
        trend_fastest_decrease_cluster_name = unname(CP_CLUSTER_NAMES[as.character(trend_bottom)]),
        disagreement_note = paste(
          "2024 highest-median cluster is not the cluster with the steepest 2019-2024 increase."
        ),
        stringsAsFactors = FALSE
      )
    }
  }
}
disagreement <- if (length(disagreement_rows)) bind_rows(disagreement_rows) else tibble()

cp_write_csv(trend, file.path(out_dir, "yearbook_2019_2024_trend_support.csv"))
cp_write_csv(disagreement, file.path(out_dir, "yearbook_identity_trend_disagreement.csv"))

message("Wrote yearbook trends to ", out_dir)
