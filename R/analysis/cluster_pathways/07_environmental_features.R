# Step 07: province x matrix x antibiotic-class environmental burden features.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

source("R/utils/cluster_pathways_constants.R")
source("R/utils/cluster_pathways_endpoints.R")

out_dir <- cp_step_dir("07_environmental_features")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

groups <- cp_read_csv(file.path(cp_step_dir("01_cluster_provenance"), "province_groups_with_clusters.csv"))

build_env_features <- function(groups, scenario) {
  env <- cp_read_csv("data/cleaned/environmental_cleaned.csv") %>%
    filter(.data$province %in% CP_PROVINCES) %>%
    mutate(
      matrix = trimws(tolower(as.character(.data$matrix))),
      env_class = env_class_norm(if ("group_of_antibiotic" %in% names(.)) {
        .data$group_of_antibiotic
      } else {
        .data$antibiotic_class
      }),
      concentration = suppressWarnings(as.numeric(.data$mean_concentration))
    ) %>%
    filter(.data$matrix %in% CP_ENV_MATRICES)

  if (identical(scenario, "sample_year_ge_2012")) {
    env <- env %>%
      mutate(sample_year_num = suppressWarnings(as.numeric(.data$sample_year))) %>%
      filter(is.finite(.data$sample_year_num), .data$sample_year_num >= 2012)
  }

  if ("max_concentration" %in% names(env)) {
    env <- env %>%
      mutate(
        concentration = ifelse(
          is.finite(.data$concentration),
          .data$concentration,
          suppressWarnings(as.numeric(.data$max_concentration))
        )
      )
  }

  env <- env %>%
    filter(is.finite(.data$concentration), .data$concentration >= 0) %>%
    mutate(log_concentration = log10(.data$concentration + 1))

  ref_col <- if ("full_reference_name" %in% names(env)) "full_reference_name" else "reference_number"

  env %>%
    group_by(.data$province, .data$matrix, .data$env_class) %>%
    summarise(
      env_burden_log_median = stats::median(.data$log_concentration, na.rm = TRUE),
      env_burden_log_p90 = as.numeric(stats::quantile(.data$log_concentration, 0.90, na.rm = TRUE)),
      env_records_total = dplyr::n(),
      env_refs = dplyr::n_distinct(.data[[ref_col]]),
      env_year_min = if (any(is.finite(suppressWarnings(as.numeric(.data$sample_year))))) {
        min(suppressWarnings(as.numeric(.data$sample_year)), na.rm = TRUE)
      } else {
        NA_real_
      },
      env_year_max = if (any(is.finite(suppressWarnings(as.numeric(.data$sample_year))))) {
        max(suppressWarnings(as.numeric(.data$sample_year)), na.rm = TRUE)
      } else {
        NA_real_
      },
      concentration_unit_mode = {
        units <- na.omit(as.character(.data$concentration_unit))
        if (length(units)) names(sort(table(units), decreasing = TRUE))[1] else ""
      },
      n_units = dplyr::n_distinct(na.omit(as.character(.data$concentration_unit))),
      .groups = "drop"
    ) %>%
    inner_join(groups %>% select("province", "cluster_id", "cluster_name"), by = "province")
}

for (scenario in c("sample_year_ge_2012", "all_years")) {
  features <- build_env_features(groups, scenario)
  cp_write_csv(
    features,
    file.path(out_dir, paste0("environmental_matrix_class_features_", scenario, ".csv"))
  )
}

message("Wrote environmental features to ", out_dir)
