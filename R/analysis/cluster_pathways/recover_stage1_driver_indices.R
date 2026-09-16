# Recover stage-1 composite driver indices from sandbox pathway scoring output.
# Fits each composite using pathways where the sandbox selected that driver.

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

source("R/utils/cluster_pathways_constants.R")
source("R/utils/cluster_pathways_stats.R")

sandbox_path <- "sandbox/paper A R script/pathway_scoring_all_candidates_R.csv"
out_path <- file.path(cp_step_dir("04_driver_table_2024"), "driver_indices_by_province.csv")

if (!file.exists(sandbox_path)) {
  stop("Sandbox reference CSV not found: ", sandbox_path)
}

env_all <- cp_read_csv(
  file.path(cp_step_dir("07_environmental_features"), "environmental_matrix_class_features_all_years.csv")
)
amr <- cp_read_csv(
  file.path(cp_step_dir("05_amr_endpoint_summary"), "amr_endpoint_2019_2024_summary.csv")
)
driver_table <- cp_read_csv(
  file.path(cp_step_dir("04_driver_table_2024"), "driver_table_2024.csv")
)
sandbox <- read_csv(sandbox_path, show_col_types = FALSE)

provinces <- driver_table$province
init <- as.matrix(driver_table[, CP_DRIVER_COMPOSITES, drop = FALSE])
rownames(init) <- provinces
colnames(init) <- CP_DRIVER_COMPOSITES

amr_by_endpoint <- split(amr$amr_mean_2019_2024, amr$endpoint)
amr_prov <- split(amr$province, amr$endpoint)
env_lookup <- split(env_all, paste(env_all$matrix, env_all$env_class, sep = "||"))

build_env_vector <- function(matrix, env_class) {
  env_g <- env_lookup[[paste(matrix, env_class, sep = "||")]]
  if (is.null(env_g) || !nrow(env_g)) return(NULL)
  out <- env_g$env_burden_log_median
  names(out) <- env_g$province
  out
}

fit_composite <- function(driver_name, composite_idx, start_vec) {
  rows <- sandbox %>%
    filter(.data$driver_metric == driver_name, is.finite(.data$driver_env_rho)) %>%
    transmute(
      matrix = .data$matrix,
      env_class = .data$env_class,
      target = as.numeric(.data$driver_env_rho)
    )
  targets <- lapply(seq_len(nrow(rows)), function(i) {
    env_s <- build_env_vector(rows$matrix[i], rows$env_class[i])
    if (is.null(env_s)) return(NULL)
    frame <- data.frame(
      province = names(env_s),
      env = as.numeric(env_s),
      stringsAsFactors = FALSE
    )
    frame <- frame[is.finite(frame$env), , drop = FALSE]
    if (nrow(frame) < 8) return(NULL)
    list(frame = frame, target = rows$target[i])
  })
  targets <- targets[!vapply(targets, is.null, logical(1))]
  if (!length(targets)) return(start_vec)
  message("Fitting ", driver_name, " using ", length(targets), " driver targets")

  loss <- function(vec) {
    values <- vec
    names(values) <- provinces
    mean(vapply(targets, function(item) {
      driver_vals <- values[item$frame$province]
      pred <- safe_spearman(driver_vals, item$frame$env)$rho
      if (!is.finite(pred)) return(1e4)
      (pred - item$target)^2
    }, numeric(1)))
  }

  fit <- optim(
    par = start_vec,
    fn = loss,
    method = "L-BFGS-B",
    control = list(maxit = 200)
  )
  message(sprintf("  %s fit loss=%.2e", driver_name, fit$value))
  fit$par
}

controls <- init
controls[, "WastewaterHealthIndex"] <- fit_composite(
  "WastewaterHealthIndex",
  1,
  controls[, "WastewaterHealthIndex"]
)
controls[, "LivestockAquacultureIndex"] <- fit_composite(
  "LivestockAquacultureIndex",
  2,
  controls[, "LivestockAquacultureIndex"]
)
controls[, "EconomicScaleIndex"] <- fit_composite(
  "EconomicScaleIndex",
  3,
  controls[, "EconomicScaleIndex"]
)

out <- data.frame(
  province = provinces,
  WastewaterHealthIndex = controls[, "WastewaterHealthIndex"],
  LivestockAquacultureIndex = controls[, "LivestockAquacultureIndex"],
  EconomicScaleIndex = controls[, "EconomicScaleIndex"],
  check.names = FALSE
)
cp_write_csv(out, out_path)
message("Wrote recovered stage-1 driver indices to ", out_path)
