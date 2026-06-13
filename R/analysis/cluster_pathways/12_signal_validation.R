# Step 12: validate and audit main-text pathway signals.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

source("R/utils/cluster_pathways_constants.R")
source("R/utils/cluster_pathways_scoring.R")

out_dir <- cp_step_dir("12_signal_validation")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

pathways <- cp_read_csv(file.path(cp_step_dir("09_pathway_multivariable_support"), "pathway_scoring_with_support.csv"))
exceptions <- cp_read_csv(file.path(cp_step_dir("11_province_exceptions"), "province_exception_flags.csv"))

current_signals <- load_current_maintext_signals(pathways)
interpretation <- build_interpretation_validation(current_signals, pathways, exceptions)
promotion_audit <- build_signal_promotion_audit(interpretation)
validated <- promotion_audit %>%
  filter(.data$final_label %in% c("validated lead", "conditional lead"))

cp_write_csv(current_signals, file.path(out_dir, "current_maintext_signals.csv"))
cp_write_csv(interpretation, file.path(out_dir, "interpretation_validation_summary.csv"))
cp_write_csv(promotion_audit, file.path(out_dir, "signal_promotion_audit.csv"))
cp_write_csv(validated, file.path(out_dir, "validated_maintext_signals.csv"))

message("Wrote signal validation outputs to ", out_dir)
