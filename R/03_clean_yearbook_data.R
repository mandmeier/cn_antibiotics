# Harmonize yearbook metric names and units.

source("R/utils/yearbook_metrics.R")
source("R/utils/reproducible_csv.R")

yearbook <- read_csv("data/raw/yearbook_data/yearbook_data_combined.csv")

audit_yearbook_metrics(yearbook, label = "before cleaning")

yearbook_clean <- yearbook %>%
  # Match CARSS / environmental naming (raw yearbook uses Xizang).
  mutate(province = dplyr::recode(province, "Xizang" = "Tibet")) %>%
  mutate(metric = vapply(metric, harmonize_yearbook_metric, character(1))) %>%
  apply_yearbook_unit_parsing() %>%
  apply_yearbook_unit_parsing()

n_metrics_after <- n_distinct(yearbook_clean$metric)
cat("\nunique metrics after cleaning:", n_metrics_after, "\n")

audit_yearbook_metrics(yearbook_clean, label = "after cleaning")
validate_yearbook_metrics(yearbook_clean)

dir.create("data/output/supporting", showWarnings = FALSE, recursive = TRUE)
write_csv_reproducible(yearbook_clean, "data/output/supporting/yearbook_full.csv")
