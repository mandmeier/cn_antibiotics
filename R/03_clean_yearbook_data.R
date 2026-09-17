# Harmonize yearbook metric names and units.

source("R/utils/yearbook_metrics.R")

yearbook <- read_csv("data/raw/yearbook_data/yearbook_data_combined.csv")

audit_yearbook_metrics(yearbook, label = "before cleaning")

yearbook_clean <- yearbook %>%
  mutate(metric = vapply(metric, harmonize_yearbook_metric, character(1))) %>%
  apply_yearbook_unit_parsing() %>%
  apply_yearbook_unit_parsing()

n_metrics_after <- n_distinct(yearbook_clean$metric)
cat("\nunique metrics after cleaning:", n_metrics_after, "\n")

audit_yearbook_metrics(yearbook_clean, label = "after cleaning")
validate_yearbook_metrics(yearbook_clean)

write_csv(yearbook_clean, "data/intermediate/yearbook_clean.csv")
