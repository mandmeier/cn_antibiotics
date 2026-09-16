# Flag suspicious unit conversions on combined environmental data for publication review.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})
source("R/utils/environmental_conversion_flags.R")

environmental <- readr::read_csv(
  "data/raw/environmental_data/environmental_data_combined.csv",
  show_col_types = FALSE
)

result <- flag_environmental_concentrations(environmental, from_cleaned = FALSE)

output_cols <- c(
  "reference_number",
  "source_dataset",
  "full_reference_name",
  "article_link",
  "place_in_article_data_extracted_from",
  "province",
  "location",
  "sample_type",
  "matrix",
  "sample_year",
  "season",
  "antibiotic",
  "group_of_antibiotic",
  "antibiotic_class",
  "unit_raw",
  "mean_raw",
  "max_raw",
  "conversion_factor",
  "unit_converted",
  "mean_converted",
  "max_converted",
  "flag_codes",
  "flag_reasons",
  "suggested_unit_note"
)

suspicious <- result$suspicious %>%
  dplyr::select(dplyr::any_of(output_cols))

readr::write_csv(
  suspicious,
  "data/cleaned/environmental_suspicious_conversions.csv"
)

print_flag_audit_summary(result$audited, suspicious)
