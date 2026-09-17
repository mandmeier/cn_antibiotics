# Verify antibiotic_class assignment in environmental_cleaned.csv.

source("R/utils/antibiotic_classes.R")

env_path <- "data/intermediate/environmental_cleaned.csv"
environmental <- readr::read_csv(env_path, show_col_types = FALSE)

abx_groups <- environmental %>%
  dplyr::distinct(antibiotic, antibiotic_class)

multi_group <- abx_groups %>%
  dplyr::group_by(antibiotic) %>%
  dplyr::filter(dplyr::n() > 1L)

if (nrow(multi_group) > 0) {
  stop(
    "Antibiotic maps to multiple groups:\n",
    paste(capture.output(print(multi_group)), collapse = "\n")
  )
}

bad_levels <- setdiff(
  unique(environmental$antibiotic_class),
  ANTIBIOTIC_GROUP_LEVELS
)
if (length(bad_levels) > 0) {
  stop(
    "Invalid antibiotic_class values: ",
    paste(sort(bad_levels), collapse = ", ")
  )
}

if (any(grepl("multiple classes", environmental$antibiotic_class, ignore.case = TRUE))) {
  stop("Found Multiple classes group labels.")
}

agg_rows <- environmental %>%
  dplyr::filter(vapply(antibiotic, is_aggregate_antibiotic, logical(1)))
if (nrow(agg_rows) > 0) {
  stop(
    "Aggregate antibiotics remain in cleaned data: ",
    paste(sort(unique(agg_rows$antibiotic)), collapse = ", ")
  )
}

message(
  "Antibiotic group verification passed (",
  nrow(abx_groups), " antibiotics, ",
  length(unique(environmental$antibiotic_class)), " groups)."
)
