# Audit antibiotics in cleaned environmental data against the group lookup table.

source("R/utils/antibiotic_classes.R")

env_path <- "data/output/supporting/env_records.csv"
lookup_path <- "data/raw/reference/antibiotic_group_lookup.csv"

environmental <- readr::read_csv(env_path, show_col_types = FALSE)
lookup <- readr::read_csv(lookup_path, show_col_types = FALSE)

abx <- sort(unique(environmental$antibiotic))
missing <- setdiff(abx, lookup$antibiotic)
extra <- setdiff(lookup$antibiotic, abx)

cat("Distinct antibiotics in cleaned data:", length(abx), "\n")
cat("Lookup rows:", nrow(lookup), "\n\n")

if (length(missing) > 0) {
  cat("Missing from lookup:\n")
  print(missing)
  cat("\nSuggested groups (infer_antibiotic_group):\n")
  print(
    tibble::tibble(
      antibiotic = missing,
      group_of_antibiotic = infer_antibiotic_group(missing)
    )
  )
}

if (length(extra) > 0) {
  cat("In lookup but not in cleaned data:\n")
  print(extra)
}

conflicts <- environmental %>%
  dplyr::distinct(antibiotic, group_of_antibiotic) %>%
  dplyr::left_join(lookup, by = "antibiotic", suffix = c("_assigned", "_lookup")) %>%
  dplyr::filter(group_of_antibiotic_assigned != group_of_antibiotic_lookup)

if (nrow(conflicts) > 0) {
  cat("\nAssigned vs lookup mismatches:\n")
  print(conflicts)
}
