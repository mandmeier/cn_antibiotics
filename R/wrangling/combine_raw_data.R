# Append raw environmental data sources into one table.
# Output: data/raw/environmental_data/environmental_data_combined.csv




zhang <- read_excel("data/raw/environmental_data/Zhang_2022.xls", sheet = "Records")

zhang_formatted <- zhang %>%
  # add location variable
  mutate(loc = paste(loc_l1, loc_l2, loc_l3, loc_l4, loc_Ref)) %>%
  group_by(sem_type, loc, ABX_subcat, sam_Y, sam_M, ABX_conc_max, ABX_conc_mean, pub_id) %>%
  # remove duplicate measurements
  select(sample_type = sem_type, province = loc_l1, location = loc, sample_year = sam_Y, season = sam_M, antibiotic = ABX_subcat, mean_concentration = ABX_conc_mean, max_concentration = ABX_conc_max, reference_number = pub_id) %>%
  unique() %>%
  ungroup() %>%
  # add units
  mutate(concentration_unit = "ng/g", .after = max_concentration) %>%
  # remove non-numeric measurements
  mutate(mean_concentration = as.numeric(mean_concentration)) %>%
  mutate(max_concentration = as.numeric(max_concentration)) %>%
  # remove values that have NA for both mean and max measurements
  filter(!is.na(mean_concentration) | !is.na(max_concentration))



S1 <- read_excel("data/raw/environmental_data/05052026 S1.xlsx", sheet = "S1 data")








create ONE table china_antibiotics_measurements








ENV_COLS <- c(
  "sample_type",
  "province",
  "location",
  "antibiotic",
  "group_of_antibiotic",
  "mean_concentration",
  "max_concentration",
  "concentration_unit",
  "sample_year",
  "season",
  "full_reference_name",
  "place_in_article_data_extracted_from",
  "reference_number",
  "source_dataset",
  "article_link"
)

coerce_env_types <- function(df) {
  df |>
    mutate(
      mean_concentration = as.numeric(mean_concentration),
      max_concentration = as.numeric(max_concentration)
    )
}

read_env_excel <- function(path, sheet) {
  read_excel(path, sheet = sheet) |>
    select(all_of(ENV_COLS)) |>
    coerce_env_types() |>
    mutate(source_file = basename(path))
}

read_env_csv <- function(path) {
  read_csv(path, show_col_types = FALSE) |>
    select(all_of(ENV_COLS)) |>
    coerce_env_types() |>
    mutate(source_file = basename(path))
}

environmental_raw <- bind_rows(
  read_env_excel(
    "data/raw/environmental_data/05052026 Zhang_2022.xlsx",
    sheet = "Zhang 2022 mainland data"
  ),
  read_env_excel(
    "data/raw/environmental_data/05052026 S1.xlsx",
    sheet = "S1 data"
  ),
  read_env_excel(
    "data/raw/environmental_data/05052026 S2.xlsx",
    sheet = "Additions data N91+"
  ),
  read_env_excel(
    "data/raw/environmental_data/05052026 S3.xlsx",
    sheet = "S3 data"
  ),
  read_env_csv(
    "data/raw/environmental_data/27052026_gaps_addition_to_environmental_cleaned_primary_merge_ready.csv"
  )
)


sources <- environmental_raw %>%
  select(source_file, reference_number, source_dataset) %>%
  unique() %>%
  arrange(source_file, reference_number)


unique_measurements <- environmental_raw %>%
  group_by(reference_number, place_in_article_data_extracted_from, sample_type, province, antibiotic, sample_year, season, mean_concentration) %>%
  add_tally() %>%
  filter(n > 1)





write_csv(
  environmental_raw,
  "data/raw/environmental_data/environmental_data_combined.csv"
)

message(
  "Wrote ", nrow(environmental_raw), " rows to ",
  "data/raw/environmental_data/environmental_data_combined.csv"
)
