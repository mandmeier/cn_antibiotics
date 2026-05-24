# Import and combine yearbook tables from raw Excel files into one long dataset.

source("R/utils/yearbook_metrics.R")

data_dir <- "data/raw/yearbook_data"

non_province_rows <- c(
  "National Total",
  "Central and State Organs",
  "Not Classified by Region"
)

files <- sort(list.files(
  data_dir,
  pattern = "[.](xlsx|xls)$",
  full.names = TRUE,
  ignore.case = TRUE
))

tables <- lapply(files, function(f) {
  df <- read_excel(f, sheet = 1, col_types = "text")
  names(df)[1] <- "province"
  df %>%
    filter(!province %in% non_province_rows)
})

combined_wide <- Reduce(
  function(x, y) full_join(x, y, by = "province"),
  tables
)

yearbook_data_combined <- combined_wide %>%
  pivot_longer(cols = -province, names_to = "metric", values_to = "value") %>%
  apply_yearbook_unit_parsing()

write_csv(yearbook_data_combined, file.path(data_dir, "yearbook_data_combined.csv"))
