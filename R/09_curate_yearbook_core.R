# Curate a yearbook core (24 One Health covariates) for release reuse.
# Full 764-metric table is written as a deposit-facing appendix.
# Outputs: yearbook_core.csv, yearbook_core_manifest.csv, yearbook_full.csv

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
})

source("R/utils/reproducible_csv.R")

PROVINCES <- c(
  "Anhui", "Beijing", "Chongqing", "Fujian", "Gansu", "Guangdong", "Guangxi",
  "Guizhou", "Hainan", "Hebei", "Heilongjiang", "Henan", "Hubei", "Hunan",
  "Inner Mongolia", "Jiangsu", "Jiangxi", "Jilin", "Liaoning", "Ningxia",
  "Qinghai", "Shaanxi", "Shandong", "Shanghai", "Shanxi", "Sichuan",
  "Tianjin", "Tibet", "Xinjiang", "Yunnan", "Zhejiang"
)

EXPECTED_CORE_N <- 24L
EXPECTED_FULL_N <- 764L

PATH_CLEAN <- "data/intermediate/yearbook_clean.csv"
PATH_CORE <- "data/output/yearbook_core.csv"
PATH_MANIFEST <- "data/output/yearbook_core_manifest.csv"
PATH_FULL <- "data/output/yearbook_full.csv"

if (!file.exists(PATH_CLEAN)) {
  stop("Missing yearbook clean file (run R/03_clean_yearbook_data.R first): ", PATH_CLEAN)
}

CORE_METRICS <- tibble::tribble(
  ~metric, ~theme,
  "E03_09_Gross_Regional_Product", "gdp",
  "E03_09_Per_Capita_Gross_Regional_Product", "gdp",
  "E02_07_Total_Population_year_end", "urbanization",
  "E02_07_Urban_Population_Proportion", "urbanization",
  "E12_13_Hogs_year_end", "livestock",
  "E12_13_Cattle_and_Buffaloes", "livestock",
  "E12_13_Sheep_and_Goats_year_end", "livestock",
  "E12_13_Slaughtered_Hogs", "livestock",
  "E12_14_Output_of_Meat", "livestock",
  "E12_14_Poultry_Eggs", "livestock",
  "E12_03_Gross_Output_Value_Animal_Husbandry", "livestock",
  "E25_08_Daily_Disposal_Capacity_of_City_Sewage", "wastewater",
  "E25_13_Waste_Water_Treatment_Rate", "wastewater",
  "E25_13_Centralized_Treatment_at_Sewage_Treatment_Plants", "wastewater",
  "E25_05_Total_Annual_Volume_of_Tap_Water_Supply", "wastewater",
  "E22_01_Hospitals", "hospitals",
  "E22_07_Beds_Total", "hospitals",
  "E22_02_Licensed_Physicians", "hospitals",
  "E22_02_Health_Technical_Personnel", "hospitals",
  "E07_06_Expenditure_for_Health_Care", "hospitals",
  "E08_14_Common_Industrial_Solid_Wastes_Generated", "environment",
  "E08_14_Hazardous_Wastes_Generated", "environment",
  "E08_33_Investment_in_Urban_Environmental_Infrastructure", "environment",
  "E25_13_Rate_of_Domestic_Garbage_Harmless_Treatment", "environment"
)

if (nrow(CORE_METRICS) != EXPECTED_CORE_N) {
  stop("CORE_METRICS must have ", EXPECTED_CORE_N, " rows, found ", nrow(CORE_METRICS))
}
if (any(duplicated(CORE_METRICS$metric))) {
  stop("Duplicate metric ids in CORE_METRICS")
}

humanize_yearbook_definition <- function(metric) {
  m <- str_match(metric, "^(E\\d{2}_\\d{2})_(.+)$")
  if (any(is.na(m[1, ]))) {
    return(str_replace_all(metric, "_", " "))
  }
  table_code <- m[1, 2]
  label <- str_replace_all(m[1, 3], "_", " ")
  paste0(label, " (China Statistical Yearbook table ", table_code, ")")
}

yearbook <- read_csv(PATH_CLEAN, show_col_types = FALSE)

required_cols <- c("province", "metric", "value", "unit")
missing_cols <- setdiff(required_cols, names(yearbook))
if (length(missing_cols) > 0) {
  stop("yearbook_clean missing columns: ", paste(missing_cols, collapse = ", "))
}

n_full <- n_distinct(yearbook$metric)
if (n_full != EXPECTED_FULL_N) {
  stop("Expected ", EXPECTED_FULL_N, " unique yearbook metrics, found ", n_full)
}

got_provinces <- sort(unique(yearbook$province))
if (!identical(got_provinces, sort(PROVINCES))) {
  stop(
    "Province mismatch in yearbook_clean.\n  expected: ",
    paste(PROVINCES, collapse = ", "),
    "\n  got: ", paste(got_provinces, collapse = ", ")
  )
}

missing_core <- setdiff(CORE_METRICS$metric, yearbook$metric)
if (length(missing_core) > 0) {
  stop(
    "Core metrics not found in yearbook_clean:\n  ",
    paste(missing_core, collapse = "\n  ")
  )
}

core <- yearbook %>%
  filter(metric %in% CORE_METRICS$metric) %>%
  select(province, metric, value, unit) %>%
  arrange(metric, province)

n_core <- n_distinct(core$metric)
if (n_core != EXPECTED_CORE_N) {
  stop("Expected ", EXPECTED_CORE_N, " core metrics after filter, found ", n_core)
}

unit_check <- core %>%
  distinct(metric, unit) %>%
  count(metric, name = "n_units")
if (any(unit_check$n_units > 1L)) {
  stop("Some core metrics have multiple units")
}

prov_check <- core %>%
  group_by(metric) %>%
  summarise(n_prov = n_distinct(province), .groups = "drop")
if (any(prov_check$n_prov != length(PROVINCES))) {
  bad <- prov_check$metric[prov_check$n_prov != length(PROVINCES)]
  stop("Core metrics missing some provinces: ", paste(bad, collapse = ", "))
}

manifest <- CORE_METRICS %>%
  left_join(
    core %>% distinct(metric, unit),
    by = "metric"
  ) %>%
  mutate(definition = vapply(metric, humanize_yearbook_definition, character(1))) %>%
  select(metric, theme, definition, unit) %>%
  arrange(theme, metric)

dir.create("data/output", showWarnings = FALSE, recursive = TRUE)

write_csv_reproducible(core, PATH_CORE)
write_csv_reproducible(manifest, PATH_MANIFEST)

# Deposit-facing appendix: same content as intermediate yearbook_clean.
full_out <- yearbook %>%
  select(province, metric, value, unit) %>%
  arrange(metric, province)
write_csv_reproducible(full_out, PATH_FULL)

theme_counts <- manifest %>%
  count(theme, name = "n") %>%
  arrange(theme)

message(
  "Wrote ", PATH_CORE, " (", EXPECTED_CORE_N, " metrics; ",
  nrow(core), " rows), ", PATH_MANIFEST, ", and appendix ", PATH_FULL,
  " (", EXPECTED_FULL_N, " metrics)."
)
message(
  "Core themes: ",
  paste(paste0(theme_counts$theme, "=", theme_counts$n), collapse = ", ")
)
