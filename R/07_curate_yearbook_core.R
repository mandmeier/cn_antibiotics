# Curate a yearbook province core (24 One Health covariates) for release reuse.
# Full 764-metric table is produced by R/03 as supporting/yearbook_full.csv.
# Outputs: yearbook_province.csv (with year), yearbook_province_manifest.csv
#
# Year coverage:
# - Population and urban share: all years available in yearbook_full via
#   E02_05 / E02_06 year-suffixed series (mapped onto the core metric ids).
# - Remaining core metrics: single yearbook vintage; year inferred by matching
#   E02_07_Total_Population_year_end to E02_05_Population_at_year_end_YYYY.

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

PATH_FULL <- "data/output/supporting/yearbook_full.csv"
PATH_CORE <- "data/output/primary/yearbook_province.csv"
PATH_MANIFEST <- "data/output/supporting/yearbook_province_manifest.csv"

if (!file.exists(PATH_FULL)) {
  stop("Missing yearbook full file (run R/03_clean_yearbook_data.R first): ", PATH_FULL)
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

# Core metric id → year-suffixed series prefix in yearbook_full.
MULTI_YEAR_SERIES <- tibble::tribble(
  ~metric, ~series_prefix,
  "E02_07_Total_Population_year_end", "E02_05_Population_at_year_end_",
  "E02_07_Urban_Population_Proportion", "E02_06_Urban_Population_Proportion_"
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

yearbook <- read_csv(PATH_FULL, show_col_types = FALSE)

required_cols <- c("province", "metric", "value", "unit")
missing_cols <- setdiff(required_cols, names(yearbook))
if (length(missing_cols) > 0) {
  stop("yearbook_full missing columns: ", paste(missing_cols, collapse = ", "))
}

n_full <- n_distinct(yearbook$metric)
if (n_full != EXPECTED_FULL_N) {
  stop("Expected ", EXPECTED_FULL_N, " unique yearbook metrics, found ", n_full)
}

got_provinces <- sort(unique(yearbook$province))
if (!identical(got_provinces, sort(PROVINCES))) {
  stop(
    "Province mismatch in yearbook_full.\n  expected: ",
    paste(PROVINCES, collapse = ", "),
    "\n  got: ", paste(got_provinces, collapse = ", ")
  )
}

missing_core <- setdiff(CORE_METRICS$metric, yearbook$metric)
if (length(missing_core) > 0) {
  stop(
    "Core metrics not found in yearbook_full:\n  ",
    paste(missing_core, collapse = "\n  ")
  )
}

# --- Infer single-vintage reference year from population match ----------------

pop_core <- yearbook %>%
  filter(metric == "E02_07_Total_Population_year_end") %>%
  select(province, value_core = value)

pop_years <- yearbook %>%
  filter(str_detect(metric, "^E02_05_Population_at_year_end_\\d{4}$")) %>%
  mutate(year = as.integer(str_extract(metric, "\\d{4}$"))) %>%
  select(province, year, value)

year_matches <- pop_years %>%
  inner_join(pop_core, by = "province") %>%
  group_by(year) %>%
  summarise(
    n_match = sum(as.character(value) == as.character(value_core), na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(desc(n_match), desc(year))

if (nrow(year_matches) == 0L || max(year_matches$n_match) < length(PROVINCES)) {
  stop(
    "Could not infer yearbook vintage: no E02_05 year matches all provinces ",
    "against E02_07_Total_Population_year_end."
  )
}

reference_year <- year_matches$year[[1]]
message("Inferred single-vintage reference year: ", reference_year)

# --- Multi-year core metrics from year-suffixed series ------------------------

expand_multi_year <- function(metric_id, series_prefix) {
  pattern <- paste0("^", series_prefix, "\\d{4}$")
  series <- yearbook %>%
    filter(str_detect(metric, pattern)) %>%
    mutate(
      year = as.integer(str_extract(metric, "\\d{4}$")),
      metric = metric_id
    ) %>%
    select(year, province, metric, value, unit)

  if (nrow(series) == 0L) {
    stop("No year-suffixed series found for prefix: ", series_prefix)
  }

  years <- sort(unique(series$year))
  message(
    "  ", metric_id, ": years ", paste(years, collapse = ", "),
    " (", nrow(series), " rows)"
  )
  series
}

message("Expanding multi-year core series:")
multi_year_core <- bind_rows(lapply(
  seq_len(nrow(MULTI_YEAR_SERIES)),
  function(i) {
    expand_multi_year(
      MULTI_YEAR_SERIES$metric[[i]],
      MULTI_YEAR_SERIES$series_prefix[[i]]
    )
  }
))

# Confirm reference-year multi-year values match the single-vintage core metric.
for (i in seq_len(nrow(MULTI_YEAR_SERIES))) {
  metric_id <- MULTI_YEAR_SERIES$metric[[i]]
  vintage <- yearbook %>%
    filter(metric == metric_id) %>%
    select(province, value_core = value, unit_core = unit)
  expanded_ref <- multi_year_core %>%
    filter(metric == metric_id, year == reference_year) %>%
    select(province, value, unit)
  cmp <- vintage %>%
    inner_join(expanded_ref, by = "province")
  n_match <- sum(as.character(cmp$value) == as.character(cmp$value_core), na.rm = TRUE)
  if (n_match != nrow(cmp)) {
    stop(
      "Reference-year mismatch for ", metric_id, ": only ", n_match, "/",
      nrow(cmp), " provinces match between core metric and year-suffixed series."
    )
  }
  if (any(cmp$unit != cmp$unit_core)) {
    stop("Unit mismatch for ", metric_id, " vs year-suffixed series")
  }
}

# --- Single-vintage core metrics (all except multi-year series) ---------------

single_metrics <- setdiff(CORE_METRICS$metric, MULTI_YEAR_SERIES$metric)

single_year_core <- yearbook %>%
  filter(metric %in% single_metrics) %>%
  mutate(year = as.integer(reference_year)) %>%
  select(year, province, metric, value, unit)

# --- Combine ------------------------------------------------------------------

core <- bind_rows(multi_year_core, single_year_core) %>%
  arrange(metric, year, province)

n_core <- n_distinct(core$metric)
if (n_core != EXPECTED_CORE_N) {
  stop("Expected ", EXPECTED_CORE_N, " core metrics after build, found ", n_core)
}

unit_check <- core %>%
  distinct(metric, unit) %>%
  count(metric, name = "n_units")
if (any(unit_check$n_units > 1L)) {
  stop("Some core metrics have multiple units")
}

prov_check <- core %>%
  group_by(metric, year) %>%
  summarise(n_prov = n_distinct(province), .groups = "drop")
if (any(prov_check$n_prov != length(PROVINCES))) {
  bad <- prov_check %>% filter(n_prov != length(PROVINCES))
  stop(
    "Some metric-year cells missing provinces:\n",
    paste0("  ", bad$metric, " / ", bad$year, " (", bad$n_prov, ")", collapse = "\n")
  )
}

for (metric_id in MULTI_YEAR_SERIES$metric) {
  n_years <- n_distinct(core$year[core$metric == metric_id])
  if (n_years < 2L) {
    stop("Expected multiple years for ", metric_id, ", found ", n_years)
  }
}

single_year_check <- core %>%
  filter(metric %in% single_metrics) %>%
  distinct(metric, year)
if (any(single_year_check$year != reference_year)) {
  stop("Single-vintage metrics must use reference year ", reference_year)
}

manifest <- CORE_METRICS %>%
  left_join(
    core %>% distinct(metric, unit),
    by = "metric"
  ) %>%
  mutate(definition = vapply(metric, humanize_yearbook_definition, character(1))) %>%
  select(metric, theme, definition, unit) %>%
  arrange(theme, metric)

dir.create("data/output/primary", showWarnings = FALSE, recursive = TRUE)
dir.create("data/output/supporting", showWarnings = FALSE, recursive = TRUE)

write_csv_reproducible(core, PATH_CORE)
write_csv_reproducible(manifest, PATH_MANIFEST)

theme_counts <- manifest %>%
  count(theme, name = "n") %>%
  arrange(theme)

message(
  "Wrote ", PATH_CORE, " (", EXPECTED_CORE_N, " metrics; ",
  nrow(core), " rows; years ", min(core$year), "–", max(core$year),
  ") and ", PATH_MANIFEST,
  " (from ", PATH_FULL, ", ", EXPECTED_FULL_N, " metrics)."
)
message(
  "Core themes: ",
  paste(paste0(theme_counts$theme, "=", theme_counts$n), collapse = ", ")
)
