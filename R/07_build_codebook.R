# Build a codebook for curated analysis tables.
# Yearbook long-format: one row per metric (764). Other tables: one row per column.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})

source("R/utils/reproducible_csv.R")

PROVINCES <- c(
  "Anhui", "Beijing", "Chongqing", "Fujian", "Gansu", "Guangdong", "Guangxi",
  "Guizhou", "Hainan", "Hebei", "Heilongjiang", "Henan", "Hubei", "Hunan",
  "Inner Mongolia", "Jiangsu", "Jiangxi", "Jilin", "Liaoning", "Ningxia",
  "Qinghai", "Shaanxi", "Shandong", "Shanghai", "Shanxi", "Sichuan",
  "Tianjin", "Tibet", "Xinjiang", "Yunnan", "Zhejiang"
)
PROVINCE_ALLOWED <- paste(PROVINCES, collapse = "; ")

ANTIBIOTIC_CLASSES <- c(
  "Beta-lactams", "Diaminopyrimidines", "Fluoroquinolones", "Lincosamides",
  "Macrolides", "Other", "Phenicols", "Quinolones", "Sulfonamides",
  "Tetracyclines"
)
CLASS_ALLOWED <- paste(ANTIBIOTIC_CLASSES, collapse = "; ")

SAMPLE_TYPES <- c(
  "MSW incineration leachate", "agricultural soil", "aquaculture pond water",
  "aquaculture wastewater", "coastal water", "groundwater", "hospital wastewater",
  "lake sediment", "livestock wastewater", "municipal sludge",
  "municipal wastewater (urban sewer)", "municipal wastewater effluent",
  "pharmaceutical wastewater", "river water (suspended matter fraction)",
  "sediment", "soil", "surface water", "surface water sediment",
  "wastewater influent"
)
SAMPLE_TYPE_ALLOWED <- paste(SAMPLE_TYPES, collapse = "; ")

MATRIX_ALLOWED <- paste(
  c(
    "sediment", "sludge", "soil", "surface water",
    "wastewater effluent", "wastewater influent"
  ),
  collapse = "; "
)

CONC_UNIT_ALLOWED <- "ng/L; ng/g; ng/g dw"

CURATED_PATHS <- c(
  yearbook_clean = "data/intermediate/yearbook_clean.csv",
  resistance_clean = "data/intermediate/resistance_clean.csv",
  environmental_cleaned = "data/intermediate/environmental_cleaned.csv",
  environmental_by_site = "data/intermediate/environmental_by_site.csv",
  env_abx_per_province = "data/output/env_abx_per_province.csv",
  env_abx_per_site = "data/output/env_abx_per_site.csv",
  antibiotic_metrics_china = "data/output/antibiotic_metrics_china.csv",
  province_groups = "data/output/province_groups.csv"
)

codebook_row <- function(table, name, definition, type, allowed_values, units,
                         missing_value_rules) {
  tibble(
    table = table,
    name = name,
    definition = definition,
    type = type,
    allowed_values = allowed_values,
    units = units,
    missing_value_rules = missing_value_rules
  )
}

# --- Yearbook: humanize metric names and map unit → type --------------------

humanize_yearbook_definition <- function(metric) {
  m <- str_match(metric, "^(E\\d{2}_\\d{2})_(.+)$")
  if (any(is.na(m[1, ]))) {
    return(str_replace_all(metric, "_", " "))
  }
  table_code <- m[1, 2]
  label <- str_replace_all(m[1, 3], "_", " ")
  paste0(label, " (China Statistical Yearbook table ", table_code, ")")
}

yearbook_type_from_unit <- function(unit) {
  case_when(
    unit %in% c("counts", "number", "person", "persons", "heads", "beds", "bed",
                "couples", "pieces", "piece", "units", "point") ~ "integer",
    unit %in% c("percent", "pct", "percent_mixed") ~ "percent",
    unit == "per_mille" ~ "rate",
    unit == "preceding_year_100" ~ "index",
    TRUE ~ "numeric"
  )
}

yearbook_allowed_from_unit <- function(unit) {
  case_when(
    unit %in% c("percent", "pct", "percent_mixed") ~
      "non-negative numeric; typically 0-100",
    unit == "per_mille" ~
      "non-negative numeric; typically 0-1000",
    unit == "preceding_year_100" ~
      "non-negative numeric; prior year = 100",
    unit %in% c("counts", "number", "person", "persons", "heads", "beds", "bed",
                "couples", "pieces", "piece", "units", "point") ~
      "non-negative integer",
    TRUE ~ "non-negative numeric; see unit"
  )
}

YEARBOOK_MISSING <- paste(
  "NA/blank in the long-format value cell means the metric was not reported",
  "for that province in the source extract (~1443 missing cells across 198",
  "metrics; at most 31 provinces missing for a given metric)."
)

build_yearbook_codebook <- function(yearbook_path) {
  yearbook <- read_csv(yearbook_path, show_col_types = FALSE)

  metrics <- yearbook %>%
    distinct(metric, unit) %>%
    arrange(metric)

  if (nrow(metrics) != 764L) {
    stop("Expected 764 unique yearbook metrics, found ", nrow(metrics))
  }
  if (any(duplicated(metrics$metric))) {
    stop("Some yearbook metrics have multiple units")
  }

  metrics %>%
    transmute(
      table = "yearbook_clean",
      name = metric,
      definition = vapply(metric, humanize_yearbook_definition, character(1)),
      type = yearbook_type_from_unit(unit),
      allowed_values = yearbook_allowed_from_unit(unit),
      units = unit,
      missing_value_rules = YEARBOOK_MISSING
    )
}

# --- Static schema rows for non-yearbook curated tables ----------------------

NA_BLANK <- "NA/blank means not available or not reported in the curated table."
NA_NONE <- "Not missing in curated output; every row has a value."

schema_resistance_clean <- bind_rows(
  codebook_row(
    "resistance_clean", "year",
    "CARSS surveillance year for the resistance panel.",
    "integer", "2019; 2020; 2021; 2022; 2023; 2024", "n/a", NA_NONE
  ),
  codebook_row(
    "resistance_clean", "province",
    "Provincial-level administrative unit (English name; Tibet for Xizang).",
    "categorical", PROVINCE_ALLOWED, "n/a", NA_NONE
  ),
  codebook_row(
    "resistance_clean", "bacteria_species",
    "Pathogen species in the CARSS panel.",
    "categorical",
    "A. baumannii; E. coli; K. pneumoniae; P. aeruginosa; S. aureus",
    "n/a", NA_NONE
  ),
  codebook_row(
    "resistance_clean", "antibiotic",
    "Antibiotic tested; combo drugs dropped except Trimethoprim/Sulfamethoxazole.",
    "string", "drug names as cleaned from CARSS", "n/a", NA_NONE
  ),
  codebook_row(
    "resistance_clean", "antibiotic_class",
    "Pharmacological class assigned from the project antibiotic group lookup.",
    "categorical", CLASS_ALLOWED, "n/a",
    "NA if the antibiotic is unmapped in the lookup."
  ),
  codebook_row(
    "resistance_clean", "total_n_strains",
    "Number of isolates tested for this province-year-bug-drug cell.",
    "integer", "non-negative integer", "strains", NA_BLANK
  ),
  codebook_row(
    "resistance_clean", "resistant_percent",
    "Percent of isolates classified resistant.",
    "percent", "0-100", "%", NA_BLANK
  ),
  codebook_row(
    "resistance_clean", "resistant_n_strains",
    "Count of isolates classified resistant.",
    "integer", "non-negative integer", "strains", NA_BLANK
  ),
  codebook_row(
    "resistance_clean", "intermediate_percent",
    "Percent of isolates classified intermediate.",
    "percent", "0-100", "%", NA_BLANK
  ),
  codebook_row(
    "resistance_clean", "intermediate_n_strains",
    "Count of isolates classified intermediate.",
    "integer", "non-negative integer", "strains", NA_BLANK
  ),
  codebook_row(
    "resistance_clean", "sensitive_percent",
    "Percent of isolates classified sensitive/susceptible.",
    "percent", "0-100", "%", NA_BLANK
  ),
  codebook_row(
    "resistance_clean", "sensitive_n_strains",
    "Count of isolates classified sensitive/susceptible.",
    "integer", "non-negative integer", "strains", NA_BLANK
  )
)

schema_environmental_cleaned <- bind_rows(
  codebook_row(
    "environmental_cleaned", "sample_type",
    "Literature sample category (finer than matrix).",
    "categorical", SAMPLE_TYPE_ALLOWED, "n/a", NA_NONE
  ),
  codebook_row(
    "environmental_cleaned", "matrix",
    "Harmonized environmental matrix for cross-study comparison.",
    "categorical", MATRIX_ALLOWED, "n/a", NA_NONE
  ),
  codebook_row(
    "environmental_cleaned", "province",
    "Province assigned to the measurement (English name).",
    "categorical", PROVINCE_ALLOWED, "n/a", NA_NONE
  ),
  codebook_row(
    "environmental_cleaned", "sample_year",
    "Year of environmental sampling when reported.",
    "integer", "calendar year", "n/a", NA_BLANK
  ),
  codebook_row(
    "environmental_cleaned", "antibiotic",
    "Antibiotic analyte name after cleaning.",
    "string", "antibiotic names as cleaned from literature sources", "n/a",
    NA_NONE
  ),
  codebook_row(
    "environmental_cleaned", "antibiotic_class",
    "Pharmacological class assigned from the project antibiotic group lookup.",
    "categorical", CLASS_ALLOWED, "n/a",
    "NA if the antibiotic is unmapped in the lookup."
  ),
  codebook_row(
    "environmental_cleaned", "mean_concentration",
    "Reported or derived mean environmental concentration.",
    "numeric", "non-negative numeric", "see concentration_unit", NA_BLANK
  ),
  codebook_row(
    "environmental_cleaned", "max_concentration",
    "Reported maximum environmental concentration when available.",
    "numeric", "non-negative numeric", "see concentration_unit", NA_BLANK
  ),
  codebook_row(
    "environmental_cleaned", "concentration_unit",
    "Unit of mean_concentration and max_concentration after conversion.",
    "categorical", CONC_UNIT_ALLOWED, "n/a", NA_NONE
  ),
  codebook_row(
    "environmental_cleaned", "reference_number",
    "Publication ID linking to Data_Sources / literature extract.",
    "string", "reference IDs as in source extracts (numeric or N-prefixed)",
    "n/a", NA_BLANK
  )
)

schema_environmental_by_site <- bind_rows(
  codebook_row(
    "environmental_by_site", "sample_type",
    "Literature sample category (finer than matrix).",
    "categorical", SAMPLE_TYPE_ALLOWED, "n/a", NA_NONE
  ),
  codebook_row(
    "environmental_by_site", "matrix",
    "Harmonized environmental matrix for cross-study comparison.",
    "categorical", MATRIX_ALLOWED, "n/a", NA_NONE
  ),
  codebook_row(
    "environmental_by_site", "province",
    "Province assigned to the measurement (English name).",
    "categorical", PROVINCE_ALLOWED, "n/a", NA_NONE
  ),
  codebook_row(
    "environmental_by_site", "location",
    "Heterogeneous literature site label (not a formal site ID).",
    "string", "free-text location strings from source studies", "n/a",
    NA_BLANK
  ),
  codebook_row(
    "environmental_by_site", "season",
    "Sampling season or month: Zhang month 1-12 or sparse supplemental text.",
    "string", "1-12 or free-text season labels", "n/a", NA_BLANK
  ),
  codebook_row(
    "environmental_by_site", "lon",
    "Site longitude when geocoded or reported.",
    "numeric", "decimal degrees", "degrees", NA_BLANK
  ),
  codebook_row(
    "environmental_by_site", "lat",
    "Site latitude when geocoded or reported.",
    "numeric", "decimal degrees", "degrees", NA_BLANK
  ),
  codebook_row(
    "environmental_by_site", "sample_year",
    "Year of environmental sampling when reported.",
    "integer", "calendar year", "n/a", NA_BLANK
  ),
  codebook_row(
    "environmental_by_site", "antibiotic",
    "Antibiotic analyte name after cleaning.",
    "string", "antibiotic names as cleaned from literature sources", "n/a",
    NA_NONE
  ),
  codebook_row(
    "environmental_by_site", "antibiotic_class",
    "Pharmacological class assigned from the project antibiotic group lookup.",
    "categorical", CLASS_ALLOWED, "n/a",
    "NA if the antibiotic is unmapped in the lookup."
  ),
  codebook_row(
    "environmental_by_site", "mean_concentration",
    "Reported or derived mean environmental concentration.",
    "numeric", "non-negative numeric", "see concentration_unit", NA_BLANK
  ),
  codebook_row(
    "environmental_by_site", "max_concentration",
    "Reported maximum environmental concentration when available.",
    "numeric", "non-negative numeric", "see concentration_unit", NA_BLANK
  ),
  codebook_row(
    "environmental_by_site", "concentration_unit",
    "Unit of mean_concentration and max_concentration after conversion.",
    "categorical", CONC_UNIT_ALLOWED, "n/a", NA_NONE
  ),
  codebook_row(
    "environmental_by_site", "reference_number",
    "Publication ID linking to Data_Sources / literature extract.",
    "string", "reference IDs as in source extracts (numeric or N-prefixed)",
    "n/a", NA_BLANK
  )
)

schema_env_abx_per_province <- bind_rows(
  codebook_row(
    "env_abx_per_province", "sample_type",
    "Literature sample category aggregated to province median.",
    "categorical", SAMPLE_TYPE_ALLOWED, "n/a", NA_NONE
  ),
  codebook_row(
    "env_abx_per_province", "province",
    "Province for the median concentration.",
    "categorical", PROVINCE_ALLOWED, "n/a", NA_NONE
  ),
  codebook_row(
    "env_abx_per_province", "antibiotic",
    "Antibiotic analyte for the province median.",
    "string", "antibiotic names as cleaned from literature sources", "n/a",
    NA_NONE
  ),
  codebook_row(
    "env_abx_per_province", "median_concentration",
    "Median environmental concentration across sites/records in the cell.",
    "numeric", "non-negative numeric", "see concentration_unit", NA_BLANK
  ),
  codebook_row(
    "env_abx_per_province", "concentration_unit",
    "Unit of median_concentration.",
    "categorical", CONC_UNIT_ALLOWED, "n/a", NA_NONE
  ),
  codebook_row(
    "env_abx_per_province", "sample_year",
    "Representative or retained sample year for the aggregated cell.",
    "integer", "calendar year", "n/a", NA_BLANK
  ),
  codebook_row(
    "env_abx_per_province", "reference_number",
    "Publication ID retained with the aggregated cell.",
    "string", "reference IDs as in source extracts (numeric or N-prefixed)",
    "n/a", NA_BLANK
  )
)

schema_env_abx_per_site <- bind_rows(
  codebook_row(
    "env_abx_per_site", "sample_type",
    "Literature sample category for the site median.",
    "categorical", SAMPLE_TYPE_ALLOWED, "n/a", NA_NONE
  ),
  codebook_row(
    "env_abx_per_site", "province",
    "Province of the sampling site.",
    "categorical", PROVINCE_ALLOWED, "n/a", NA_NONE
  ),
  codebook_row(
    "env_abx_per_site", "location",
    "Heterogeneous literature site label (not a formal site ID).",
    "string", "free-text location strings from source studies", "n/a",
    NA_BLANK
  ),
  codebook_row(
    "env_abx_per_site", "season",
    "Sampling season or month: Zhang month 1-12 or sparse supplemental text.",
    "string", "1-12 or free-text season labels", "n/a", NA_BLANK
  ),
  codebook_row(
    "env_abx_per_site", "lon",
    "Site longitude when geocoded or reported.",
    "numeric", "decimal degrees", "degrees", NA_BLANK
  ),
  codebook_row(
    "env_abx_per_site", "lat",
    "Site latitude when geocoded or reported.",
    "numeric", "decimal degrees", "degrees", NA_BLANK
  ),
  codebook_row(
    "env_abx_per_site", "antibiotic",
    "Antibiotic analyte for the site median.",
    "string", "antibiotic names as cleaned from literature sources", "n/a",
    NA_NONE
  ),
  codebook_row(
    "env_abx_per_site", "median_concentration",
    "Median environmental concentration for the site cell.",
    "numeric", "non-negative numeric", "see concentration_unit", NA_BLANK
  ),
  codebook_row(
    "env_abx_per_site", "concentration_unit",
    "Unit of median_concentration.",
    "categorical", CONC_UNIT_ALLOWED, "n/a", NA_NONE
  ),
  codebook_row(
    "env_abx_per_site", "sample_year",
    "Sample year retained with the site cell.",
    "integer", "calendar year", "n/a", NA_BLANK
  ),
  codebook_row(
    "env_abx_per_site", "reference_number",
    "Publication ID retained with the site cell.",
    "string", "reference IDs as in source extracts (numeric or N-prefixed)",
    "n/a", NA_BLANK
  )
)

schema_antibiotic_metrics_china <- bind_rows(
  codebook_row(
    "antibiotic_metrics_china", "province",
    "Province for the combined env/resistance metric row.",
    "categorical", PROVINCE_ALLOWED, "n/a", NA_NONE
  ),
  codebook_row(
    "antibiotic_metrics_china", "antibiotic",
    "Antibiotic shared across the metric definition.",
    "string", "antibiotic names present in resistance and/or env aggregates",
    "n/a", NA_NONE
  ),
  codebook_row(
    "antibiotic_metrics_china", "metric",
    paste(
      "Metric key: either '{species}_resistance_{year}' or",
      "'{sample_type}_concentration'."
    ),
    "string",
    "resistance keys like E. coli_resistance_2019; env keys like soil_concentration",
    "n/a", NA_NONE
  ),
  codebook_row(
    "antibiotic_metrics_china", "value",
    "Observed province value (resistant percent or median concentration); rows with value <= 0 dropped.",
    "numeric", "positive numeric", "see unit", NA_NONE
  ),
  codebook_row(
    "antibiotic_metrics_china", "unit",
    "Unit of value / china_mean.",
    "categorical", "% resistant strains; ng/L; ng/g; ng/g dw", "n/a", NA_NONE
  ),
  codebook_row(
    "antibiotic_metrics_china", "china_mean",
    "Mean of value across provinces for the same metric x antibiotic (cells with >=10 provinces).",
    "numeric", "positive numeric", "see unit", NA_NONE
  ),
  codebook_row(
    "antibiotic_metrics_china", "fold_change",
    "value / china_mean for the metric x antibiotic group.",
    "numeric", "positive numeric", "fold", NA_NONE
  ),
  codebook_row(
    "antibiotic_metrics_china", "log2_fc",
    "log2(fold_change).",
    "numeric", "any numeric", "log2 fold", NA_NONE
  ),
  codebook_row(
    "antibiotic_metrics_china", "qu_category",
    "NBS development category from province metadata.",
    "categorical", "First; Second; Third; Fourth", "n/a", NA_BLANK
  ),
  codebook_row(
    "antibiotic_metrics_china", "qu_subcategory",
    "NBS development subcategory code from province metadata.",
    "categorical", "1; 2; 3; 4; 5; 6; 7", "n/a", NA_BLANK
  ),
  codebook_row(
    "antibiotic_metrics_china", "NBS_region",
    "NBS geographic region from province metadata.",
    "categorical", "Eastern; Central; Western; Northeastern", "n/a", NA_BLANK
  ),
  codebook_row(
    "antibiotic_metrics_china", "RR_ranking",
    "Province ranking from province metadata (1-31).",
    "integer", "1-31", "n/a", NA_BLANK
  ),
  codebook_row(
    "antibiotic_metrics_china", "k2_groups",
    "k-means cluster label at k=2 from step 06 (env + resistance features).",
    "integer", "1; 2", "n/a",
    "NA until R/06 has attached cluster labels."
  ),
  codebook_row(
    "antibiotic_metrics_china", "k3_groups",
    "k-means cluster label at k=3 from step 06.",
    "integer", "1; 2; 3", "n/a",
    "NA until R/06 has attached cluster labels."
  ),
  codebook_row(
    "antibiotic_metrics_china", "k4_groups",
    "k-means cluster label at k=4 from step 06.",
    "integer", "1; 2; 3; 4", "n/a",
    "NA until R/06 has attached cluster labels."
  )
)

schema_province_groups <- bind_rows(
  codebook_row(
    "province_groups", "province",
    "Provincial-level administrative unit.",
    "categorical", PROVINCE_ALLOWED, "n/a", NA_NONE
  ),
  codebook_row(
    "province_groups", "qu_category",
    "NBS development category from province metadata.",
    "categorical", "First; Second; Third; Fourth", "n/a", NA_NONE
  ),
  codebook_row(
    "province_groups", "qu_subcategory",
    "NBS development subcategory code from province metadata.",
    "categorical", "1; 2; 3; 4; 5; 6; 7", "n/a", NA_NONE
  ),
  codebook_row(
    "province_groups", "NBS_region",
    "NBS geographic region from province metadata.",
    "categorical", "Eastern; Central; Western; Northeastern", "n/a", NA_NONE
  ),
  codebook_row(
    "province_groups", "RR_ranking",
    "Province ranking from province metadata (1-31).",
    "integer", "1-31", "n/a", NA_NONE
  ),
  codebook_row(
    "province_groups", "k2_groups",
    "k-means cluster label at k=2 (env + resistance features).",
    "integer", "1; 2", "n/a", NA_NONE
  ),
  codebook_row(
    "province_groups", "k3_groups",
    "k-means cluster label at k=3.",
    "integer", "1; 2; 3", "n/a", NA_NONE
  ),
  codebook_row(
    "province_groups", "k4_groups",
    "k-means cluster label at k=4.",
    "integer", "1; 2; 3; 4", "n/a", NA_NONE
  )
)

STATIC_SCHEMA <- bind_rows(
  schema_resistance_clean,
  schema_environmental_cleaned,
  schema_environmental_by_site,
  schema_env_abx_per_province,
  schema_env_abx_per_site,
  schema_antibiotic_metrics_china,
  schema_province_groups
)

# --- Main --------------------------------------------------------------------

missing_files <- CURATED_PATHS[!file.exists(CURATED_PATHS)]
if (length(missing_files) > 0) {
  stop(
    "Missing curated tables required for codebook:\n  ",
    paste(missing_files, collapse = "\n  ")
  )
}

# Confirm static schema matches physical columns for non-yearbook tables.
other_tables <- setdiff(names(CURATED_PATHS), "yearbook_clean")
for (tbl in other_tables) {
  cols <- names(read_csv(CURATED_PATHS[[tbl]], n_max = 0, show_col_types = FALSE))
  schema_names <- STATIC_SCHEMA %>%
    filter(table == tbl) %>%
    pull(name)
  if (!identical(sort(cols), sort(schema_names))) {
    stop(
      "Schema mismatch for ", tbl, ".\n  file: ", paste(cols, collapse = ", "),
      "\n  schema: ", paste(schema_names, collapse = ", ")
    )
  }
}

yearbook_codebook <- build_yearbook_codebook(CURATED_PATHS[["yearbook_clean"]])

codebook <- bind_rows(yearbook_codebook, STATIC_SCHEMA) %>%
  arrange(table, name)

expected_n <- 764L + nrow(STATIC_SCHEMA)
if (nrow(codebook) != expected_n) {
  stop("Expected ", expected_n, " codebook rows, got ", nrow(codebook))
}
if (any(duplicated(codebook %>% select(table, name)))) {
  stop("Duplicate (table, name) pairs in codebook")
}

out_path <- "data/output/codebook.csv"
write_csv_reproducible(codebook, out_path)

message(
  "Wrote ", out_path, ": ", nrow(codebook), " rows (",
  sum(codebook$table == "yearbook_clean"), " yearbook metrics + ",
  sum(codebook$table != "yearbook_clean"), " other columns)."
)
