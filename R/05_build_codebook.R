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
  yearbook_full = "data/output/supporting/yearbook_full.csv",
  yearbook_province = "data/output/primary/yearbook_province.csv",
  resistance_province = "data/output/primary/resistance_province.csv",
  env_records = "data/output/supporting/env_records.csv",
  env_site_records = "data/output/supporting/env_site_records.csv",
  env_province = "data/output/primary/env_province.csv",
  env_site = "data/output/supporting/env_site.csv"
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
      table = "yearbook_full",
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

schema_resistance_province <- bind_rows(
  codebook_row(
    "resistance_province", "year",
    "CARSS surveillance year for the resistance panel.",
    "integer", "2019; 2020; 2021; 2022; 2023; 2024", "n/a", NA_NONE
  ),
  codebook_row(
    "resistance_province", "province",
    "Provincial-level administrative unit (English name; Tibet for Xizang).",
    "categorical", PROVINCE_ALLOWED, "n/a", NA_NONE
  ),
  codebook_row(
    "resistance_province", "bacteria_species",
    "Pathogen species in the CARSS panel.",
    "categorical",
    "A. baumannii; E. coli; K. pneumoniae; P. aeruginosa; S. aureus",
    "n/a", NA_NONE
  ),
  codebook_row(
    "resistance_province", "antibiotic",
    "Antibiotic tested; combo drugs dropped except Trimethoprim/Sulfamethoxazole.",
    "string", "drug names as cleaned from CARSS", "n/a", NA_NONE
  ),
  codebook_row(
    "resistance_province", "antibiotic_class",
    "Pharmacological class assigned from the project antibiotic group lookup.",
    "categorical", CLASS_ALLOWED, "n/a",
    "NA if the antibiotic is unmapped in the lookup."
  ),
  codebook_row(
    "resistance_province", "total_n_strains",
    "Number of isolates tested for this province-year-bug-drug cell.",
    "integer", "non-negative integer", "strains", NA_BLANK
  ),
  codebook_row(
    "resistance_province", "resistant_percent",
    "Percent of isolates classified resistant.",
    "percent", "0-100", "%", NA_BLANK
  ),
  codebook_row(
    "resistance_province", "resistant_n_strains",
    "Count of isolates classified resistant.",
    "integer", "non-negative integer", "strains", NA_BLANK
  ),
  codebook_row(
    "resistance_province", "intermediate_percent",
    "Percent of isolates classified intermediate.",
    "percent", "0-100", "%", NA_BLANK
  ),
  codebook_row(
    "resistance_province", "intermediate_n_strains",
    "Count of isolates classified intermediate.",
    "integer", "non-negative integer", "strains", NA_BLANK
  ),
  codebook_row(
    "resistance_province", "sensitive_percent",
    "Percent of isolates classified sensitive/susceptible.",
    "percent", "0-100", "%", NA_BLANK
  ),
  codebook_row(
    "resistance_province", "sensitive_n_strains",
    "Count of isolates classified sensitive/susceptible.",
    "integer", "non-negative integer", "strains", NA_BLANK
  )
)

schema_env_records <- bind_rows(
  codebook_row(
    "env_records", "sample_type",
    "Literature sample category (finer than matrix).",
    "categorical", SAMPLE_TYPE_ALLOWED, "n/a", NA_NONE
  ),
  codebook_row(
    "env_records", "matrix",
    "Harmonized environmental matrix for cross-study comparison.",
    "categorical", MATRIX_ALLOWED, "n/a", NA_NONE
  ),
  codebook_row(
    "env_records", "province",
    "Province assigned to the measurement (English name).",
    "categorical", PROVINCE_ALLOWED, "n/a", NA_NONE
  ),
  codebook_row(
    "env_records", "sample_year",
    "Year of environmental sampling when reported.",
    "integer", "calendar year", "n/a", NA_BLANK
  ),
  codebook_row(
    "env_records", "antibiotic",
    "Antibiotic analyte name after cleaning.",
    "string", "antibiotic names as cleaned from literature sources", "n/a",
    NA_NONE
  ),
  codebook_row(
    "env_records", "antibiotic_class",
    "Pharmacological class assigned from the project antibiotic group lookup.",
    "categorical", CLASS_ALLOWED, "n/a",
    "NA if the antibiotic is unmapped in the lookup."
  ),
  codebook_row(
    "env_records", "mean_concentration",
    "Reported or derived mean environmental concentration.",
    "numeric", "non-negative numeric", "see concentration_unit", NA_BLANK
  ),
  codebook_row(
    "env_records", "max_concentration",
    "Reported maximum environmental concentration when available.",
    "numeric", "non-negative numeric", "see concentration_unit", NA_BLANK
  ),
  codebook_row(
    "env_records", "concentration_unit",
    "Unit of mean_concentration and max_concentration after conversion.",
    "categorical", CONC_UNIT_ALLOWED, "n/a", NA_NONE
  ),
  codebook_row(
    "env_records", "reference_number",
    "Publication ID linking to Data_Sources / literature extract.",
    "string", "reference IDs as in source extracts (numeric or N-prefixed)",
    "n/a", NA_BLANK
  )
)

schema_env_site_records <- bind_rows(
  codebook_row(
    "env_site_records", "sample_type",
    "Literature sample category (finer than matrix).",
    "categorical", SAMPLE_TYPE_ALLOWED, "n/a", NA_NONE
  ),
  codebook_row(
    "env_site_records", "matrix",
    "Harmonized environmental matrix for cross-study comparison.",
    "categorical", MATRIX_ALLOWED, "n/a", NA_NONE
  ),
  codebook_row(
    "env_site_records", "province",
    "Province assigned to the measurement (English name).",
    "categorical", PROVINCE_ALLOWED, "n/a", NA_NONE
  ),
  codebook_row(
    "env_site_records", "location",
    "Heterogeneous literature site label (not a formal site ID).",
    "string", "free-text location strings from source studies", "n/a",
    NA_BLANK
  ),
  codebook_row(
    "env_site_records", "season",
    "Sampling season or month: Zhang month 1-12 or sparse supplemental text.",
    "string", "1-12 or free-text season labels", "n/a", NA_BLANK
  ),
  codebook_row(
    "env_site_records", "lon",
    "Site longitude when geocoded or reported.",
    "numeric", "decimal degrees", "degrees", NA_BLANK
  ),
  codebook_row(
    "env_site_records", "lat",
    "Site latitude when geocoded or reported.",
    "numeric", "decimal degrees", "degrees", NA_BLANK
  ),
  codebook_row(
    "env_site_records", "sample_year",
    "Year of environmental sampling when reported.",
    "integer", "calendar year", "n/a", NA_BLANK
  ),
  codebook_row(
    "env_site_records", "antibiotic",
    "Antibiotic analyte name after cleaning.",
    "string", "antibiotic names as cleaned from literature sources", "n/a",
    NA_NONE
  ),
  codebook_row(
    "env_site_records", "antibiotic_class",
    "Pharmacological class assigned from the project antibiotic group lookup.",
    "categorical", CLASS_ALLOWED, "n/a",
    "NA if the antibiotic is unmapped in the lookup."
  ),
  codebook_row(
    "env_site_records", "mean_concentration",
    "Reported or derived mean environmental concentration.",
    "numeric", "non-negative numeric", "see concentration_unit", NA_BLANK
  ),
  codebook_row(
    "env_site_records", "max_concentration",
    "Reported maximum environmental concentration when available.",
    "numeric", "non-negative numeric", "see concentration_unit", NA_BLANK
  ),
  codebook_row(
    "env_site_records", "concentration_unit",
    "Unit of mean_concentration and max_concentration after conversion.",
    "categorical", CONC_UNIT_ALLOWED, "n/a", NA_NONE
  ),
  codebook_row(
    "env_site_records", "reference_number",
    "Publication ID linking to Data_Sources / literature extract.",
    "string", "reference IDs as in source extracts (numeric or N-prefixed)",
    "n/a", NA_BLANK
  )
)

schema_env_province <- bind_rows(
  codebook_row(
    "env_province", "sample_type",
    "Literature sample category aggregated to province median.",
    "categorical", SAMPLE_TYPE_ALLOWED, "n/a", NA_NONE
  ),
  codebook_row(
    "env_province", "province",
    "Province for the median concentration.",
    "categorical", PROVINCE_ALLOWED, "n/a", NA_NONE
  ),
  codebook_row(
    "env_province", "antibiotic",
    "Antibiotic analyte for the province median.",
    "string", "antibiotic names as cleaned from literature sources", "n/a",
    NA_NONE
  ),
  codebook_row(
    "env_province", "antibiotic_class",
    "Pharmacological class assigned from the project antibiotic group lookup.",
    "categorical", CLASS_ALLOWED, "n/a",
    "NA if the antibiotic is unmapped in the lookup."
  ),
  codebook_row(
    "env_province", "median_concentration",
    "Median environmental concentration across sites/records in the cell.",
    "numeric", "non-negative numeric", "see concentration_unit", NA_BLANK
  ),
  codebook_row(
    "env_province", "concentration_unit",
    "Unit of median_concentration.",
    "categorical", CONC_UNIT_ALLOWED, "n/a", NA_NONE
  ),
  codebook_row(
    "env_province", "sample_year",
    "Representative or retained sample year for the aggregated cell.",
    "integer", "calendar year", "n/a", NA_BLANK
  ),
  codebook_row(
    "env_province", "reference_number",
    "Publication ID retained with the aggregated cell.",
    "string", "reference IDs as in source extracts (numeric or N-prefixed)",
    "n/a", NA_BLANK
  )
)

schema_env_site <- bind_rows(
  codebook_row(
    "env_site", "sample_type",
    "Literature sample category for the site median.",
    "categorical", SAMPLE_TYPE_ALLOWED, "n/a", NA_NONE
  ),
  codebook_row(
    "env_site", "province",
    "Province of the sampling site.",
    "categorical", PROVINCE_ALLOWED, "n/a", NA_NONE
  ),
  codebook_row(
    "env_site", "location",
    "Heterogeneous literature site label (not a formal site ID).",
    "string", "free-text location strings from source studies", "n/a",
    NA_BLANK
  ),
  codebook_row(
    "env_site", "season",
    "Sampling season or month: Zhang month 1-12 or sparse supplemental text.",
    "string", "1-12 or free-text season labels", "n/a", NA_BLANK
  ),
  codebook_row(
    "env_site", "lon",
    "Site longitude when geocoded or reported.",
    "numeric", "decimal degrees", "degrees", NA_BLANK
  ),
  codebook_row(
    "env_site", "lat",
    "Site latitude when geocoded or reported.",
    "numeric", "decimal degrees", "degrees", NA_BLANK
  ),
  codebook_row(
    "env_site", "antibiotic",
    "Antibiotic analyte for the site median.",
    "string", "antibiotic names as cleaned from literature sources", "n/a",
    NA_NONE
  ),
  codebook_row(
    "env_site", "antibiotic_class",
    "Pharmacological class assigned from the project antibiotic group lookup.",
    "categorical", CLASS_ALLOWED, "n/a",
    "NA if the antibiotic is unmapped in the lookup."
  ),
  codebook_row(
    "env_site", "median_concentration",
    "Median environmental concentration for the site cell.",
    "numeric", "non-negative numeric", "see concentration_unit", NA_BLANK
  ),
  codebook_row(
    "env_site", "concentration_unit",
    "Unit of median_concentration.",
    "categorical", CONC_UNIT_ALLOWED, "n/a", NA_NONE
  ),
  codebook_row(
    "env_site", "sample_year",
    "Sample year retained with the site cell.",
    "integer", "calendar year", "n/a", NA_BLANK
  ),
  codebook_row(
    "env_site", "reference_number",
    "Publication ID retained with the site cell.",
    "string", "reference IDs as in source extracts (numeric or N-prefixed)",
    "n/a", NA_BLANK
  )
)

schema_yearbook_province <- bind_rows(
  codebook_row(
    "yearbook_province", "year",
    paste(
      "Calendar year for the covariate value. Population and urban share cover",
      "all years available in the extract (2015-2024 via E02_05/E02_06 series).",
      "Other core metrics are a single vintage (currently 2024)."
    ),
    "integer", "2015; 2016; 2017; 2018; 2019; 2020; 2021; 2022; 2023; 2024",
    "n/a", NA_NONE
  ),
  codebook_row(
    "yearbook_province", "province",
    "Provincial-level administrative unit (English name; Tibet for Xizang).",
    "categorical", PROVINCE_ALLOWED, "n/a", NA_NONE
  ),
  codebook_row(
    "yearbook_province", "metric",
    "One of 24 curated One Health yearbook covariates (see yearbook_province_manifest).",
    "categorical",
    paste(
      "E02_07_Total_Population_year_end; E02_07_Urban_Population_Proportion;",
      "E03_09_Gross_Regional_Product; E03_09_Per_Capita_Gross_Regional_Product;",
      "E07_06_Expenditure_for_Health_Care;",
      "E08_14_Common_Industrial_Solid_Wastes_Generated; E08_14_Hazardous_Wastes_Generated;",
      "E08_33_Investment_in_Urban_Environmental_Infrastructure;",
      "E12_03_Gross_Output_Value_Animal_Husbandry; E12_13_Cattle_and_Buffaloes;",
      "E12_13_Hogs_year_end; E12_13_Sheep_and_Goats_year_end; E12_13_Slaughtered_Hogs;",
      "E12_14_Output_of_Meat; E12_14_Poultry_Eggs; E22_01_Hospitals;",
      "E22_02_Health_Technical_Personnel; E22_02_Licensed_Physicians; E22_07_Beds_Total;",
      "E25_05_Total_Annual_Volume_of_Tap_Water_Supply;",
      "E25_08_Daily_Disposal_Capacity_of_City_Sewage;",
      "E25_13_Centralized_Treatment_at_Sewage_Treatment_Plants;",
      "E25_13_Rate_of_Domestic_Garbage_Harmless_Treatment;",
      "E25_13_Waste_Water_Treatment_Rate"
    ),
    "n/a", NA_NONE
  ),
  codebook_row(
    "yearbook_province", "value",
    "Numeric value of the metric for the province-year.",
    "numeric", "non-negative numeric; see unit", "see unit",
    paste(
      "NA/blank means the metric was not reported for that province-year",
      "in the source extract (9 missing cells in the current core table)."
    )
  ),
  codebook_row(
    "yearbook_province", "unit",
    "Unit of value for the metric (constant within metric).",
    "categorical",
    paste(
      "10000_cu_m; 10000_heads; 10000_persons; 10000_tons; 10000_yuan;",
      "100_million_yuan; bed; counts; percent; person; yuan"
    ),
    "n/a", NA_NONE
  )
)

STATIC_SCHEMA <- bind_rows(
  schema_resistance_province,
  schema_env_records,
  schema_env_site_records,
  schema_env_province,
  schema_env_site,
  schema_yearbook_province
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
other_tables <- setdiff(names(CURATED_PATHS), "yearbook_full")
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

yearbook_codebook <- build_yearbook_codebook(CURATED_PATHS[["yearbook_full"]])

codebook <- bind_rows(yearbook_codebook, STATIC_SCHEMA) %>%
  arrange(table, name)

expected_n <- 764L + nrow(STATIC_SCHEMA)
if (nrow(codebook) != expected_n) {
  stop("Expected ", expected_n, " codebook rows, got ", nrow(codebook))
}
if (any(duplicated(codebook %>% select(table, name)))) {
  stop("Duplicate (table, name) pairs in codebook")
}

dir.create("data/output/meta", showWarnings = FALSE, recursive = TRUE)
out_path <- "data/output/meta/codebook.csv"
write_csv_reproducible(codebook, out_path)

message(
  "Wrote ", out_path, ": ", nrow(codebook), " rows (",
  sum(codebook$table == "yearbook_full"), " yearbook metrics + ",
  sum(codebook$table != "yearbook_full"), " other columns)."
)
