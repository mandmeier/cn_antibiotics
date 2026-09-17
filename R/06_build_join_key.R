# Build a one-page join key for env / CARSS / yearbook interoperability.
# Outputs: data/output/meta/join_key.md + antibiotic_classes.csv

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

EXPECTED_SHARED_N <- 15L

PATHS <- list(
  environmental = "data/output/supporting/env_records.csv",
  resistance = "data/output/primary/resistance_province.csv",
  yearbook = "data/output/supporting/yearbook_full.csv",
  class_lookup = "data/raw/reference/antibiotic_group_lookup.csv"
)

missing_files <- PATHS[!file.exists(unlist(PATHS))]
if (length(missing_files) > 0) {
  stop(
    "Missing files required for join key:\n  ",
    paste(missing_files, collapse = "\n  ")
  )
}

environmental <- read_csv(PATHS$environmental, show_col_types = FALSE)
resistance <- read_csv(PATHS$resistance, show_col_types = FALSE)
yearbook <- read_csv(PATHS$yearbook, show_col_types = FALSE)
class_lookup <- read_csv(PATHS$class_lookup, show_col_types = FALSE) %>%
  arrange(antibiotic)

# --- Province key ------------------------------------------------------------

assert_provinces <- function(provs, label) {
  got <- sort(unique(provs))
  if (!identical(got, sort(PROVINCES))) {
    stop(
      "Province mismatch in ", label, ".\n  expected: ",
      paste(PROVINCES, collapse = ", "),
      "\n  got: ", paste(got, collapse = ", ")
    )
  }
}

assert_provinces(environmental$province, "env_records")
assert_provinces(resistance$province, "resistance_province")
assert_provinces(yearbook$province, "yearbook_full")

# --- env ∩ CARSS compounds ---------------------------------------------------

shared <- sort(intersect(
  unique(environmental$antibiotic),
  unique(resistance$antibiotic)
))

if (length(shared) != EXPECTED_SHARED_N) {
  stop(
    "Expected ", EXPECTED_SHARED_N, " env∩CARSS compounds, found ",
    length(shared), ": ", paste(shared, collapse = ", ")
  )
}

lookup_map <- setNames(
  class_lookup$group_of_antibiotic,
  class_lookup$antibiotic
)
shared_class <- lookup_map[shared]
if (any(is.na(shared_class))) {
  stop(
    "Shared compounds missing from class lookup: ",
    paste(shared[is.na(shared_class)], collapse = ", ")
  )
}

shared_tbl <- tibble(
  antibiotic = shared,
  antibiotic_class = unname(shared_class)
)

# --- Antibiotic-class lookup CSV ---------------------------------------------

class_out <- class_lookup %>%
  rename(antibiotic_class = group_of_antibiotic)

dir.create("data/output/meta", showWarnings = FALSE, recursive = TRUE)
class_csv_path <- "data/output/meta/antibiotic_classes.csv"
write_csv_reproducible(class_out, class_csv_path)

# --- Markdown join key -------------------------------------------------------

md_escape <- function(x) {
  str_replace_all(x, "\\|", "\\\\|")
}

bullet_list <- function(items) {
  paste0("- ", items, collapse = "\n")
}

md_table <- function(df) {
  cols <- names(df)
  header <- paste0("| ", paste(cols, collapse = " | "), " |")
  sep <- paste0("| ", paste(rep("---", length(cols)), collapse = " | "), " |")
  rows <- apply(df, 1, function(r) {
    paste0("| ", paste(md_escape(as.character(r)), collapse = " | "), " |")
  })
  paste(c(header, sep, rows), collapse = "\n")
}

# Class-grouped compact lists for the full lookup (fits one page better than 133 rows).
by_class <- class_out %>%
  group_by(antibiotic_class) %>%
  summarise(
    compounds = paste(antibiotic, collapse = "; "),
    .groups = "drop"
  ) %>%
  arrange(antibiotic_class)

class_sections <- vapply(
  seq_len(nrow(by_class)),
  function(i) {
    paste0(
      "**", by_class$antibiotic_class[i], ".** ",
      by_class$compounds[i]
    )
  },
  character(1)
)

lines <- c(
  "# Join key",
  "",
  paste(
    "Merge curated tables on English `province` names and standardized",
    "`antibiotic` names. `yearbook_province.csv` also carries a `year`",
    "column (population and urban share: 2015–2024; other core metrics:",
    "2024 only) for optional alignment with CARSS years. Environmental",
    "concentrations are only comparable within the matrix ↔ unit system",
    "below. Column definitions: [`codebook.csv`](codebook.csv)."
  ),
  "",
  "## 1. Province key (31)",
  "",
  paste(
    "Canonical provincial-level units (CARSS / NBS English names;",
    "`Tibet` for Xizang)."
  ),
  "",
  bullet_list(PROVINCES),
  "",
  "## 2. env ∩ CARSS compounds (15)",
  "",
  paste(
    "Exact name intersection of `env_records` and",
    "`resistance_province`. Use these for cross-domain joins; env-only and",
    "CARSS-only compounds remain in their domains."
  ),
  "",
  md_table(shared_tbl),
  "",
  "## 3. Matrix ↔ unit rules",
  "",
  paste(
    "Target units after conversion in `R/utils/environmental_units.R`",
    "(`target_concentration_unit`). Incompatible matrix–unit pairs are",
    "dropped in R/01."
  ),
  "",
  "| Matrix | Target unit |",
  "| --- | --- |",
  "| soil; sediment; sludge | `ng/g` |",
  "| soil; sediment; sludge (source unit dry-weight) | `ng/g dw` |",
  "| surface water; wastewater influent; wastewater effluent | `ng/L` |",
  "",
  "## 4. Antibiotic-class lookup",
  "",
  paste0(
    length(unique(class_out$antibiotic)), " compounds → ",
    length(unique(class_out$antibiotic_class)), " pharmacological classes",
    " (from `data/raw/reference/antibiotic_group_lookup.csv`).",
    " Machine-readable copy: [`antibiotic_classes.csv`]",
    "(antibiotic_classes.csv)."
  ),
  "",
  paste(class_sections, collapse = "\n\n"),
  ""
)

md_path <- "data/output/meta/join_key.md"
writeLines(lines, md_path, useBytes = TRUE)

message(
  "Wrote ", md_path, " and ", class_csv_path, " (",
  length(PROVINCES), " provinces; ", length(shared),
  " shared compounds; ", nrow(class_out), " class mappings)."
)