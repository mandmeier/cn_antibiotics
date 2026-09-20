# Tables 1–4 for the Scientific Data manuscript.
#
# Run with working directory = cn_antibiotics/:
#   source("R/tables/tables_build.R")
#
# Writes CSV + Markdown under tables/.
# Table 3 (spot-check) is filled by hand; this script does not overwrite it.
# Table 4 (unit-audit flags) is rebuilt from the pipeline audit.
# Paper PDFs / folders stay in ../figures_sandbox/table03_spotcheck_papers/.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(tibble)
})

out_dir <- "tables"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)

n_rows <- function(path) {
  nrow(read_csv(path, show_col_types = FALSE))
}

n_provinces <- function(path) {
  d <- read_csv(path, show_col_types = FALSE)
  if (!"province" %in% names(d)) {
    return(NA_integer_)
  }
  as.integer(n_distinct(d$province))
}

# Prefer staged deposit meta for env_sources (not always under data/output/meta/).
env_sources_path <- if (file.exists("deposit/zenodo_v1/meta/env_sources.csv")) {
  "deposit/zenodo_v1/meta/env_sources.csv"
} else if (file.exists("data/output/meta/env_sources.csv")) {
  "data/output/meta/env_sources.csv"
} else if (file.exists("data/raw/environmental_data/Data_Sources.csv")) {
  "data/raw/environmental_data/Data_Sources.csv"
} else {
  NA_character_
}

paths <- c(
  "env_province.csv" = "data/output/primary/env_province.csv",
  "resistance_province.csv" = "data/output/primary/resistance_province.csv",
  "yearbook_province.csv" = "data/output/primary/yearbook_province.csv",
  "env_records.csv" = "data/output/supporting/env_records.csv",
  "env_site_records.csv" = "data/output/supporting/env_site_records.csv",
  "env_site.csv" = "data/output/supporting/env_site.csv",
  "yearbook_full.csv" = "data/output/supporting/yearbook_full.csv",
  "yearbook_province_manifest.csv" = "data/output/supporting/yearbook_province_manifest.csv",
  "codebook.csv" = "data/output/meta/codebook.csv",
  "antibiotic_classes.csv" = "data/output/meta/antibiotic_classes.csv"
)

count_for_file <- function(file) {
  if (identical(file, "join_key.md")) {
    return(list(n_rows = NA_integer_, n_provinces = NA_integer_))
  }
  if (identical(file, "env_sources.csv")) {
    if (is.na(env_sources_path) || !file.exists(env_sources_path)) {
      return(list(n_rows = NA_integer_, n_provinces = NA_integer_))
    }
    return(list(
      n_rows = as.integer(n_rows(env_sources_path)),
      n_provinces = NA_integer_
    ))
  }
  p <- unname(paths[file])
  if (length(p) == 0 || is.na(p) || !file.exists(p)) {
    return(list(n_rows = NA_integer_, n_provinces = NA_integer_))
  }
  list(
    n_rows = as.integer(n_rows(p)),
    n_provinces = n_provinces(p)
  )
}

# =============================================================================
# Table 1 — Data files
# =============================================================================

table01 <- tibble::tribble(
  ~folder, ~file, ~grain, ~purpose,
  "primary", "env_province.csv",
  "province-median (sample type × province × antibiotic)",
  "Median environmental concentration for province-level joins with CARSS",
  "primary", "resistance_province.csv",
  "province-year (year × province × species × antibiotic)",
  "Cleaned CARSS clinical resistance panel (31 provinces; 2019–2024)",
  "primary", "yearbook_province.csv",
  "province-year (year × province × metric)",
  "24 One Health yearbook covariates (pop/urban 2015–2024; others 2024 vintage)",
  "supporting", "env_records.csv",
  "measurement (province-oriented)",
  "Harmonized env concentrations after QC; feeds env_province",
  "supporting", "env_site_records.csv",
  "measurement (site-oriented)",
  "Same harmonization with location, season, lon, lat retained",
  "supporting", "env_site.csv",
  "site-median (sample type × location × season × antibiotic)",
  "Site-level medians for spatial or seasonal reuse",
  "supporting", "yearbook_full.csv",
  "province-metric",
  "All 764 cleaned yearbook metrics",
  "supporting", "yearbook_province_manifest.csv",
  "metric list",
  "Theme tags and definitions for the 24 core yearbook metrics",
  "meta", "codebook.csv",
  "variable dictionary",
  "Column definitions for curated tables",
  "meta", "join_key.md",
  "join documentation",
  "31 provinces, 15 shared compounds, matrix ↔ unit rules",
  "meta", "antibiotic_classes.csv",
  "lookup",
  "Antibiotic → pharmacological class (10 classes)",
  "meta", "env_sources.csv",
  "literature provenance",
  "Publication ID → citation for environmental rows"
)

counts <- lapply(table01$file, count_for_file)
table01$n_rows <- vapply(counts, `[[`, integer(1), "n_rows")
table01$n_provinces <- vapply(counts, `[[`, integer(1), "n_provinces")
table01 <- table01 %>%
  select(folder, file, grain, n_rows, n_provinces, purpose)

write_csv(table01, file.path(out_dir, "table01_data_files.csv"), na = "")

# =============================================================================
# Table 2 — Decision rules
# =============================================================================

table02 <- tibble::tribble(
  ~rule_id, ~domain, ~rule, ~action, ~script,
  "E1", "Environmental",
  "Province mapping",
  "Map free-text places to the 31 CARSS/NBS English names (Xizang → Tibet). Drop Hong Kong, Macau, Taiwan, multi-province / nationwide / basin / empty labels.",
  "R/01",
  "E2", "Environmental",
  "Sample type / matrix",
  "Collapse free-text sample labels into sample types and six matrices (soil, sediment, sludge, surface water, wastewater influent, wastewater effluent). Drop solid waste.",
  "R/01",
  "E3", "Environmental",
  "Antibiotic names and classes",
  "INN-style curation (synonyms, salts). Drop class totals, mixed 'all antibiotics', and non-antibiotics. Assign pharmacological class from antibiotic_group_lookup (TMP-SMX → Sulfonamides).",
  "R/01",
  "E4", "Environmental",
  "Year midpoint",
  "Parse single years and ranges; for a range take floor((start+end)/2). Missing year stays missing.",
  "R/01",
  "E5", "Environmental",
  "Unit conversion and incompatible pairs",
  "Convert to matrix targets: solids ng/g or ng/g dw; liquids ng/L. Drop incompatible unit–matrix pairs rather than invent a conversion.",
  "R/01",
  "E6", "Environmental",
  "Source-rank deduplication",
  "When identical measurements appear in multiple references, keep the highest source-rank (NEW_* > CAND_* > N* > Zhang numeric pub_id). Province grain also collapses season so the same value is not double-counted.",
  "R/01",
  "E7", "Environmental",
  "Province median aggregation",
  "Median by sample type × province × antibiotic → env_province.csv. Prefer median over mean/max because literature points are not a probability sample.",
  "R/04",
  "E8", "Environmental",
  "Zeros as LOD/2",
  "Within each aggregation group, estimate LOD as the lowest positive mean; replace zero with LOD/2. If a group has no positives, drop it.",
  "R/04, R/04b",
  "E9", "Environmental",
  "Site grain",
  "Retain location, season, lon, lat in env_site_records; site medians in env_site.csv use the same LOD/2 + median rules keyed by site/season.",
  "R/01, R/04b",
  "R1", "Resistance (CARSS)",
  "Province filter",
  "Keep the 31 provincial units; drop the national aggregate.",
  "R/02",
  "R2", "Resistance (CARSS)",
  "Combo / multiple AST filter",
  "Drop combination AST categories (slash names, 'compound', aggregates) so the table emphasizes single-agent results. Retain Trimethoprim/Sulfamethoxazole as co-formulated AST (no separate TMP/SMX rows in this extract); class as Sulfonamides.",
  "R/02",
  "R3", "Resistance (CARSS)",
  "Name standardization",
  "Correct spelling variants (e.g. Polymixin B → Polymyxin B). Assign the same pharmacological classes as the environmental table.",
  "R/02",
  "Y1", "Yearbook (NBS)",
  "Province and unit parse",
  "Rename Xizang → Tibet. Parse unit suffixes into a unit column; strip them from metric names.",
  "R/03",
  "Y2", "Yearbook (NBS)",
  "Core vs full extract",
  "Primary yearbook_province.csv: 24 One Health metrics (livestock, wastewater, hospitals, GDP, urbanization, environment) with a year column. Supporting yearbook_full.csv: all cleaned metrics.",
  "R/03, R/07",
  "Y3", "Yearbook (NBS)",
  "Year coverage rule",
  "Population and urban share expanded to 2015–2024. Other 22 core metrics are the 2024 vintage only. Do not treat as a long multi-year panel for all metrics.",
  "R/07",
  "J1", "Join keys",
  "Merge keys",
  "Merge on English province (and antibiotic for env × CARSS). Optional year join for yearbook × CARSS. Cross-domain compound analyses start from the 15 exact name overlaps in meta/join_key.md. Compare concentrations only within matrix ↔ unit.",
  "R/05, R/06"
)

write_csv(table02, file.path(out_dir, "table02_decision_rules.csv"))

# =============================================================================
# Table 3 — Spot-check (do not overwrite a filled selection)
# =============================================================================
# Papers are selected by R/tables/table03_select_spotcheck_papers.py
# (seed 20250918). Paper folders stay under ../figures_sandbox/.
# Keep table03_spotcheck.csv if it already lists references / scores.

spotcheck_path <- file.path(out_dir, "table03_spotcheck.csv")
if (!file.exists(spotcheck_path)) {
  message(
    "table03_spotcheck.csv missing — run ",
    "python3 R/tables/table03_select_spotcheck_papers.py ",
    "then fill agreement columns."
  )
}

# =============================================================================
# Table 4 — Unit-audit flags (automated from pipeline)
# =============================================================================

flags_path <- "data/intermediate/validation/suspicious_conversions.csv"
stopifnot(file.exists(flags_path))

n_audited <- nrow(read_csv(
  "data/intermediate/validation/unit_conversion_audit.csv",
  show_col_types = FALSE
))

flags <- read_csv(flags_path, show_col_types = FALSE) %>%
  mutate(
    flag_id = row_number(),
    # Same ID as Data_Sources / env_sources pub_id and env_records$reference_number.
    publication_id = as.character(reference_number),
    retained_in_env_records = TRUE,
    disposition = dplyr::case_when(
      str_detect(flag_codes, "high_converted") ~
        "keep — human-confirmed against source",
      TRUE ~
        "keep — retained as near-LOD / below review band"
    ),
    disposition_note = dplyr::case_when(
      str_detect(flag_codes, "high_converted") ~
        paste(
          "Li et al. 2008 Table 3: OTC in activated sludge 4,363 ± 520 (SBR)",
          "and 3,763 ± 353 mg/kg (continuous-flow); mean 4,063 / max 4,363",
          "match the two reactors. Extreme but source-verified production-WWTP sludge."
        ),
      TRUE ~
        paste(
          "Converted mean < 0.01 (audit floor); alt_unit_more_plausible is a weak",
          "signal for near-LOD detections. Retained as reported; no evidence of",
          "unit mislabeling."
        )
    )
  ) %>%
  select(
    flag_id,
    publication_id,
    province,
    sample_type,
    matrix,
    sample_year,
    antibiotic,
    unit_raw,
    mean_raw,
    unit_converted,
    mean_converted,
    flag_codes,
    flag_reasons,
    suggested_unit_note,
    retained_in_env_records,
    disposition,
    disposition_note
  )

write_csv(flags, file.path(out_dir, "table04_unit_flags.csv"))

# =============================================================================
# Markdown companions
# =============================================================================

fmt_n <- function(x) {
  ifelse(is.na(x), "—", format(x, big.mark = ","))
}

md_escape <- function(x) {
  str_replace_all(as.character(x), "\\|", "\\\\|")
}

write_section_rows <- function(df) {
  vapply(seq_len(nrow(df)), function(i) {
    sprintf(
      "| `%s` | %s | %s | %s | %s |",
      df$file[[i]], md_escape(df$grain[[i]]),
      fmt_n(df$n_rows[[i]]), fmt_n(df$n_provinces[[i]]),
      md_escape(df$purpose[[i]])
    )
  }, character(1))
}

header <- c(
  "| File | Grain | n rows | n provinces | Purpose |",
  "|---|---|---:|---:|---|"
)

lines01 <- c(
  "# Table 1. Curated data files",
  "",
  paste0(
    "Row counts refreshed from pipeline outputs (",
    format(Sys.Date(), "%Y-%m-%d"),
    "). Join on English `province` (and `antibiotic` for env × CARSS; ",
    "optionally `year` for yearbook × CARSS)."
  ),
  "",
  "## Primary (province join set)",
  "",
  header,
  write_section_rows(table01 %>% filter(folder == "primary")),
  "",
  "## Supporting",
  "",
  header,
  write_section_rows(table01 %>% filter(folder == "supporting")),
  "",
  "## Meta",
  "",
  header,
  write_section_rows(table01 %>% filter(folder == "meta"))
)
writeLines(lines01, file.path(out_dir, "table01_data_files.md"))

lines02 <- c(
  "# Table 2. Decision rules",
  "",
  "Harmonization and curation rules applied by the R pipeline. One row per rule.",
  "",
  "| ID | Domain | Rule | Action | Script |",
  "|---|---|---|---|---|",
  vapply(seq_len(nrow(table02)), function(i) {
    sprintf(
      "| %s | %s | %s | %s | `%s` |",
      table02$rule_id[[i]],
      md_escape(table02$domain[[i]]),
      md_escape(table02$rule[[i]]),
      md_escape(table02$action[[i]]),
      table02$script[[i]]
    )
  }, character(1))
)
writeLines(lines02, file.path(out_dir, "table02_decision_rules.md"))

lines03 <- c(
  "# Table 3. Independent spot-check of 20 papers",
  "",
  if (file.exists(spotcheck_path)) {
    paste(
      "Papers selected (seed `20250918`). See `table03_spotcheck.csv`.",
      "Paper folders stay under `../figures_sandbox/table03_spotcheck_papers/`.",
      "Do not re-roll the sample unless intentionally changing the seed."
    )
  } else {
    paste(
      "Spot-check sample not yet built. Run",
      "`python3 R/tables/table03_select_spotcheck_papers.py`",
      "then fill agreement columns in `table03_spotcheck.csv`."
    )
  }
)
writeLines(lines03, file.path(out_dir, "table03_spotcheck.md"))

lines04 <- c(
  "# Table 4. Unit-conversion audit flags (n = 27)",
  "",
  paste0(
    "From `data/intermediate/validation/suspicious_conversions.csv` ",
    "(audited ", format(n_audited, big.mark = ","),
    " rows with a convertible mean). ",
    "All 27 flagged rows are **retained** in `env_records.csv` at the converted value. ",
    "Human disposition: 1 `high_converted` row (Oxytetracycline, publication_id 223) ",
    "confirmed against Li et al. 2008 Table 3; the other 26 are very low means ",
    "flagged by `alt_unit_more_plausible` and retained as near-LOD / below the audit floor."
  ),
  "",
  paste(
    "| flag_id | publication_id | province | matrix | antibiotic |",
    "unit_raw → unit_converted | mean_converted | flag | disposition |"
  ),
  "|---:|---|---|---|---|---|---:|---|---|",
  vapply(seq_len(nrow(flags)), function(i) {
    sprintf(
      "| %s | %s | %s | %s | %s | %s → %s | %s | %s | %s |",
      flags$flag_id[[i]],
      flags$publication_id[[i]],
      flags$province[[i]],
      flags$matrix[[i]],
      flags$antibiotic[[i]],
      flags$unit_raw[[i]],
      flags$unit_converted[[i]],
      signif(flags$mean_converted[[i]], 4),
      flags$flag_codes[[i]],
      flags$disposition[[i]]
    )
  }, character(1))
)
writeLines(lines04, file.path(out_dir, "table04_unit_flags.md"))

message("Wrote tables to ", normalizePath(out_dir))
message("  table01_data_files.csv/.md")
message("  table02_decision_rules.csv/.md")
message("  table03_spotcheck.md")
message("  table04_unit_flags.csv/.md")
if (file.exists(spotcheck_path)) {
  message("  table03_spotcheck.csv preserved (not overwritten)")
}
