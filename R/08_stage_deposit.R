# Stage Zenodo data deposit package (v1.0.0)
# Copies curated tables into deposit/zenodo_v1/ and writes FILES.md + zip.
# Run from repository root: Rscript R/08_stage_deposit.R

suppressPackageStartupMessages({
  library(fs)
})

VERSION <- "1.0.0"
STAGE_DIR <- file.path("deposit", "zenodo_v1")
ZIP_PATH <- file.path("deposit", sprintf("cn_antibiotics_data_v%s.zip", VERSION))

# Curated products + provenance companion (no third-party raw dumps / shapefile).
FILES <- c(
  "data/intermediate/environmental_cleaned.csv",
  "data/intermediate/environmental_by_site.csv",
  "data/intermediate/resistance_clean.csv",
  "data/output/env_abx_per_province.csv",
  "data/output/env_abx_per_site.csv",
  "data/output/yearbook_core.csv",
  "data/output/yearbook_core_manifest.csv",
  "data/output/yearbook_full.csv",
  "data/output/codebook.csv",
  "data/output/join_key.md",
  "data/output/join_key_antibiotic_classes.csv",
  "data/raw/environmental_data/Data_Sources.csv"
)

# yearbook_clean.csv is the pipeline intermediate; yearbook_full.csv is the
# deposit-facing appendix (same content). Skip intermediate to avoid duplicate.

missing <- FILES[!file_exists(FILES)]
if (length(missing) > 0) {
  stop(
    "Missing required files (run the pipeline first):\n  ",
    paste(missing, collapse = "\n  ")
  )
}

if (dir_exists(STAGE_DIR)) {
  dir_delete(STAGE_DIR)
}
dir_create(STAGE_DIR)

# Flat layout in the deposit for easy browsing on Zenodo.
for (src in FILES) {
  dest <- file.path(STAGE_DIR, path_file(src))
  file_copy(src, dest, overwrite = TRUE)
}

readme_src <- file.path("deposit", "README_deposit.md")
if (!file_exists(readme_src)) {
  stop("Missing deposit/README_deposit.md")
}
file_copy(readme_src, file.path(STAGE_DIR, "README_deposit.md"), overwrite = TRUE)

# Manifest with sizes
info <- file_info(dir_ls(STAGE_DIR, type = "file"))
info <- info[order(path_file(info$path)), ]
manifest_lines <- c(
  sprintf("# Deposit file manifest (v%s)", VERSION),
  "",
  "License: Creative Commons Attribution 4.0 International (CC BY 4.0).",
  "See README_deposit.md for descriptions and recommended joins.",
  "",
  "| File | Size (bytes) |",
  "|---|---:|"
)
for (i in seq_len(nrow(info))) {
  manifest_lines <- c(
    manifest_lines,
    sprintf("| `%s` | %s |", path_file(info$path[i]), format(info$size[i], scientific = FALSE))
  )
}
writeLines(manifest_lines, file.path(STAGE_DIR, "FILES.md"))

# Zip the staged directory (contents at archive root)
if (file_exists(ZIP_PATH)) {
  file_delete(ZIP_PATH)
}
dir_create(path_dir(ZIP_PATH))
# zip::zipr puts STAGE_DIR basename as root; use withr-free base utils via system zip
old_wd <- getwd()
setwd(STAGE_DIR)
on.exit(setwd(old_wd), add = TRUE)
zip_status <- system2(
  "zip",
  c("-r", "-q", file.path("..", path_file(ZIP_PATH)), "."),
  stdout = TRUE,
  stderr = TRUE
)
setwd(old_wd)
on.exit(NULL)
if (!file_exists(ZIP_PATH)) {
  stop("Failed to create zip: ", paste(zip_status, collapse = "\n"))
}

message("Staged ", length(dir_ls(STAGE_DIR)), " files in ", STAGE_DIR)
message("Wrote ", ZIP_PATH, " (", file_info(ZIP_PATH)$size, " bytes)")
