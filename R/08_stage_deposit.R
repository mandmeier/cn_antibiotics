# Stage Zenodo data deposit package (v1.0.0)
# Copies curated tables into deposit/zenodo_v1/ and writes FILES.md + zip.
# Run from repository root: Rscript R/08_stage_deposit.R

suppressPackageStartupMessages({
  library(fs)
})

VERSION <- "1.0.0"
STAGE_DIR <- file.path("deposit", "zenodo_v1")
ZIP_PATH <- file.path("deposit", sprintf("cn_antibiotics_data_v%s.zip", VERSION))

# Preserve primary / supporting / meta hierarchy in the deposit.
PRIMARY_FILES <- c(
  "data/output/primary/env_province.csv",
  "data/output/primary/resistance_province.csv",
  "data/output/primary/yearbook_province.csv"
)
SUPPORTING_FILES <- c(
  "data/output/supporting/env_records.csv",
  "data/output/supporting/env_site_records.csv",
  "data/output/supporting/env_site.csv",
  "data/output/supporting/yearbook_full.csv",
  "data/output/supporting/yearbook_province_manifest.csv"
)
META_FILES <- c(
  "data/output/meta/codebook.csv",
  "data/output/meta/join_key.md",
  "data/output/meta/antibiotic_classes.csv"
)
# Provenance companion (copied into meta/ as env_sources.csv).
ENV_SOURCES_SRC <- "data/raw/environmental_data/Data_Sources.csv"

FILES <- c(PRIMARY_FILES, SUPPORTING_FILES, META_FILES, ENV_SOURCES_SRC)

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
dir_create(file.path(STAGE_DIR, "primary"))
dir_create(file.path(STAGE_DIR, "supporting"))
dir_create(file.path(STAGE_DIR, "meta"))

rel_dest <- function(src) {
  if (identical(src, ENV_SOURCES_SRC)) {
    return(file.path("meta", "env_sources.csv"))
  }
  sub("^data/output/", "", src)
}

for (src in FILES) {
  dest <- file.path(STAGE_DIR, rel_dest(src))
  dir_create(path_dir(dest))
  file_copy(src, dest, overwrite = TRUE)
}

readme_src <- file.path("deposit", "README_deposit.md")
if (!file_exists(readme_src)) {
  stop("Missing deposit/README_deposit.md")
}
file_copy(readme_src, file.path(STAGE_DIR, "README_deposit.md"), overwrite = TRUE)

info <- file_info(dir_ls(STAGE_DIR, type = "file", recurse = TRUE))
info$rel <- path_rel(info$path, STAGE_DIR)
info <- info[order(info$rel), ]

size_line <- function(rel, size) {
  sprintf("| `%s` | %s |", rel, format(size, scientific = FALSE))
}

section <- function(title, pred) {
  lines <- c("", paste("##", title), "", "| File | Size (bytes) |", "|---|---:|")
  hit <- info[pred, , drop = FALSE]
  for (i in seq_len(nrow(hit))) {
    lines <- c(lines, size_line(hit$rel[i], hit$size[i]))
  }
  lines
}

manifest_lines <- c(
  sprintf("# Deposit file manifest (v%s)", VERSION),
  "",
  "License: Creative Commons Attribution 4.0 International (CC BY 4.0).",
  "See README_deposit.md for descriptions and recommended joins.",
  section("Primary (province join set)", grepl("^primary/", info$rel)),
  section("Supporting", grepl("^supporting/", info$rel)),
  section("Meta", grepl("^meta/", info$rel)),
  section("Package docs", !grepl("^(primary|supporting|meta)/", info$rel))
)
writeLines(manifest_lines, file.path(STAGE_DIR, "FILES.md"))

if (file_exists(ZIP_PATH)) {
  file_delete(ZIP_PATH)
}
dir_create(path_dir(ZIP_PATH))
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

message(
  "Staged ", length(dir_ls(STAGE_DIR, recurse = TRUE, type = "file")),
  " files in ", STAGE_DIR
)
message("Wrote ", ZIP_PATH, " (", file_info(ZIP_PATH)$size, " bytes)")
