# Clean CARSS resistance data for joins with environmental measurements.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
})
source("R/utils/antibiotic_classes.R")

# 31 provincial-level units in CARSS (excludes National aggregate).
CARSS_PROVINCES <- c(
  "Anhui", "Beijing", "Chongqing", "Fujian", "Gansu", "Guangdong", "Guangxi",
  "Guizhou", "Hainan", "Hebei", "Heilongjiang", "Henan", "Hubei", "Hunan",
  "Inner Mongolia", "Jiangsu", "Jiangxi", "Jilin", "Liaoning", "Ningxia",
  "Qinghai", "Shaanxi", "Shandong", "Shanghai", "Shanxi", "Sichuan",
  "Tianjin", "Tibet", "Xinjiang", "Yunnan", "Zhejiang"
)

CP_ALLOWED_COMBO_ANTIBIOTICS <- c("Trimethoprim/Sulfamethoxazole")

is_multiple_or_combo_antibiotic <- function(antibiotic) {
  if (is.na(antibiotic) || !nzchar(antibiotic)) {
    return(FALSE)
  }
  if (antibiotic %in% CP_ALLOWED_COMBO_ANTIBIOTICS) {
    return(FALSE)
  }
  grepl("/", antibiotic, fixed = TRUE) ||
    grepl("compound", antibiotic, ignore.case = TRUE) ||
    is_aggregate_antibiotic(antibiotic)
}

resistance <- read_csv("data/raw/resistance_data/carss_drug_resistance_full.csv")

n_before <- nrow(resistance)

resistance_clean <- resistance %>%
  select(-province_cn, -bacteria_species_cn, -drug_full_name_cn, -drug_code) %>%
  mutate(antibiotic = recode(antibiotic, "Polymixin B" = "Polymyxin B")) %>%
  filter(province %in% CARSS_PROVINCES) %>%
  filter(!vapply(antibiotic, is_multiple_or_combo_antibiotic, logical(1))) %>%
  mutate(
    antibiotic_class = assign_antibiotic_group(antibiotic),
    .after = antibiotic
  )

n_removed_province <- sum(!resistance$province %in% CARSS_PROVINCES)
n_removed_combo <- sum(
  resistance$province %in% CARSS_PROVINCES &
    vapply(resistance$antibiotic, is_multiple_or_combo_antibiotic, logical(1))
)
message(
  "Resistance rows: ", n_before, " -> ", nrow(resistance_clean),
  " (removed ", n_removed_province, " non-provincial; ",
  n_removed_combo, " combo/multiple antibiotic)"
)

write_csv(resistance_clean, "data/cleaned/resistance_clean.csv")
