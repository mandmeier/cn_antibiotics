# clean CARSS


carss_raw <- readxl::read_excel("data/raw/CARSS.xlsx", sheet = "drug_sensitivity")


##### select relevant columns
carss_cleaned <- carss_raw %>%
  rename(antibiotic = CARSS_abx) %>%
  # rename Rifampin to Rifampicin
  mutate(antibiotic = recode(antibiotic, "Rifampin" = "Rifampicin")) %>%
  # remove double drug treatments
  filter(!grepl("/", antibiotic)) %>%
  # not considering number of strains
  select(-n_strains) %>%
  # replace zero values with lowest non-zero reported concentration for the antibiotic / 2
  group_by(antibiotic) %>%
  mutate(lod_est = {
    positives <- resistant_pc[resistant_pc > 0]
    if (length(positives) == 0) {
      NA_real_
    } else {
      min(positives, na.rm = TRUE)
    }
  }) %>%
  # replace zero values with LLOD/2
  mutate(
    resistant_pc = case_when(
      resistant_pc == 0 & !is.na(lod_est) ~ lod_est / 2,
      resistant_pc == 0 & is.na(lod_est)  ~ NA_real_,
      TRUE ~ resistant_pc
    )
  ) %>%
  # remove antibiotics that are always reported as zero
  filter(!is.na(lod_est)) %>%
  select(-lod_est) %>%
  ungroup()


readr::write_csv(carss_cleaned, "data/cleaned/carss_cleaned.csv")
