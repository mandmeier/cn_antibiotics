# clean CARSS

carss_raw <- read_csv("data/raw/carss_drug_resistance_full.csv")

##### select relevant columns
carss_cleaned <- carss_raw %>%
  # select 2024 data
  filter(year == "2024") %>%
  # not including national data
  filter(province != "National") %>%
  # remove double drug treatments
  filter(!grepl("/", antibiotic)) %>%
  # replace zero values with lowest non-zero reported concentration for the antibiotic / 2
  group_by(antibiotic) %>%
  mutate(lod_est = {
    positives <- resistant_percent[resistant_percent > 0]
    if (length(positives) == 0) {
      NA_real_
    } else {
      min(positives, na.rm = TRUE)
    }
  }) %>%
  # replace zero values with LLOD/2
  mutate(
    resistant_pc = case_when(
      resistant_percent == 0 & !is.na(lod_est) ~ lod_est / 2,
      resistant_percent == 0 & is.na(lod_est)  ~ NA_real_,
      TRUE ~ resistant_percent
    )
  ) %>%
  # remove antibiotics that are always reported as zero
  filter(!is.na(lod_est)) %>%
  select(-lod_est) %>%
  ungroup() %>%
  select(province, bacteria = bacteria_species, antibiotic, resistant_pc)


readr::write_csv(carss_cleaned, "data/cleaned/carss_cleaned.csv")
