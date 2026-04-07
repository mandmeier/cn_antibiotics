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
  select(-n_strains)


readr::write_csv(carss_cleaned, "data/cleaned/carss_cleaned.csv")
