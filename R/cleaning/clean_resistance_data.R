# Clean CARSS resistance data. Combo antibiotic names are kept as in the source panel.

resistance <- read_csv("data/raw/resistance_data/carss_drug_resistance_full.csv")

resistance_clean <- resistance %>%
  select(-province_cn, -bacteria_species_cn, -drug_full_name_cn) %>%
  mutate(
    antibiotic = recode(antibiotic, "Polymixin B" = "Polymyxin B")
  )

write_csv(resistance_clean, "data/cleaned/resistance_clean.csv")
