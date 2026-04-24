library(tidyr)

# conbine data from Zhang soil antibiotics and yearbook summary statistics
# in wide format. rows:province, columns: statistics and antibiotic concentrations
# province_data_wide


# import clean data
carss <- read_csv("data/cleaned/carss_cleaned.csv")
zhang <- read_csv("data/cleaned/zhang_cleaned.csv")
yearbook <- read_csv("data/cleaned/yearbook_cleaned.csv")


#### integrate by_province data

zhang_bp <- zhang %>%
  # use only cases with at least 3 provinces measured
  group_by(sample_type, antibiotic) %>%
  add_tally() %>%
  filter(n > 3) %>%
  select(-n) %>%
  ungroup() %>%
  select(province, sample_type, antibiotic, mean_concentration) %>%
  # pivot wider
  mutate(sample_type_antibiotic = paste0(sample_type, "_", antibiotic, "_ug_kg"), .after = province) %>%
  select(-sample_type, -antibiotic) %>%
  pivot_wider(names_from = sample_type_antibiotic, values_from = mean_concentration) %>%
  arrange(province) %>%
  # remove whitespace in column names
  rename_with(~ gsub("\\s+", "_", .))


carss_bp <- carss %>%
  mutate(bacteria = str_replace(bacteria, ". ", "_")) %>%
  mutate(antibiotic = str_replace(antibiotic, " ", "_")) %>%
  # pivot wider
  mutate(bacteria_antibiotic = paste0(bacteria, "_", antibiotic, "_resistant_pc"), .after = province) %>%
  select(-bacteria, -antibiotic) %>%
  pivot_wider(names_from = bacteria_antibiotic, values_from = resistant_pc) %>%
  arrange(province)

#
# yearbook_bp <- yearbook %>%
#   rename(province = Province) %>%
#   mutate(province = ifelse(province == "Xizang", "Tibet", province))


## combine datasets

province_data_wide <- zhang_bp %>%
  full_join(carss_bp) %>%
  full_join(yearbook)



write_csv(province_data_wide, "data/analysis_ready/province_data_wide.csv")




