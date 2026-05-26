# calculate data relative to China mean



resistance <- read_csv("data/cleaned/resistance_clean.csv")

env_abx <- read_csv("data/analysis_ready/env_abx_per_province.csv")

province_groups <- read_csv("data/cleaned/province_groups.csv")

#antibiotic_groups <- read_csv("sandbox/manual_R/data/cleaned/antibiotic_groups.csv")




# bacterial resistance relative to China mean

chm_resistance <- resistance %>%
  #filter(year == "2024") %>%
  filter(province != "National") %>%
  mutate(bacteria_species = paste0(bacteria_species, "_resistance_", year)) %>%
  rename(metric = bacteria_species) %>%
  select(-year) %>%
  rename(value = resistant_percent) %>%
  relocate(antibiotic, .after = "province") %>%
  select(province, antibiotic, metric, value) %>%
  mutate(unit = "% resistant strains", .after = value)


env_concentration <- env_abx %>%
  select(province, antibiotic, sample_type, median_concentration, concentration_unit) %>%
  mutate(sample_type = paste0(sample_type, "_concentration")) %>%
  rename(metric = sample_type) %>%
  rename(value = median_concentration) %>%
  mutate(unit = concentration_unit, .after = value) %>%
  select(-concentration_unit) %>%
  group_by(metric)



# find antibiotics for which we have both environmental and clinical data
common_antibiotics <- intersect(unique(resistance$antibiotic), unique(env_abx$antibiotic))


antibiotic_metrics_china <- chm_resistance %>%
  bind_rows(env_concentration) %>%

  # use only antibiotics for which we have both environmental and clinical data
  # dplyr::filter(antibiotic %in% common_antibiotics) %>%

  # use only values > 0
  filter(value > 0) %>%

  # retain only combinations of metric and antibiotic for which we have data in at least 10 provinces
  group_by(metric, antibiotic) %>%
  add_tally(name = "n_provinces") %>%
  filter(n_provinces >= 10) %>%
  select(-n_provinces) %>%

  # create derived variables for each metric and antibiotic
  mutate(
    china_mean = mean(value),
    fold_change = value / mean(value),
    log2_fc = log2(fold_change)
  ) %>%

  # add province and antibiotic groups metadata
  left_join(province_groups, by = "province") #%>%
  #left_join(antibiotic_groups, by = "antibiotic")



write_csv(antibiotic_metrics_china, "data/analysis_ready/antibiotic_metrics_china.csv")









