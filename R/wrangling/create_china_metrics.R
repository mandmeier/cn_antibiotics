# calculate data relative to China mean



carss <- read_csv("data/cleaned/carss_cleaned.csv")
zhang <- read_csv("data/cleaned/zhang_cleaned.csv")

province_groups <- read_csv("data/cleaned/province_groups.csv")

antibiotic_groups <- read_csv("data/cleaned/antibiotic_groups.csv")




# bacterial resistance relatice to China mean

chm_resistance <- carss %>%
  mutate(bacteria = paste(bacteria, "resistance")) %>%
  rename(metric = bacteria) %>%
  rename(value = resistant_pc) %>%
  relocate(antibiotic, .after = "province") %>%
  mutate(unit = "% resistant strains", .after = value)


env_concentration <- zhang %>%
  select(province, antibiotic, sample_type, median_concentration) %>%
  mutate(sample_type = paste(sample_type, "concentration")) %>%
  rename(metric = sample_type) %>%
  rename(value = median_concentration) %>%
  mutate(unit = "mg/kg", .after = value) %>%
  group_by(metric)



# find antibiotics for which we have both environmental and clinical data
common_antibiotics <- intersect(unique(carss$antibiotic), unique(zhang$antibiotic))


antibiotic_metrics_china <- chm_resistance %>%
  bind_rows(env_concentration) %>%

  # use only antibiotics for which we have both environmental and clinical data
  dplyr::filter(antibiotic %in% common_antibiotics) %>%

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
  left_join(province_groups, by = "province") %>%
  left_join(antibiotic_groups, by = "antibiotic")



write_csv(antibiotic_metrics_china, "data/analysis_ready/antibiotic_metrics_china.csv")









