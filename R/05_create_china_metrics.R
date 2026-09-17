# calculate data relative to China mean

source("R/utils/reproducible_csv.R")

resistance <- read_csv("data/intermediate/resistance_clean.csv")

env_abx <- read_csv("data/output/env_abx_per_province.csv")

province_metadata <- read_csv("data/raw/reference/province_metadata.csv")




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
  left_join(province_metadata, by = "province") #%>%
  #left_join(antibiotic_groups, by = "antibiotic")



write_csv_reproducible(
  antibiotic_metrics_china,
  "data/output/antibiotic_metrics_china.csv",
  # Cluster labels are attached by R/06; do not clobber them on re-run.
  ignore_cols = c("k2_groups", "k3_groups", "k4_groups")
)









