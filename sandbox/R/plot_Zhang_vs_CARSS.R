
# map all CARSS antibiotics to antibiotics listed in Zhang et al
mapping_df <- data.frame(
  abx = c(
    "amoxicillin",
    "ampicillin",
    "cefazolin",
    "cefotaxime",
    "ceftriaxone",
    "chloramphenicol",
    "clindamycin",
    "ciprofloxacin",
    "erythromycin",
    "gentamicin",
    "levofloxacin",
    "minocycline",
    "oxacillin",
    "penicillin g",
    "rifampicin",
    "tetracycline"
  ),
  CARSS_abx = c(
    "Amoxicillin",
    "Ampicillin",
    "Cefazolin",
    "Cefotaxime",
    "Ceftriaxone",
    "Chloramphenicol",
    "Clindamycin",
    "Ciprofloxacin",
    "Erythromycin",
    "Gentamicin",
    "Levofloxacin",
    "Minocyclin",
    "Oxacillin",
    "Penicillin G",
    "Rifampin",
    "Tetracycline"
  ),
  stringsAsFactors = FALSE
)


df_joined <- zhang_cleaned %>%
  left_join(mapping_df) %>%
  rename(Zhang_abx = abx)

# get CARSS data

carss <- read_csv("antibiotics_data/CARSS/drug_sensitivity.csv")

carss_cleaned <- carss %>%
  filter(CARSS_abx %in% mapping_df$CARSS_abx)


ABX_dat <- df_joined %>%
  select(province, Zhang_abx, sample_type, ABX_conc_mean, unit, CARSS_abx) %>%
  group_by(province, sample_type, Zhang_abx) %>%
  mutate(province_mean = mean(ABX_conc_mean), .after = ABX_conc_mean) %>%
  select(-ABX_conc_mean) %>%
  unique() %>%
  arrange(province, Zhang_abx, sample_type) %>%
  left_join(carss_cleaned) %>%
  ungroup() %>%
  filter(!is.na(province)) %>%
  filter(!is.na(CARSS_abx)) %>%
  filter(!is.na(bacteria)) %>%
  filter(province_mean > 0) %>%
  rename(ABX = CARSS_abx) %>%
  rename(mean_concentration = province_mean) %>%
  select(-Zhang_abx) %>%
  relocate(ABX, .after = province) %>%
  # add count
  group_by(bacteria, ABX) %>%
  add_tally() %>%
  filter(n > 10) %>%
  ungroup()




library(ggplot2)
library(ggpubr)

options(scipen=999)


ggplot(ABX_dat, aes(x = mean_concentration, y = resistant_pc)) +
  geom_point(aes(color = province, shape = sample_type), alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_grid(ABX ~ sample_type, scales = "free") +
  scale_x_log10() +
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top") +
  theme_minimal() +
  labs(
    x = "Mean Antibiotic Concentration in Environment [μg/kg]",
    y = "% Resistant Microbial Strains in Clinical Samples",
    color = "Province"
  )

# by antibiotic

ABX_dat <- ABX_dat %>%
  #filter(sample_type == "municipal sludge") %>%
  filter(mean_concentration > 0) %>%
  group_by(bacteria, ABX) %>%
  add_tally() %>%
  filter(n > 1) %>%
  ungroup()

ggplot(ABX_dat, aes(x = mean_concentration, y = resistant_pc)) +
  geom_point(aes(color = province, shape = sample_type), alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap(~ sample_type, scales = "free") +
  scale_x_log10() +
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top") +
  theme_minimal() +
  labs(
    x = "Mean Antibiotic Concentration in Environment [μg/kg]",
    y = "% Resistant Microbial Strains in Clinical Samples",
    color = "Province"
  )


### Erythromycin, Tetracycline in municipal sludge


plot_dat <- ABX_dat %>%
  #filter(sample_type == "municipal sludge") %>%
  filter(mean_concentration > 0) %>%
  filter(ABX %in% c("Erythromycin", "Tetracycline")) %>%
  filter(sample_type == "municipal sludge")

ggplot(plot_dat, aes(x = mean_concentration, y = resistant_pc)) +
  geom_point(aes(color = province, shape = sample_type), alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  facet_wrap( ~ ABX, scales = "free") +
  scale_x_log10() +
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(
    x = "Mean Antibiotic Concentration in Municipal Sludge [μg/kg]",
    y = "% Resistant Microbial Strains in Clinical Samples",
    color = "Province"
  )

