

env_abx <- read_csv("data/raw/environmental_antibiotics_data_manual.csv")

#### Harmonized Dataset show ing ALL measurements (including location specific for hotspot analysis)
env_abx_harmonized <- env_abx %>%
  select(-`...15`, -`...16`) %>%
  # harmonize units
  mutate(concentration_unit = ifelse(grepl("water", sample_type), "ng/L", "μg/kg dw")) %>%
  # filter missing antibiotics
  filter(!is.na(antibiotic)) %>%
  # remove municipal wastewater effluent measurements (only 4, and not interesting, not a contaminated matrix)
  filter(sample_type != "municipal wastewater effluent") %>%

  # fix antibiotic spellings
  # Fix Erythromycin badly encoded bytes
  mutate(
    antibiotic = iconv(antibiotic, from = "", to = "UTF-8", sub = ""),  # drop bad bytes

    antibiotic_clean = case_when(
      str_detect(antibiotic, "Erythromycin.*H2O") ~ "Erythromycin-H2O",
      TRUE ~ antibiotic
    )
  ) %>%
  mutate(
    antibiotic_clean = recode(
      antibiotic,

      # --- Oxytetracycline ---
      "_-apo-oxytetracycline" = "Apo-Oxytetracycline",
      "_-apo-Oxytetracycline" = "Apo-Oxytetracycline",

      # --- Dehydroerythromycin group ---
      "Anhydro-erythromycin" = "Dehydroerythromycin",
      "Anhydroerythromycin" = "Dehydroerythromycin",
      "Dehydrated erythromycin" = "Dehydroerythromycin",
      "Dehydroerythromycin" = "Dehydroerythromycin",
      "anhydroerythromycin" = "Dehydroerythromycin",

      # --- Erythromycin typo ---
      "Enrythromycin" = "Erythromycin",

      # --- Erythromycin hydrate variants ---
      "Erythromycin-H2O-H2O" = "Erythromycin A dihydrate",
      "Erythromycin-H_O (ETM-H_O)" = "Erythromycin-H2O",
      "erythromycin-H2O" = "Erythromycin-H2O",
      "ErythromycinH2O" = "Erythromycin-H2O",

      # --- Chlorotetracycline ---
      "Chlorotetracycline" = "Chlorotetracycline",
      "Chlortetracycline" = "Chlorotetracycline",
      "Chlortracycline" = "Chlorotetracycline",

      # --- Cephalexin ---
      "Cefalexin" = "Cephalexin",
      "Cephalexin" = "Cephalexin",

      # --- Fluoroquinolones ---
      "Ciprofloxacinoxacin" = "Ciprofloxacin",
      "Ciprofloxacin" = "Ciprofloxacin",

      "Danafloxacin" = "Danofloxacin",
      "Danofloxaxin" = "Danofloxacin",

      "Enrofloxacinoxacin" = "Enrofloxacin",
      "Enrofloxacin" = "Enrofloxacin",

      "LevOfloxacin" = "Levofloxacin",
      "Levofloxacin" = "Levofloxacin",

      "Mabofloxacin" = "Marbofloxacin",
      "Marbofloxacin" = "Marbofloxacin",

      "Norfloxacinfloxacin" = "Norfloxacin",
      "Norfloxacin" = "Norfloxacin",

      "Ofloxacinoxacin" = "Ofloxacin",
      "Ofloxacin" = "Ofloxacin",

      # --- Pipemidic acid ---
      "Pipemidic acid" = "Pipemidic Acid",

      # --- Spiramycin ---
      "SPiramycin" = "Spiramycin",

      # --- Sulfonamides ---
      "Sulfachlorpyridazine" = "Sulfachloropyridazine",
      "Sulfachloryridazine" = "Sulfachloropyridazine",

      "Sulfadimidin" = "Sulfadimidine",
      "Sulfadoxin" ="Sulfadoxine",
      "Sulfamerazin" = "Sulfamerazine",
      "sulfamethazine" = "Sulfamethazine",
      "Sulfamethizol" = "Sulfamethizole",
      "Sulfamethoxazol" = "Sulfamethoxazole",
      "Sulfametoxydiazine" = "Sulfamethoxydiazine",
      "Sulfaphenazolum" = "Sulfaphenazole",
      "Sulfapyridne" = "Sulfapyridine",
      "Sulfaquirioxaline" = "Sulfaquinoxaline",

      # --- Tetracycline typo ---
      "TetracyclineY" = "Tetracycline",

      # --- Total Antibiotics ---
      "Total antibiotics" = "Total Antibiotics",

      # --- lowercase fixes ---
      "sulfamethazine" = "Sulfamethazine",
      "sulfapyridine" = "Sulfapyridine",

      .default = antibiotic
    )
  ) %>%


  ### remove summarized measurements (Total Antibiotics etc)
  #filter(!grepl("Total ", antibiotic_clean)) %>%
  relocate(antibiotic_clean, .after = antibiotic) %>%
  select(-antibiotic) %>%
  rename(antibiotic = antibiotic_clean)




write_csv(env_abx_harmonized, "data/cleaned/env_abx_harmonized.csv")



#### Create ONE value per specimen_type, province, antibiotic
#### Select median measured value for each sample_type, province, antibiotic
# goal is to get ONE representative measurement per sample_type, antibiotic and province
# median does not exclude data, and as opposed to using mean or max, we are not susceptible to one-off high measurements from heavily polluted sites

env_abx_per_province <- env_abx_harmonized %>%

  # remove measurements with no mean concentration
  filter(!is.na(mean_concentration)) %>%

  select(sample_type, province, location, antibiotic, mean_concentration, concentration_unit, sample_year, season, full_reference_name, reference_number) %>%
  unique() %>%
  group_by(sample_type, province, antibiotic) %>%

  # get median reported concentration for all specimens taken in the province
  # need to treat zero values in a defensible way. Likely not true zeroes, but below limit of detection.
  # estimating LOD as "lowest measured value among all measurements in a group / 2"
  ## in cases where NO values are reported > 0 we assume a real NA value (no data measured). These cases are removed
  mutate(
    lod_est = {
      positives <- mean_concentration[mean_concentration > 0]

      if (length(positives) == 0) {
        NA_real_
      } else {
        min(positives, na.rm = TRUE)
      }
    }
  ) %>%
  # replace with LLOD/2 if reported as zero
  mutate(
    mean_concentration = case_when(
      mean_concentration == 0 & !is.na(lod_est) ~ lod_est/2,
      mean_concentration == 0 & is.na(lod_est)  ~ NA_real_,
      TRUE ~ mean_concentration
    )
  ) %>%
  # calculate median
  mutate(median_concentration = median(mean_concentration)) %>%
  select(-mean_concentration) %>%
  # remove true NA measurements (nothing measured for this antibiotic in this province)
  filter(!is.na(median_concentration)) %>%
  unique() %>%
  add_tally() %>%
  # in case of tie use first publication with the reported value
  slice_min(reference_number, n = 1, with_ties = FALSE) %>%
  arrange(sample_type, province, antibiotic) %>%
  select(-n) %>%
  relocate(median_concentration, .after = "antibiotic") %>%
  ungroup() %>%
  select(-lod_est) %>%
  # filter missing antibiotics
  filter(!is.na(antibiotic))

write_csv(env_abx_per_province, "data/analysis_ready//env_abx_per_province.csv")
