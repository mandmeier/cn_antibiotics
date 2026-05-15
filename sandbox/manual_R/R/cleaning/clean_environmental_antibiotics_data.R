

env_abx <- read_csv("data/raw/environmental_antibiotics_data_manual.csv")

#### Harmonized Dataset showing ALL measurements (including location specific for hotspot analysis)
env_abx_harmonized <- env_abx %>%
  select(-`...15`, -`...16`) %>%
  # harmonize units (recalculate values where necessary)
  #
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

      "Perfloxacin" = "Pefloxacin",

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
  #filter(!grepl("Total ", antibiotic_clean)) %>
  relocate(antibiotic_clean, .after = antibiotic) %>%
  select(-antibiotic) %>%
  rename(antibiotic = antibiotic_clean)




write_csv(env_abx_harmonized, "data/cleaned/env_abx_harmonized.csv")




