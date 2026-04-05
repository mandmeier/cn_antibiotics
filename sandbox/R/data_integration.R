#integrate data


#### Zhang_2022 ####


zhang <- read_excel("antibiotics_data/clean_data/clean_data.xlsx", sheet = "Zhang_2022")


zhang_cleaned <- zhang %>%
  mutate(ABX_conc_mean = ifelse(grepl("ND", ABX_conc_mean), NA, ABX_conc_mean)) %>%
  mutate(ABX_conc_mean = ifelse(grepl("E-2", ABX_conc_mean), as.numeric(sub("E-.*$", "", ABX_conc_mean))/100, ABX_conc_mean)) %>%
  mutate(ABX_conc_mean = ifelse(grepl("E-3", ABX_conc_mean), as.numeric(sub("E-.*$", "", ABX_conc_mean))/1000, ABX_conc_mean)) %>%
  mutate(ABX_conc_mean = as.numeric(ABX_conc_mean)) %>%
  mutate(unit = "μg/kg") %>%
  mutate(
    ABX_subcat_clean = ABX_subcat %>%
      str_to_lower() %>%
      str_trim() %>%
      str_replace_all("–|-", "-") %>%   # normalize dashes
      str_replace_all("\\s+", " ") %>%
      str_remove_all(
        " hydrochloride| sodium salt| sodium| potassium salt| hyclate| mesylate| tosylate| dihydrate| phosphate"
      ) %>%
      str_remove_all(" h2o(\\-h2o)?") %>%   # removes h2o and h2o-h2o
      str_trim(),
    .after = ABX_subcat
  ) %>%
  mutate(
    abx = ifelse(is.na(ABX_subcat), ABX_cat, ABX_subcat) %>%
      str_to_lower() %>%
      str_trim()
  ) %>%

  # remove salts / water
  mutate(
    abx = str_remove_all(abx,
                         " hydrochloride| sodium salt| sodium| potassium salt| h2o(\\-h2o)?| dihydrate"
    )
  ) %>%

  # fix typos + normalize
  mutate(
    abx = recode(abx,
                 "enrythromycin" = "erythromycin",
                 "erythromycin-h2o" = "erythromycin",
                 "erythromycin-h2o-h2o" = "erythromycin",
                 "erythromycin a" = "erythromycin",
                 "erythromycin -" = "erythromycin",
                 "anhydroerythromycin" = "erythromycin",
                 "dehydrated erythromycin" = "erythromycin",
                 "dehydroerythromycin" = "erythromycin",

                 "chloromycetin" = "chloramphenicol",
                 "florophenicol" = "florfenicol",

                 "norfloxaxin" = "norfloxacin",
                 "norfloxacinfloxacin" = "norfloxacin",
                 "ofloxacinoxacin" = "ofloxacin",
                 "ciprofloxacinoxacin" = "ciprofloxacin",
                 "enrofloxacinoxacin" = "enrofloxacin",

                 "flerofloxacin" = "fleroxacin",
                 "perfloxacin" = "pefloxacin",
                 "danafloxacin" = "danofloxacin",

                 "sarmoxicillin" = "amoxicillin",
                 "pencillin" = "penicillin",

                 "sulfamethazole" = "sulfamethoxazole",
                 "sulfamethoxazol" = "sulfamethoxazole",
                 "sulphamethoxazole" = "sulfamethoxazole",

                 "sulfadimidin" = "sulfadimidine",
                 "sulfadimethazine" = "sulfadimidine",

                 "sulfamerazin" = "sulfamerazine",
                 "sulfapyridne" = "sulfapyridine",

                 "sulfachloryridazine" = "sulfachloropyridazine",
                 "sulfachlorpyridazine" = "sulfachloropyridazine",

                 "sulfachinoxalin" = "sulfachinoxaline",

                 "sulphisoxazole" = "sulfisoxazole",
                 "sulphamethazine" = "sulfamethazine",
                 "sulphadimethoxine" = "sulfadimethoxine"
    )
  ) %>%

  # collapse derivatives
  mutate(
    abx = recode(abx,
                 "n4-acetyl-sulfamethoxazole" = "sulfamethoxazole",
                 "acetyl sulfamethazine" = "sulfamethazine",
                 "β-apo-oxytetracycline" = "oxytetracycline",
                 "α-apo-oxytetracycline" = "oxytetracycline",
                 "epitetracycline" = "tetracycline",
                 "epioxytetracycline" = "oxytetracycline"
    )
  ) %>%
  mutate(
    abx = recode(abx,

                 # erythromycin cleanup
                 "roxithromycin-h2o" = "roxithromycin",
                 "erythromycin–h2o" = "erythromycin",
                 "anhydro erythromycin" = "erythromycin",
                 "anhydro-erythromycin" = "erythromycin",

                 # trimethoprim
                 "trimethoprimg" = "trimethoprim",

                 # pipemidic acid
                 "pipemidicacid" = "pipemidic acid",

                 # tetracycline issues
                 "tetracycliney" = "tetracycline",
                 "chlorotetracycline" = "chlortetracycline",
                 "chlortracycline" = "chlortetracycline",

                 # sulfonamide spelling
                 "sulphamethoxazole" = "sulfamethoxazole",
                 "sulfamethoxazol" = "sulfamethoxazole",

                 "sulphathiazole" = "sulfathiazole",
                 "sulphachloropyridazine" = "sulfachloropyridazine",
                 "sulphisoxazole" = "sulfisoxazole",
                 "sulfamethizol" = "sulfamethizole",

                 # other typos
                 "sulfanlamide" = "sulfanilamide",
                 "sulfisomidin" = "sulfisomidine"
    )
  ) %>%

  # remove salts
  mutate(
    abx = str_remove_all(abx, " tosylate| mesylate| phosphate| hyclate")
  ) %>%

  # drop non-antibiotics
  filter(!abx %in% c(
    "propranolol","metoprolol","atenolol","fluconazole","thiabendazole"
  )) %>%
  mutate(
    abx = recode(abx,
                 # --- Fix 1: typos / duplicates ---
                 "cefalexin" = "cephalexin",
                 "nallidixic acid" = "nalidixic acid",
                 "sparfoxacin" = "sparfloxacin",
                 "sulfametoxydiazine" = "sulfamethoxydiazine",
                 "sulfadoxin" = "sulfadoxine",
                 "sulfaphenazolum" = "sulfaphenazole",
                 "kitasamycinum" = "kitasamycin",

                 # --- Fix 2: standardization decisions ---
                 # ambiguous penicillin → map to penicillin g
                 "penicillin" = "penicillin g",

                 # tetracycline derivatives → parent compound
                 "isochlortetracycline" = "chlortetracycline",
                 "4-epichlortetracycline" = "chlortetracycline",
                 "anhydrochlortetracycline" = "chlortetracycline",
                 "epianhydrotetracycline" = "tetracycline"
    )
  ) %>%

  # emove non-antibiotics
  filter(!abx %in% c(
    "olaquindox", "nicarbazin", "monensin",
    "salinomycin", "narasin", "cyromazine"
  )) %>%

  # remove NA
  filter(!is.na(abx)) %>%
  mutate(
    abx = recode(abx,

                 # --- Remove class labels ---
                 "fluoroquinolones" = NA_character_,
                 "macrolides" = NA_character_,
                 "sulfonamides" = NA_character_,
                 "tetracyclines" = NA_character_,
                 "β-lactams" = NA_character_,
                 "other" = NA_character_,

                 # --- Synonym / canonical fixes ---
                 "aureomycin" = "chlortetracycline",
                 "leucomycin a3" = "leucomycin",
                 "sulfamethizole" = "sulfamethazole",
                 "sulfamethiazole" = "sulfamethazole",
                 "sulfaquirioxaline" = "sulfaquinoxaline"
    )
  ) %>%

  # optional: drop non-target antimicrobials
  filter(!abx %in% c(
    "furazolidone",
    "metronidazole",
    "ormetoprim"
  )) %>%

  # remove NA created above
  filter(!is.na(abx)) %>%
  mutate(
    abx = recode(abx,

                 # fix sulfonamide naming
                 "sulfamethazole" = "sulfamethizole",

                 # leucomycin normalization
                 "leucomycin a3" = "leucomycin",

                 # questionable naming correction
                 "danoxacin" = "danofloxacin"
    )
  ) %>%
  filter(!is.na(abx)) %>%
  mutate(
    abx = case_when(

      # optional sulfonamide harmonization layer
      abx == "sulfadimethoxine" ~ "sulfadimethoxine",
      abx == "sulfadimoxine" ~ "sulfadimethoxine",

      # keep sulfonamide subclasses but standard naming only
      TRUE ~ abx
    )
  ) %>%
  filter(!is.na(province)) %>%
  filter(!is.na(ABX_conc_mean))


write_csv(zhang_cleaned, "antibiotics_data/clean_data/zhang_cleaned.csv")



#### CARSS ####


# get CARSS data

carss <- read_csv("antibiotics_data/CARSS/drug_sensitivity.csv")


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


carss_cleaned <- carss %>%
  left_join(mapping_df) %>%
  filter(!is.na(abx)) %>%
  select(province, bacteria, abx, resistant_pc)


write_csv(carss_cleaned, "antibiotics_data/clean_data/carss_cleaned.csv")


#### Statistical Yearbook 2024 ####

SY <- read_csv("antibiotics_data/Statistical_Yearbook_China_2024.csv")

SY_cleaned <- SY %>%
  mutate(across(Total_Water_Resources_100_million_cu_m:last_col(), ~ as.numeric(.)))

write_csv(SY_cleaned, "antibiotics_data/clean_data/SY_cleaned.csv")



#### integrate by_province data

zhang_bp <- zhang_cleaned %>%
  # to get one representative measurement by province:
  # get latest available measurement by year and select highest measurement in that year
  group_by(province, sample_type, abx) %>%
  filter(sam_Y == max(sam_Y, na.rm = TRUE)) %>%
  filter(ABX_conc_mean == max(ABX_conc_mean, na.rm = TRUE)) %>%
  select(province, sample_type, abx, ABX_conc_mean) %>%
  unique() %>%
  # use only cases with at least 10 provinces measured
  group_by(sample_type, abx) %>%
  add_tally() %>%
  filter(n > 14) %>%
  select(-n) %>%
  ungroup() %>%
  # pivot wider
  mutate(sample_type_abx = paste0(sample_type, "_", abx, "_ug_kg"), .after = province) %>%
  select(-sample_type, -abx) %>%
  pivot_wider(names_from = sample_type_abx, values_from = ABX_conc_mean) %>%
  arrange(province)


carss_bp <- carss_cleaned %>%
  mutate(bacteria = str_replace(bacteria, ". ", "_")) %>%
  mutate(abx = str_replace(abx, " ", "_")) %>%
  # pivot wider
  mutate(bacteria_abx = paste0(bacteria, "_", abx, "_resistant_pc"), .after = province) %>%
  select(-bacteria, -abx) %>%
  pivot_wider(names_from = bacteria_abx, values_from = resistant_pc) %>%
  arrange(province)


SY_bp <- SY_cleaned %>%
  rename(province = Province) %>%
  mutate(province = ifelse(province == "Xizang", "Tibet", province))


## combine datasets

province_stats <- zhang_bp %>%
  full_join(carss_bp) %>%
  full_join(SY_bp)


write_csv(province_stats, "antibiotics_data/clean_data/province_stats.csv")
