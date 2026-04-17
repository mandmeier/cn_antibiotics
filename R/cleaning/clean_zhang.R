# clean_zhang

#### Zhang_2022 ####


# Assumptions for cleaning this dataset
# 1) Checking data against curated list of antibiotics that are also available in CARSS dataset, using only measurements of antibiotics that are available in both datasets
# 2) using the highest (max) reported concentration of antibiotics for each province, antibiotic, and sample type,
# This generates one representative and recent value per province, assuming that environmental concentrations tend to increase over the years

# get standardized names of antibiotics
antibiotic_groups <- read_csv("data/cleaned/antibiotic_groups.csv")

zhang_raw <- readxl::read_excel("data/raw/Zhang_2022.xls", sheet = "Records")


##### select relevant columns
zhang <- zhang_raw %>%
  dplyr::select(sample_type = sem_type, province = loc_l1, antibiotic = ABX_subcat, mean_concentration = ABX_conc_mean, sam_Y, pub_id, pub_full) %>%
  filter(!is.na(sample_type)) %>%
  filter(!is.na(province)) %>%
  filter(!is.na(antibiotic)) %>%
  mutate(mean_concentration = as.numeric(mean_concentration)) %>%
  filter(!is.na(mean_concentration))


##### harmonize antibiotics spelling
# use curated list of antibiotics that are also available in CARSS dataset

# Ensure antibiotics vector is lowercase for consistent matching
abx_curated <- tolower(antibiotic_groups$antibiotic)

zhang <- zhang %>%
  # 1. Basic normalization & salt/water stripping
  mutate(
    abx = antibiotic %>%
      str_to_lower() %>%
      str_trim() %>%
      str_replace_all("–", "-") %>%
      str_squish() %>%
      # Remove common salts, counter-ions, and hydrates
      str_remove_all("\\s+(hydrochloride|sodium salt|sodium|potassium salt|hyclate|mesylate|tosylate|dihydrate|phosphate)\\b") %>%
      str_remove_all("-?h2o(-h2o)?") %>%
      str_trim()
  ) %>%

  # 2. Fix typos, brand names, and merge duplicates
  mutate(
    abx = recode(abx,
                 # Penicillins & Cephalosporins
                 "pencillin" = "penicillin G",
                 "sarmoxicillin" = "amoxicillin",
                 "cefalexin" = "cephalexin",

                 # Macrolides
                 "enrythromycin" = "erythromycin",
                 "anhydroerythromycin" = "erythromycin",
                 "anhydro-erythromycin" = "erythromycin",
                 "dehydrated erythromycin" = "erythromycin",
                 "dehydroerythromycin" = "erythromycin",
                 "erythromycin-h2o" = "erythromycin",
                 "erythromycin-h2o-h2o" = "erythromycin",
                 "erythromycin–h2o" = "erythromycin",
                 "anhydro erythromycin" = "erythromycin",
                 "erythromycin a" = "erythromycin",
                 "roxithromycin-h2o" = "roxithromycin",
                 "acetylspiramycin" = "acetylspiramycin",
                 "sPiramycin" = "spiramycin",
                 "kitasamycinum" = "kitasamycin",
                 "leucomycin" = "leucomycin",
                 "josamycin" = "josamycin",

                 # Fluoroquinolones & Quinolones
                 "norfloxaxin" = "norfloxacin",
                 "norfloxacinfloxacin" = "norfloxacin",
                 "ofloxacinoxacin" = "ofloxacin",
                 "ciprofloxacinoxacin" = "ciprofloxacin",
                 "enrofloxacinoxacin" = "enrofloxacin",
                 "flerofloxacin" = "fleroxacin",
                 "perfloxacin" = "pefloxacin",
                 "danafloxacin" = "danofloxacin",
                 "danoxacin" = "danofloxacin",
                 "sparfoxacin" = "sparfloxacin",
                 "nallidixic acid" = "nalidixic acid",
                 "pipemidicacid" = "pipemidic acid",

                 # Tetracyclines & derivatives
                 "chlorotetracycline" = "chlortetracycline",
                 "chlortracycline" = "chlortetracycline",
                 "aureomycin" = "chlortetracycline",
                 "isochlortetracycline" = "chlortetracycline",
                 "4-epichlortetracycline" = "chlortetracycline",
                 "anhydrochlortetracycline" = "chlortetracycline",
                 "epitetracycline" = "tetracycline",
                 "epianhydrotetracycline" = "tetracycline",
                 "tetracycliney" = "tetracycline",
                 "epioxytetracycline" = "oxytetracycline",
                 "α-apo-oxytetracycline" = "oxytetracycline",
                 "β-apo-oxytetracycline" = "oxytetracycline",

                 # Phenicols & Others
                 "chloromycetin" = "chloramphenicol",
                 "florophenicol" = "florfenicol",
                 "thiamphenicol" = "thiamphenicol",
                 "trimethoprimg" = "trimethoprim",
                 "lincomycin" = "lincomycin",
                 "clindamycin" = "clindamycin",
                 "gentamicin" = "gentamicin",
                 "streptomycin" = "streptomycin",
                 "spectinomycin" = "spectinomycin",
                 "vancomycin" = "vancomycin",
                 "tilmicosin" = "tilmicosin",
                 "tylosin" = "tylosin",
                 "tosufloxacin" = "tosufloxacin", # left after tosylate removal

                 # Known non-antibiotics / uncurated (will be dropped later)
                 "metronidazole" = NA_character_,
                 "furazolidone" = NA_character_,
                 "oleandomycin" = NA_character_,
                 "orbifloxacin" = NA_character_,
                 "ormetoprim" = NA_character_,
                 "oxacillin" = NA_character_,
                 "cefmetazole" = NA_character_,
                 "cefradine" = NA_character_
    )
  ) %>%

  # 3. Normalize Sulfonamides (UK/US spelling, acetyl derivatives, typos)
  mutate(
    abx = recode(abx,
                 # methoxazole family
                 "sulphamethoxazole" = "sulfamethoxazole",
                 "sulfamethoxazol" = "sulfamethoxazole",
                 "sulfamethazole" = "sulfamethoxazole",
                 "n4-acetyl-sulfamethoxazole" = "sulfamethoxazole",

                 # methazine family
                 "sulphamethazine" = "sulfamethazine",
                 "acetyl sulfamethazine" = "sulfamethazine",

                 # chloropyridazine family
                 "sulphachloropyridazine" = "sulfachloropyridazine",
                 "sulfachlorpyridazine" = "sulfachloropyridazine",
                 "sulfachloryridazine" = "sulfachloropyridazine",

                 # dimethoxine family
                 "sulphadimethoxine" = "sulfadimethoxine",
                 "sulfadimethoxypyrimidine" = "sulfadimethoxine",
                 "sulfadimoxine" = "sulfadimethoxine",

                 # dimidine family
                 "sulfadimidin" = "sulfadimidine",
                 "sulfadimethazine" = "sulfadimidine",

                 # thiazole family
                 "sulphathiazole" = "sulfathiazole",
                 "sulfamethiazole" = "sulfathiazole",

                 # Other sulfonamides
                 "sulphisoxazole" = "sulfisoxazole",
                 "sulfisomidin" = "sulfisomidine",
                 "sulfapyridne" = "sulfapyridine",
                 "sulfamerazin" = "sulfamerazine",
                 "sulfadoxin" = "sulfadoxine",
                 "sulfachinoxalin" = "sulfachinoxaline",
                 "sulfaquirioxaline" = "sulfaquinoxaline",
                 "sulfanlamide" = "sulfanilamide",
                 "sulfaphenazolum" = "sulfaphenazole",
                 "sulfametoxydiazine" = "sulfamethoxydiazine",

                 # Unifying chemically identical compounds, keeping INN/USAN standard names
                 "sulfadimidine" = "sulfamethazine",
                 "sulfadimethoxypyrimidine" = "sulfadimethoxine",
                 "sulfamethoxypyridazine" = "sulfameter"
    )
  ) %>%

  # 4. Drop confirmed non-antibiotics (beta-blockers, antifungals, ionophores, etc.)
  filter(!abx %in% c(
    "propranolol", "metoprolol", "atenolol", "fluconazole", "thiabendazole",
    "cyromazine", "monensin", "narasin", "nicarbazin", "salinomycin", "olaquindox"
  )) %>%

  # 5. Final safety net: keep ONLY exact matches to your curated codelist
  filter(abx %in% abx_curated, !is.na(abx)) %>%

  # 6. Clean up column names
  select(-antibiotic) %>%
  rename(antibiotic = abx) %>%
  relocate(antibiotic, .after = "province") %>%
  mutate(antibiotic = stringr::str_to_title(antibiotic))



#### Select highest measured value for each sample_type, province, antibiotic


zhang_cleaned <- zhang %>%
  group_by(sample_type, province, antibiotic) %>%
  # get highest reported concentration
  filter(mean_concentration == max(mean_concentration)) %>%
  add_tally() %>%
  # in case of tie use first publication with the reported value
  slice_min(pub_id, n = 1, with_ties = FALSE) %>%
  arrange(sample_type, province, antibiotic) %>%
  select(-n) %>%
  ungroup()



readr::write_csv(zhang_cleaned, "data/cleaned/zhang_cleaned.csv")



