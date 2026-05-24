# Harmonize antibiotic names in environmental measurements.
# Cross-dataset joins with resistance use ~15 shared drug names; resistance combos stay separate.

source("R/utils/environmental_units.R")

environmental <- read_csv("data/raw/environmental_data/environmental_data_combined.csv")

n_antibiotics_before <- n_distinct(environmental$antibiotic)

environmental_clean <- environmental %>%
  # Removed because it is not clear if Zhi Su used Penicillin G or Penicillin V.
  # These are chemically distinct.
  filter(!(source_dataset == "Su_2025_Table2_PDF_text" & antibiotic == "Penicillin (PEN)")) %>%
  mutate(
    antibiotic = iconv(antibiotic, from = "", to = "UTF-8", sub = ""),
    antibiotic = str_squish(antibiotic),

    antibiotic = case_when(
      antibiotic %in% c("NOR (summer)", "NOR (winter)", "NOR") ~ "Norfloxacin",
      antibiotic %in% c("OFX (summer)", "OFX (winter)", "OFX") ~ "Ofloxacin",
      antibiotic == "ROX" ~ "Roxithromycin",
      antibiotic == "SMX" ~ "Sulfamethoxazole",
      TRUE ~ antibiotic
    ),

    antibiotic = str_remove(
      antibiotic,
      regex(" (?i)(hydrochloride|hyclate|mesylate|tosylate|phosphate|sodium salt)$")
    ),

    antibiotic = case_when(
      str_detect(antibiotic, regex("Erythromycin.*H2O", ignore_case = TRUE)) ~ "Erythromycin-H2O",
      str_detect(antibiotic, regex("apo[- ]?oxytetracycline", ignore_case = TRUE)) ~ "Apo-Oxytetracycline",
      TRUE ~ antibiotic
    ),

    antibiotic = recode(
      antibiotic,

      # --- Apo-oxytetracycline ---
      "_-apo-oxytetracycline" = "Apo-Oxytetracycline",
      "_-apo-Oxytetracycline" = "Apo-Oxytetracycline",
      "α-apo-oxytetracycline" = "Apo-Oxytetracycline",
      "α-apo-Oxytetracycline" = "Apo-Oxytetracycline",
      "β-apo-oxytetracycline" = "Apo-Oxytetracycline",
      "β-apo-Oxytetracycline" = "Apo-Oxytetracycline",

      # --- Dehydroerythromycin (metabolite; keep separate) ---
      "Anhydro-erythromycin" = "Dehydroerythromycin",
      "Anhydroerythromycin" = "Dehydroerythromycin",
      "Dehydrated erythromycin" = "Dehydroerythromycin",
      "Dehydroerythromycin" = "Dehydroerythromycin",
      "anhydroerythromycin" = "Dehydroerythromycin",

      # --- Erythromycin ---
      "Enrythromycin" = "Erythromycin",
      "Erythromycin-H2O-H2O" = "Erythromycin A dihydrate",
      "Erythromycin-H_O (ETM-H_O)" = "Erythromycin-H2O",
      "erythromycin-H2O" = "Erythromycin-H2O",
      "ErythromycinH2O" = "Erythromycin-H2O",
      "Erythromycin–H2O" = "Erythromycin-H2O",

      # --- Chlortetracycline ---
      "Chlorotetracycline" = "Chlortetracycline",
      "Chlortetracycline" = "Chlortetracycline",
      "Chlortracycline" = "Chlortetracycline",
      "Aureomycin" = "Chlortetracycline",

      # --- Cephalosporins / penicillins ---
      "Cefalexin" = "Cephalexin",
      "Cephalexin" = "Cephalexin",
      "Sarmoxicillin" = "Amoxicillin",

      # --- Phenicols ---
      "Chloromycetin" = "Chloramphenicol",
      "Florophenicol" = "Florfenicol",

      # --- Fluoroquinolones ---
      "Ciprofloxacinoxacin" = "Ciprofloxacin",
      "Danafloxacin" = "Danofloxacin",
      "Danofloxaxin" = "Danofloxacin",
      "Danoxacin" = "Danofloxacin",
      "Enrofloxacinoxacin" = "Enrofloxacin",
      "LevOfloxacin" = "Levofloxacin",
      "Mabofloxacin" = "Marbofloxacin",
      "Norfloxacinfloxacin" = "Norfloxacin",
      "Ofloxacinoxacin" = "Ofloxacin",
      "Perfloxacin" = "Pefloxacin",
      "Nallidixic Acid" = "Nalidixic Acid",
      "Nalidixic acid" = "Nalidixic Acid",
      "Oxolinic acid" = "Oxolinic Acid",
      "Sparfoxacin" = "Sparfloxacin",
      "Tosufloxacin" = "Tosufloxacin",

      # --- Pipemidic acid ---
      "Pipemidic acid" = "Pipemidic Acid",

      # --- Macrolides ---
      "SPiramycin" = "Spiramycin",
      "Lincomysin" = "Lincomycin",
      "Tylosine" = "Tylosin",

      # --- Sulfonamides ---
      "Sulfachlorpyridazine" = "Sulfachloropyridazine",
      "Sulfachloryridazine" = "Sulfachloropyridazine",
      "Sulphachloropyridazine" = "Sulfachloropyridazine",
      "Sulfachinoxalin" = "Sulfachinoxaline",
      "Sulfadimidin" = "Sulfadimidine",
      "Sulfadoxin" = "Sulfadoxine",
      "Sulfamerazin" = "Sulfamerazine",
      "sulfamethazine" = "Sulfadimidine",
      "Sulphamethazine" = "Sulfadimidine",
      "Sulfamethazine" = "Sulfadimidine",
      "Sulfamethizol" = "Sulfamethizole",
      "Sulfamethoxazol" = "Sulfamethoxazole",
      "Sulphamethoxazole" = "Sulfamethoxazole",
      "Sulfametoxydiazine" = "Sulfamethoxydiazine",
      "Sulfaphenazolum" = "Sulfaphenazole",
      "Sulfapyridne" = "Sulfapyridine",
      "sulfapyridine" = "Sulfapyridine",
      "Sulfaquirioxaline" = "Sulfaquinoxaline",
      "Sulphadimethoxine" = "Sulfadimethoxine",
      "Sulphisoxazole" = "Sulfafurazole",
      "Sulfisoxazole" = "Sulfafurazole",
      "Sulfamethazole" = "Sulfamethizole",
      "Acetyl sulfamethazine" = "Acetylsulfamethazine",
      "Acetyl Sulfamethazine" = "Acetylsulfamethazine",
      "Acetyl-sulfamethoxazole" = "Acetylsulfamethoxazole",
      "Acetyl-Sulfamethoxazole" = "Acetylsulfamethoxazole",

      # --- Tetracyclines ---
      "TetracyclineY" = "Tetracycline",
      "Metacycline" = "Methacycline",

      # --- Aggregates (collapse Sum/Total to base class name) ---
      "Total antibiotics" = "Total Antibiotics",
      "Total quinolones" = "Total Quinolones",
      "All antibiotics (median)" = "All Antibiotics",
      "Mixed antibiotics" = "Multiple Antibiotics",
      "Mixed Antibiotics" = "Multiple Antibiotics",
      "Multiple antibiotics (aggregate)" = "Multiple Antibiotics",
      "Fluoroquinolones (sum)" = "Fluoroquinolones",
      "Fluoroquinolones (total)" = "Fluoroquinolones",
      "Macrolides (sum)" = "Macrolides",
      "Macrolides (total)" = "Macrolides",
      "Macrolides (sum, dominant class in sediment)" = "Macrolides",
      "Lincosamides (sum)" = "Lincosamides",
      "Sulfonamides (sum)" = "Sulfonamides",
      "Sulfonamides + aminophenyl sulfone compounds (cumulative)" = "Sulfonamides",
      "Tetracyclines (sum)" = "Tetracycline",
      "Tetracyclines (total)" = "Tetracycline",
      "Tetracyclines (dominant class)" = "Tetracycline",
      "Tetracyclines" = "Tetracycline",
      "Total antibiotics (sum)" = "Total Antibiotics",
      "Total antibiotics (QNs+SAs)" = "Total Antibiotics",
      "Total antibiotics (sum, macrolides dominant)" = "Total Antibiotics",
      "Total antibiotics (sum, tetracyclines dominant)" = "Total Antibiotics",
      "Total Chloramphenicols (CAs)" = "Total Chloramphenicols",
      "Total fishery drugs (antibiotics + pesticides)" = "Total Fishery Drugs",
      "Total Macrolides (MLs)" = "Macrolides",
      "Total Quinolones (QNs)" = "Total Quinolones",
      "Total quinolones (sum)" = "Total Quinolones",
      "Total Sulfonamides (SAs)" = "Sulfonamides",
      "Total Tetracyclines (TCs)" = "Tetracycline",
      "Total Tetracyclines" = "Tetracycline",

      .default = antibiotic
    ),

    # Catch remaining class-level (Sum)/(Total) variants
    antibiotic = str_replace(antibiotic, " \\((Sum|Total)\\)$", ""),
    antibiotic = str_replace(antibiotic, " \\((Sum|Total), [^)]+\\)$", ""),
    antibiotic = str_replace(antibiotic, " \\(Dominant Class\\)$", ""),
    antibiotic = str_replace(antibiotic, " \\(Dominant Class in Sediment\\)$", ""),
    antibiotic = str_replace(antibiotic, " \\(Median\\)$", ""),

    # Remove trailing abbreviated codes in parentheses (e.g. CIP, TCs, ETM-H2O)
    antibiotic = str_remove(antibiotic, " \\([A-Z][A-Z0-9+-]{1,15}\\)$"),

    # Synonyms revealed after abbreviation stripping
    antibiotic = recode(
      antibiotic,
      "Sulfamethazine" = "Sulfadimidine",
      "Sulfisoxazole" = "Sulfafurazole",
      .default = antibiotic
    )
  ) %>%
  mutate(
    antibiotic = if_else(
      source_dataset == "Wu_2025.csv" & antibiotic == "Penicillin",
      "Penicillin G",
      antibiotic
    )
  ) %>%
  filter(!antibiotic %in% c(
    "Atenolol",
    "Metoprolol",
    "Propranolol",
    "Fluconazole",
    "Pimaricin",
    "Monensin",
    "Narasin",
    "Nicarbazin"
  )) %>%
  # Removed solid waste: different antibiotics concentration pattern from other
  # matrices and only n = 2 samples.
  filter(sample_type != "solid waste") %>%
  mutate(
    matrix = assign_environmental_matrix(sample_type, reference_number),
    .after = sample_type
  ) %>%
  # unclear how to convert measurments measured in liquid sample to solid sludge,
  # removed 7 measurements from sources N69, N92, N451
  filter(
    !(matrix == "sludge" & concentration_unit %in% c("mg/L", "µg/L"))
  ) %>%
  # unclear how to convert measurments measured in solid sample to liquid surface water,
  # removed 5 measurements from sources N453 and N459
  filter(
    !(matrix == "surface water" & concentration_unit %in% c("mg/kg", "ng/kg"))
  ) %>%
  # unclear how to convert measurments measured in solid sample to liquid surface water,
  # removed 21 measurements from sources N154, N453, N455, and N457
  filter(
    !(
      matrix == "surface water" &
        grepl("^ng/g", str_to_lower(str_replace_all(concentration_unit, "μ", "µ")))
    )
  ) %>%
  # normalize units
  mutate(
    .unit_norm = normalize_unit_string(concentration_unit),
    .is_dw = unit_is_dry_weight(.unit_norm),
    .unit_factor = unit_conversion_factor(concentration_unit),
    mean_concentration = mean_concentration * .unit_factor,
    max_concentration = max_concentration * .unit_factor,
    concentration_unit = target_concentration_unit(matrix, .is_dw)
  ) %>%
  select(-.unit_norm, -.is_dw, -.unit_factor)

if (any(is.na(environmental_clean$concentration_unit))) {
  bad_units <- unique(environmental_clean$concentration_unit[
    is.na(environmental_clean$concentration_unit)
  ])
  stop(
    "Unconverted concentration_unit: ",
    paste(bad_units, collapse = ", ")
  )
}

n_antibiotics_after <- n_distinct(environmental_clean$antibiotic)
message(
  "Antibiotics: ", n_antibiotics_before, " -> ", n_antibiotics_after
)
if (any(is.na(environmental_clean$matrix))) {
  warning(
    "Unmapped sample_type: ",
    paste(unique(environmental_clean$sample_type[is.na(environmental_clean$matrix)]), collapse = ", ")
  )
}

write_csv(environmental_clean, "data/cleaned/environmental_cleaned.csv")
