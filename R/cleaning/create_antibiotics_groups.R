# Create comprehensive antibiotic classification

antibiotic_groups <- data.frame(
  antibiotic = c(
    # Tetracyclines (7)
    "Chlortetracycline", "Demeclocycline", "Doxycycline", "Methacycline",
    "Minocycline", "Oxytetracycline", "Tetracycline",

    # Fluoroquinolones (22)
    "Cinoxacin", "Ciprofloxacin", "Danofloxacin", "Difloxacin", "Enoxacin",
    "Enrofloxacin", "Fleroxacin", "Gatifloxacin", "Lomefloxacin", "Mabofloxacin",
    "Marbofloxacin", "Moxifloxacin", "Nadifloxacin", "Nalidixic acid", "Norfloxacin",
    "Ofloxacin", "Oxolinic acid", "Pefloxacin", "Pipemidic acid", "Sarafloxacin",
    "Sparfloxacin", "Tosufloxacin",

    # Macrolides (9)
    "Acetylspiramycin", "Azithromycin", "Clarithromycin", "Erythromycin",
    "Josamycin", "Leucomycin", "Roxithromycin", "Spiramycin", "Tylosin",

    # β-Lactams (12)
    "Amoxicillin", "Ampicillin", "Cefazolin", "Cefotaxime", "Cefoxitin",
    "Ceftriaxone", "Cephalexin", "Cloxacillin", "Mecillinam", "Oxacillin",
    "Penicillin G", "Penicillin V",

    # Phenicols (3)
    "Chloramphenicol", "Florfenicol", "Thiamphenicol",

    # Aminoglycosides (3)
    "Gentamicin", "Spectinomycin", "Streptomycin",

    # Lincosamides (2)
    "Clindamycin", "Lincomycin",

    # Sulfonamides (24)
    "Sulfabenzamide", "Sulfacetamide", "Sulfachinoxaline", "Sulfachloropyridazine",
    "Sulfadiazine", "Sulfadimethoxine", "Sulfadoxine", "Sulfaguanidine",
    "Sulfamerazine", "Sulfameter", "Sulfamethazine", "Sulfamethizole",
    "Sulfamethoxazole", "Sulfamethoxydiazine", "Sulfamonomethoxine", "Sulfamoxole",
    "Sulfanilamide", "Sulfanitran", "Sulfaphenazole", "Sulfapyridine",
    "Sulfaquinoxaline", "Sulfathiazole", "Sulfisomidine", "Sulfisoxazole",

    # Glycopeptides (2)
    "Teicoplanin", "Vancomycin",

    # Oxazolidinones (1)
    "Linezolid",

    # Nitrofurans (1)
    "Nitrofurantoin",

    # Rifamycins (1)
    "Rifampicin",

    # Dihydrofolate reductase inhibitors (1)
    "Trimethoprim"
  ),

  antibiotic_group = c(
    rep("Tetracyclines", 7),
    rep("Fluoroquinolones", 22),
    rep("Macrolides", 9),
    rep("β-Lactams", 12),
    rep("Phenicols", 3),
    rep("Aminoglycosides", 3),
    rep("Lincosamides", 2),
    rep("Sulfonamides", 24),
    rep("Glycopeptides", 2),
    rep("Oxazolidinones", 1),
    rep("Nitrofurans", 1),
    rep("Rifamycins", 1),
    rep("Dihydrofolate reductase inhibitors", 1)
  ),

  primary_source = c(
    rep("Agriculture", 7),
    rep("Clinical + Agriculture", 22),
    rep("Clinical", 9),
    rep("Clinical", 12),
    rep("Agriculture", 3),
    rep("Clinical", 3),
    rep("Clinical + Agriculture", 2),
    rep("Clinical + Agriculture", 24),
    rep("Clinical", 2),
    rep("Clinical", 1),
    rep("Clinical", 1),
    rep("Clinical", 1),
    rep("Clinical + Agriculture", 1)
  ),

  environmental_signature = c(
    rep("High in soil & sediment; persistent; strong sorption", 7),
    rep("Moderate in water & sediment; mobile but persistent", 22),
    rep("Higher in urban sludge; low agricultural use", 9),
    rep("Low environmental persistence (degrade quickly); found mainly in wastewater", 12),
    rep("Detected in aquaculture sediments and livestock soils", 3),
    rep("Moderate persistence; found in sludge & hospital effluents", 3),
    rep("Moderate persistence; used for Gram-positive infections", 2),
    rep("Highly mobile in water; lower sorption; higher in surface water & groundwater", 24),
    rep("Very low environmental concentrations; last-resort antibiotics", 2),
    rep("Emerging contaminant; detected in hospital wastewater", 1),
    rep("Low persistence; primarily in hospital effluents", 1),
    rep("Low environmental detection; specialized use", 1),
    rep("Often paired with sulfonamides; moderate mobility", 1)
  ),

  stringsAsFactors = FALSE
)

# Verify total count
cat("Total antibiotics:", nrow(antibiotic_groups), "\n")
cat("Groups:", paste(unique(antibiotic_groups$antibiotic_group), collapse = ", "), "\n")

# Save to CSV
write_csv(antibiotic_groups, "data/cleaned/antibiotic_groups.csv")
