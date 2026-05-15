# Create comprehensive antibiotic classification

antibiotic_groups <- data.frame(
  antibiotic = c(
    # Tetracyclines (7)
    "Chlortetracycline", "Demeclocycline", "Doxycycline", "Methacycline",
    "Minocycline", "Oxytetracycline", "Tetracycline",

    # Quinolones (25) - includes both fluorinated and non-fluorinated
    "Cinoxacin", "Ciprofloxacin", "Danofloxacin", "Difloxacin", "Enoxacin",
    "Enrofloxacin", "Fleroxacin", "Flumequine", "Gatifloxacin", "Lomefloxacin",
    "Mabofloxacin", "Marbofloxacin", "Moxifloxacin", "Nadifloxacin", "Nalidixic Acid",
    "Norfloxacin", "Ofloxacin", "Oxolinic Acid", "Pefloxacin", "Pipemidic Acid",
    "Sarafloxacin", "Sparfloxacin", "Tosufloxacin", "Levofloxacin", "Orbifloxacin",

    # Macrolides (10)
    "Acetylspiramycin", "Azithromycin", "Clarithromycin", "Erythromycin",
    "Josamycin", "Leucomycin", "Roxithromycin", "Spiramycin", "Tylosin", "Tilmicosin",

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
    # Tetracyclines (7)
    rep("Tetracyclines", 7),

    # Quinolones (25) - unified group
    rep("Quinolones", 25),

    # Macrolides (10)
    rep("Macrolides", 10),

    # β-Lactams (12)
    rep("β-Lactams", 12),

    # Phenicols (3)
    rep("Phenicols", 3),

    # Aminoglycosides (3)
    rep("Aminoglycosides", 3),

    # Lincosamides (2)
    rep("Lincosamides", 2),

    # Sulfonamides (24)
    rep("Sulfonamides", 24),

    # Glycopeptides (2)
    rep("Glycopeptides", 2),

    # Oxazolidinones (1)
    rep("Oxazolidinones", 1),

    # Nitrofurans (1)
    rep("Nitrofurans", 1),

    # Rifamycins (1)
    rep("Rifamycins", 1),

    # Dihydrofolate reductase inhibitors (1)
    rep("Dihydrofolate reductase inhibitors", 1)
  ),

  primary_source = c(
    # Tetracyclines - Agriculture
    rep("Agriculture", 7),

    # Quinolones - Dual use
    rep("Clinical + Agriculture", 25),

    # Macrolides - Mostly Clinical (Tylosin/Tilmicosin are veterinary)
    rep("Clinical", 8), "Agriculture", "Agriculture",

    # β-Lactams - Clinical
    rep("Clinical", 12),

    # Phenicols - Agriculture
    rep("Agriculture", 3),

    # Aminoglycosides - Clinical
    rep("Clinical", 3),

    # Lincosamides - Dual
    rep("Clinical + Agriculture", 2),

    # Sulfonamides - Dual
    rep("Clinical + Agriculture", 24),

    # Glycopeptides - Clinical
    rep("Clinical", 2),

    # Oxazolidinones - Clinical
    rep("Clinical", 1),

    # Nitrofurans - Clinical
    rep("Clinical", 1),

    # Rifamycins - Clinical
    rep("Clinical", 1),

    # Dihydrofolate reductase inhibitors - Dual
    rep("Clinical + Agriculture", 1)
  ),

  environmental_signature = c(
    # Tetracyclines
    rep("High in soil & sediment; persistent; strong sorption", 7),

    # Quinolones
    rep("Moderate in water & sediment; mobile but persistent", 25),

    # Macrolides
    rep("Higher in urban sludge; low agricultural use", 8),
    "Veterinary macrolide; detected in livestock soils",
    "Veterinary macrolide; detected in livestock soils",

    # β-Lactams
    rep("Low environmental persistence (degrade quickly); found mainly in wastewater", 12),

    # Phenicols
    rep("Detected in aquaculture sediments and livestock soils", 3),

    # Aminoglycosides
    rep("Moderate persistence; found in sludge & hospital effluents", 3),

    # Lincosamides
    rep("Moderate persistence; used for Gram-positive infections", 2),

    # Sulfonamides
    rep("Highly mobile in water; lower sorption; higher in surface water & groundwater", 24),

    # Glycopeptides
    rep("Very low environmental concentrations; last-resort antibiotics", 2),

    # Oxazolidinones
    rep("Emerging contaminant; detected in hospital wastewater", 1),

    # Nitrofurans
    rep("Low persistence; primarily in hospital effluents", 1),

    # Rifamycins
    rep("Low environmental detection; specialized use", 1),

    # Dihydrofolate reductase inhibitors
    rep("Often paired with sulfonamides; moderate mobility", 1)
  ),

  stringsAsFactors = FALSE
)

# remove Orbifloxacin (no idea where this came from)
antibiotic_groups <- antibiotic_groups %>%
  dplyr::filter(antibiotic != "Orbifloxacin")


# Verify total count
cat("Total antibiotics:", nrow(antibiotic_groups), "\n")
cat("Groups:", paste(unique(antibiotic_groups$antibiotic_group), collapse = ", "), "\n")

# Save to CSV
write_csv(antibiotic_groups, "data/cleaned/antibiotic_groups.csv")
