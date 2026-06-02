# Pharmacological class labels and aggregate detection for environmental antibiotics.

ANTIBIOTIC_GROUP_LEVELS <- c(
  "Fluoroquinolones",
  "Quinolones",
  "Sulfonamides",
  "Tetracyclines",
  "Macrolides",
  "Lincosamides",
  "Phenicols",
  "Beta-lactams",
  "Diaminopyrimidines",
  "Other"
)

AGGREGATE_ANTIBIOTIC_EXACT <- c(
  "Fluoroquinolones",
  "Macrolides",
  "Sulfonamides",
  "Lincosamides",
  "Quinolones",
  "Multiple Antibiotics",
  "All Antibiotics",
  "Total Antibiotics",
  "Total Quinolones",
  "Total Fishery Drugs",
  "Total Chloramphenicols"
)

is_tetracycline_class_total <- function(name) {
  if (is.na(name) || !nzchar(name)) {
    return(FALSE)
  }
  n <- str_squish(name)
  if (n %in% c(
    "Tetracyclines",
    "Total Tetracyclines",
    "Total Tetracyclines (TCs)"
  )) {
    return(TRUE)
  }
  grepl(
    "^Tetracyclines( \\(sum|total|dominant class\\))?$",
    n,
    ignore.case = TRUE
  ) ||
    grepl("^Total Tetracyclines", n, ignore.case = TRUE) ||
    grepl("^Total tetracyclines", n, ignore.case = TRUE) ||
    grepl("tetracyclines.*\\(class sum\\)", n, ignore.case = TRUE)
}

is_aggregate_antibiotic <- function(name) {
  if (is.na(name) || !nzchar(name)) {
    return(FALSE)
  }
  if (is_tetracycline_class_total(name)) {
    return(FALSE)
  }
  n <- str_squish(name)
  if (n %in% AGGREGATE_ANTIBIOTIC_EXACT) {
    return(TRUE)
  }
  if (grepl("^Total ", n, ignore.case = TRUE)) {
    return(TRUE)
  }
  if (grepl("^Mixed ", n, ignore.case = TRUE)) {
    return(TRUE)
  }
  if (grepl("multiple antibiotics", n, ignore.case = TRUE)) {
    return(TRUE)
  }
  if (grepl("^all antibiotics", n, ignore.case = TRUE)) {
    return(TRUE)
  }
  grepl(
    "\\(sum\\)|\\(total\\)|\\(cumulative\\)|dominant class|QNs\\+SAs|\\+ aminophenyl",
    n,
    ignore.case = TRUE
  )
}

infer_antibiotic_group <- function(antibiotic) {
  abx <- antibiotic

  dplyr::case_when(
    abx %in% c(
      "Ciprofloxacin", "Norfloxacin", "Ofloxacin", "Enrofloxacin", "Levofloxacin",
      "Danofloxacin", "Marbofloxacin", "Pefloxacin", "Sparfloxacin", "Tosufloxacin",
      "Fleroxacin", "Flumequine", "Gatifloxacin", "Lomefloxacin", "Moxifloxacin",
      "Nadifloxacin", "Difloxacin", "Enoxacin", "Sarafloxacin"
    ) ~ "Fluoroquinolones",

    abx %in% c(
      "Nalidixic Acid", "Oxolinic Acid", "Pipemidic Acid", "Cinoxacin"
    ) ~ "Quinolones",

    abx %in% c(
      "Sulfabenzamide", "Sulfacetamide", "Sulfachinoxaline", "Sulfachloropyridazine",
      "Sulfadiazine", "Sulfadimethoxine", "Sulfadimethoxypyrimidine", "Sulfadimidine",
      "Sulfadimoxine", "Sulfadoxine", "Sulfafurazole", "Sulfaguanidine",
      "Sulfamerazine", "Sulfameter", "Sulfamethizole", "Sulfamethoxazole",
      "Sulfamethoxydiazine", "Sulfamethoxypyridazine", "Sulfamonomethoxine",
      "Sulfamoxole", "Sulfanilamide", "Sulfanitran", "Sulfaphenazole",
      "Sulfapyridine", "Sulfaquinoxaline", "Sulfathiazole", "Sulfisomidine",
      "Acetylsulfamethazine", "Acetylsulfamethoxazole"
    ) ~ "Sulfonamides",

    abx %in% c(
      "Tetracycline", "Oxytetracycline", "Chlortetracycline", "Doxycycline",
      "Apo-Oxytetracycline", "Methacycline", "Demeclocycline", "Minocycline",
      "4-Epichlortetracycline", "Anhydrochlortetracycline", "Isochlortetracycline",
      "Epitetracycline", "Epioxytetracycline", "Epianhydrotetracycline"
    ) ~ "Tetracyclines",

    abx %in% c(
      "Erythromycin", "Azithromycin", "Roxithromycin", "Clarithromycin",
      "Spiramycin", "Acetylspiramycin", "Tylosin", "Tilmicosin", "Josamycin",
      "Kitasamycin", "Leucomycin", "Oleandomycin", "Dehydroerythromycin",
      "Erythromycin-H2O", "Erythromycin A dihydrate", "Roxithromycin-H2O"
    ) ~ "Macrolides",

    abx %in% c("Lincomycin", "Clindamycin") ~ "Lincosamides",

    abx %in% c("Chloramphenicol", "Florfenicol", "Thiamphenicol") ~ "Phenicols",

    abx %in% c(
      "Penicillin G", "Penicillin V", "Amoxicillin", "Ampicillin", "Cephalexin",
      "Cefadroxil", "Cefazolin", "Cefotaxime", "Ceftriaxone", "Cloxacillin",
      "Mecillinam", "Deacetoxycephalosporin"
    ) ~ "Beta-lactams",

    abx %in% c("Trimethoprim", "Ormetoprim") ~ "Diaminopyrimidines",

    TRUE ~ "Other"
  )
}

assign_antibiotic_group <- function(
  antibiotic,
  lookup_path = "data/reference/antibiotic_group_lookup.csv"
) {
  lookup <- readr::read_csv(lookup_path, show_col_types = FALSE)
  out <- dplyr::tibble(antibiotic = antibiotic) %>%
    dplyr::left_join(lookup, by = "antibiotic")

  unmapped <- unique(out$antibiotic[is.na(out$group_of_antibiotic)])
  if (length(unmapped) > 0) {
    stop(
      "Unmapped antibiotic(s) in ", lookup_path, ": ",
      paste(sort(unmapped), collapse = ", ")
    )
  }

  out$group_of_antibiotic
}
