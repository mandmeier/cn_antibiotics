# data cleaning

library(readxl)
library(tidyverse)
library(dplyr)
library(stringr)
library(readr)
library(purrr)

options(scipen = 999)


#### Wu_2025 ####
# Read a specific sheet by name
wu <- read_excel("antibiotics_data/clean_data/clean_data.xlsx", sheet = "Wu_2025")

parse_concentration <- function(x) {

  map_dfr(x, function(val) {

    if (is.na(val)) {
      return(tibble(value = NA_real_, unit = NA_character_))
    }

    val <- as.character(val)

    # normalize weird encodings
    val_clean <- val %>%
      str_replace_all("√ó", "*") %>%
      str_replace_all("‚Äì|–|—", "-") %>%
      str_replace_all("¬±|±|¬†¬±¬†", "±") %>%
      str_replace_all("Ôºú", "<") %>%
      str_squish()

    # extract unit (everything after last number/expression)
    unit <- str_extract(val_clean, "(mg/kg TS|mg/kg|mg/L|ng/g|EQ|Ery-A EQ|SPM EQ|OTC EQ)$")

    # handle < values
    if (str_detect(val_clean, "^<")) {
      num <- str_extract(val_clean, "[0-9.]+")
      return(tibble(
        value = as.numeric(num),
        unit = unit
      ))
    }

    # handle scientific notation like 76*10^-3 or 187*10^(-3)
    if (str_detect(val_clean, "\\*10\\^")) {
      base <- as.numeric(str_extract(val_clean, "^[0-9.]+"))
      exp  <- as.numeric(str_extract(val_clean, "(?<=10\\^\\(?)-?[0-9]+"))
      return(tibble(
        value = base * 10^exp,
        unit = unit
      ))
    }

    # handle ranges (take first number)
    if (str_detect(val_clean, "-")) {
      num <- str_extract(val_clean, "[0-9.]+")
      return(tibble(
        value = as.numeric(num),
        unit = unit
      ))
    }

    # handle ± (take mean)
    if (str_detect(val_clean, "±")) {
      num <- str_extract(val_clean, "[0-9.]+")
      return(tibble(
        value = as.numeric(num),
        unit = unit
      ))
    }

    # handle plain numbers / E notation
    num <- suppressWarnings(as.numeric(val_clean))
    if (!is.na(num)) {
      return(tibble(
        value = num,
        unit = unit
      ))
    }

    # fallback
    tibble(value = NA_real_, unit = unit)
  })
}
extract_first_numeric <- function(x) {
  # Ensure character
  x <- as.character(x)

  # Remove common prefixes like "About", "Nearly", etc.
  x <- str_replace_all(x, c("About" = "", "Nearly" = ""))
  x <- str_trim(x)

  # Extract first numeric value (integer, decimal, scientific)
  num_str <- str_extract(x, "[0-9]+\\.?[0-9]*(?:[eE][+-]?[0-9]+)?")

  # Convert to numeric
  as.numeric(num_str)
}

wu <- wu %>%

  # parse city, province, country
  mutate(author_loc_ref = str_trim(author_loc_ref)) %>%
  mutate(author_loc_ref2 = author_loc_ref) %>%
  separate(author_loc_ref2, into = c("part1", "part2", "part3"), sep = ",", fill = "left", extra = "merge") %>%
  mutate(
    country = part3 %||% part2,
    province = case_when(
      !is.na(part3) ~ part2,
      !is.na(part2) ~ part1,
      TRUE ~ NA_character_
    ),
    city = case_when(
      !is.na(part3) ~ part1,
      TRUE ~ NA_character_
    )
  ) %>%
  select(-part1, -part2, -part3) %>%

  # parse concentration
  mutate(parsed = parse_concentration(ABX_conc_mean)) %>%
  unnest(parsed) %>%

  # fix numeric values
  mutate(value = ifelse(is.na(value), extract_first_numeric(ABX_conc_mean), value)) %>%
  mutate(value = ifelse(grepl("E-2", ABX_conc_mean), value/100, value)) %>%
  mutate(value = ifelse(grepl("E-3", ABX_conc_mean), value/1000, value)) %>%

  # fix units
  mutate(unit = ifelse(grepl("sludge", sample_type, ignore.case = TRUE), "mg/kg", unit)) %>%
  mutate(unit = ifelse(grepl("residue", sample_type, ignore.case = TRUE), "mg/L", unit)) %>%
  mutate(ABX_conc_mean = value) %>%
  select(-value) %>%

  #trim whitespace
  mutate(across(where(is.character), ~ trimws(.))) %>%
  mutate(country = ifelse(country == "Pharmaceutical Plant", NA, country))



#### Yu_2025 ####


yu <- read_excel("antibiotics_data/clean_data/clean_data.xlsx", sheet = "Yu_2025")


abx_lookup <- tibble::tribble(
  ~ABX, ~ABX_cat,
  "AGC", "aminoglycosides",
  "BLC", "beta-lactams",
  "FAC", "folate pathway antagonists",
  "MCC", "macrolides",
  "LCC", "lincosamides",
  "NIC", "nitroimidazoles",
  "PCC", "phenicols",
  "QLC", "quinolones",
  "TCC", "tetracyclines"
)

yu <- yu %>%
  pivot_longer(cols = BLC:TCC, names_to = "ABX", values_to = "ABX_conc_mean") %>%

  # fill in ABX names
  left_join(abx_lookup, by = "ABX") %>%
  relocate(ABX_cat, .after = "ABX") %>%
  mutate(ABX_subcat = ifelse(nchar(ABX) == 3, NA, ABX), .after = "ABX_cat") %>%
  mutate(ABX_subcat_abbre = NA, .after = "ABX_subcat") %>%
  select(-ABX) %>%
  # antibiotics concentrations are log10 transformed. undo this. concentration in ng/L
  mutate(ABX_conc_mean = 10^(ABX_conc_mean)) %>%
  mutate(unit = "ng/L")


#### Cho_2023 ####


cho <- read_excel("antibiotics_data/clean_data/clean_data.xlsx", sheet = "Cho_2023")

abx_lookup <- tribble(
  ~ABX, ~ABX_subcat,
  "AMO", "amoxicillin",
  "AMP", "ampicillin",
  "AZI", "azithromycin",
  "AXO", "ceftriaxone",
  "TAZ", "ceftazidime",
  "TIO", "ceftiofur",
  "CIP", "ciprofloxacin",
  "DAP", "daptomycin",
  "ERY", "erythromycin",
  "GEN", "gentamicin",
  "KAN", "kanamycin",
  "LIN", "lincomycin",
  "LNZ", "linezolid",
  "MER", "meropenem",
  "MET", "methicillin",
  "NAL", "nalidixic acid",
  "OXA", "oxacillin",
  "PEN", "penicillin",
  "SMX", "sulfamethoxazole",
  "FIS", "sulfisoxazole",
  "STR", "streptomycin",
  "TIG", "tigecycline",
  "TRI", "trimethoprim",
  "TET", "tetracycline",
  "TYL", "tylosin",
  "VAN", "vancomycin"
)


cho <- cho %>%
  pivot_longer(cols = Amo:Van, names_to = "ABX", values_to = "ABX_conc_mean") %>%

  # fill in ABX names
  mutate(ABX = toupper(ABX)) %>%
  # fix wrong spelling
  mutate(ABX = ifelse(ABX == "SUL", "SMX", ABX)) %>%
  mutate(ABX_cat = NA, .after = "ABX") %>%
  left_join(abx_lookup, by = "ABX") %>%
  relocate(ABX_subcat, .after = "ABX_cat") %>%
  mutate(ABX_subcat_abbre = ABX, .after = "ABX_subcat") %>%
  select(-ABX) %>%
  mutate(unit = "ng/L")



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


write_csv(zhang, "antibiotics_data/clean_data/Zhang_2022_clean.csv")



#### Singer_2014 ####


singer <- read_excel("antibiotics_data/clean_data/clean_data.xlsx", sheet = "Singer_2014")



test <- singer %>%
  mutate(
    DATE = case_when(
      inherits(DATE, "Date") ~ DATE,
      grepl("^\\d+$", as.character(DATE)) ~
        as.Date(as.numeric(as.character(DATE)), origin = "1899-12-30"),
      grepl("^\\d{2}/\\d{2}/\\d{4}$", as.character(DATE)) ~
        dmy(as.character(DATE)),
      TRUE ~ as.Date(NA)
    )
  )
