# Harmonize antibiotic names in environmental measurements.
# Cross-dataset joins with resistance use ~15 shared drug names; resistance combos stay separate.

source("R/utils/environmental_units.R")
source("R/utils/antibiotic_classes.R")
source("R/utils/environmental_conversion_flags.R")
source("R/utils/reproducible_csv.R")

# 31 provincial-level units in CARSS (excludes National aggregate).
CARSS_PROVINCES <- c(
  "Anhui", "Beijing", "Chongqing", "Fujian", "Gansu", "Guangdong", "Guangxi",
  "Guizhou", "Hainan", "Hebei", "Heilongjiang", "Henan", "Hubei", "Hunan",
  "Inner Mongolia", "Jiangsu", "Jiangxi", "Jilin", "Liaoning", "Ningxia",
  "Qinghai", "Shaanxi", "Shandong", "Shanghai", "Shanxi", "Sichuan",
  "Tianjin", "Tibet", "Xinjiang", "Yunnan", "Zhejiang"
)

province_map <- c(
  setNames(CARSS_PROVINCES, str_to_lower(CARSS_PROVINCES)),
  "xizang" = "Tibet",
  "tibet autonomous region" = "Tibet",
  "xizang autonomous region" = "Tibet",
  "shaanxi (xi'an)" = "Shaanxi",
  "shaanxi (xian)" = "Shaanxi"
)

# Higher rank = newer / preferred source when identical measurements appear in multiple references.
# Supplemental references (N*, NEW_*, CAND_*) outrank Zhang pub_id values.
reference_source_rank <- function(reference_number) {
  ref <- as.character(reference_number)
  vapply(ref, function(r) {
    year <- suppressWarnings(as.numeric(str_extract(r, "(19|20)[0-9]{2}(?!\\d)")))
    if (is.na(year)) {
      years <- str_extract_all(r, "(19|20)[0-9]{2}")[[1]]
      if (length(years) > 0) {
        year <- suppressWarnings(as.numeric(tail(years, 1)))
      }
    }
    if (is.na(year)) {
      year <- 0
    }

    if (str_starts(r, "NEW_")) {
      return(4e8 + year * 1e3)
    }
    if (str_starts(r, "CAND_")) {
      return(3e8 + year * 1e3)
    }
    if (str_detect(r, "^N[0-9]+$")) {
      return(2e8 + as.numeric(str_remove(r, "^N")))
    }
    num <- suppressWarnings(as.numeric(r))
    if (!is.na(num)) {
      return(1e8 + num)
    }
    0
  }, numeric(1))
}

parse_sample_year <- function(year) {
  year_chr <- as.character(year)
  year_chr <- str_squish(year_chr)
  year_chr <- str_replace_all(year_chr, "[\u2013\u2014–—]", "-")
  year_chr <- if_else(
    is.na(year_chr) | year_chr %in% c("", "NA", "na"),
    NA_character_,
    year_chr
  )

  years_list <- str_extract_all(year_chr, "\\d{4}")
  vapply(years_list, function(years_chr) {
    years <- as.numeric(years_chr)
    years <- years[!is.na(years)]
    if (length(years) == 0) {
      return(NA_real_)
    }
    if (length(years) == 1) {
      return(years[[1]])
    }
    start <- years[[1]]
    end <- years[[length(years)]]
    # Midpoint of inclusive range; if tied, take the lower year.
    floor((start + end) / 2)
  }, numeric(1))
}

# Join Zhang admin levels / loc_Ref without turning missing parts into "NA".
paste_location_parts <- function(...) {
  parts_list <- list(...)
  n <- length(parts_list[[1]])
  vapply(seq_len(n), function(i) {
    parts <- vapply(parts_list, function(col) {
      val <- col[[i]]
      if (is.na(val)) {
        return(NA_character_)
      }
      val <- str_squish(as.character(val))
      if (val == "" || toupper(val) == "NA") {
        return(NA_character_)
      }
      val
    }, character(1))
    parts <- parts[!is.na(parts)]
    if (length(parts) == 0) {
      NA_character_
    } else {
      paste(parts, collapse = " ")
    }
  }, character(1))
}

# Harmonize location labels before site aggregation:
# - strip paste "NA" artifacts
# - collapse doubled province/municipality prefix (e.g. "Beijing Beijing …")
# - lowercase generic place words (district, river, …)
# - squish whitespace
clean_location_label <- function(location, province = NULL) {
  loc <- as.character(location)
  loc <- if_else(
    is.na(loc) | loc %in% c("", "NA", "na"),
    NA_character_,
    loc
  )
  loc <- str_replace_all(
    loc,
    regex("(?<=^|\\s)NA(?=\\s|$)", ignore_case = TRUE),
    " "
  )
  loc <- str_squish(loc)
  loc <- if_else(is.na(loc) | loc == "", NA_character_, loc)

  if (!is.null(province)) {
    prov <- str_squish(as.character(province))
    loc <- vapply(seq_along(loc), function(i) {
      x <- loc[[i]]
      p <- prov[[i]]
      if (is.na(x) || is.na(p) || p == "") {
        return(x)
      }
      doubled <- regex(
        paste0("^", str_escape(p), "\\s+", str_escape(p), "(?=\\s|$)"),
        ignore_case = TRUE
      )
      if (str_detect(x, doubled)) {
        x <- str_replace(
          x,
          regex(paste0("^", str_escape(p), "\\s+", str_escape(p)), ignore_case = TRUE),
          p
        )
      }
      x
    }, character(1))
  }

  generic_words <- c(
    "district", "districts",
    "river", "rivers",
    "town", "towns",
    "reservoir", "reservoirs",
    "basin", "basins",
    "island", "islands"
  )
  for (word in generic_words) {
    loc <- str_replace_all(
      loc,
      regex(paste0("\\b", word, "\\b"), ignore_case = TRUE),
      word
    )
  }

  loc <- str_squish(loc)
  if_else(is.na(loc) | loc == "", NA_character_, loc)
}

# Light season harmonization before site aggregation:
# - month names (-> optional year) to Zhang-style "1"-"12"
# - merge obvious synonyms
# - drop non-season labels
# Leave multi-season / phenology strings verbatim.
clean_season_label <- function(season) {
  s <- as.character(season)
  s <- if_else(
    is.na(s) | s %in% c("", "NA", "na"),
    NA_character_,
    str_squish(s)
  )

  s_lower <- str_to_lower(s)
  s <- if_else(s_lower == "sediment summary", NA_character_, s)
  s_lower <- str_to_lower(s)

  s <- if_else(
    s_lower %in% c("summer and autumn", "summer+autumn"),
    "summer+autumn",
    s
  )

  month_map <- c(
    january = "1",
    february = "2",
    march = "3",
    april = "4",
    may = "5",
    june = "6",
    july = "7",
    august = "8",
    september = "9",
    october = "10",
    november = "11",
    december = "12"
  )
  month_match <- str_match(
    str_to_lower(s),
    paste0(
      "^(",
      paste(names(month_map), collapse = "|"),
      ")(?:\\s+\\d{4})?$"
    )
  )[, 2]
  s <- if_else(
    !is.na(month_match),
    unname(month_map[month_match]),
    s
  )

  if_else(is.na(s) | s == "", NA_character_, s)
}

# import Zhang data, wrangle into standard format
zhang <- read_excel("data/raw/environmental_data/Zhang_2022.xls", sheet = "Records")

zhang_formatted <- zhang %>%
  # add location variable (skip missing loc_l* / loc_Ref so paste does not emit "NA")
  mutate(loc = paste_location_parts(loc_l1, loc_l2, loc_l3, loc_l4, loc_Ref)) %>%
  group_by(sem_type, loc, ABX_subcat, sam_Y, sam_M, ABX_conc_max, ABX_conc_mean, pub_id) %>%
  # remove duplicate measurements
  select(
    sample_type = sem_type,
    province = loc_l1,
    location = loc,
    lon = lon,
    lat = lat,
    sample_year = sam_Y,
    season = sam_M,
    antibiotic = ABX_subcat,
    mean_concentration = ABX_conc_mean,
    max_concentration = ABX_conc_max,
    reference_number = pub_id
  ) %>%
  unique() %>%
  ungroup() %>%
  # add units
  mutate(concentration_unit = "ng/g", .after = max_concentration) %>%
  # type formatting
  mutate(sample_year = as.character(sample_year)) %>%
  mutate(season = as.character(season)) %>%
  mutate(reference_number = as.character(reference_number)) %>%
  # convert to numeric measurements
  mutate(mean_concentration = as.numeric(mean_concentration)) %>%
  mutate(max_concentration = as.numeric(max_concentration)) %>%
  mutate(lon = as.numeric(lon), lat = as.numeric(lat)) %>%
  # Zhang unit corrections (default placeholder is ng/g):
  # pub_id 223 (Li et al. 2008): sludge/sediment values are paper mg/kg (dw) × 1000.
  # pub_id 113 (Zhao et al. 2017): soil values are paper µg/kg (dw); 1 µg/kg = 1 ng/g.
  mutate(
    concentration_unit = case_when(
      reference_number == "223" ~ "mg/kg dw",
      reference_number == "113" ~ "µg/kg dw",
      TRUE ~ concentration_unit
    ),
    mean_concentration = if_else(
      reference_number == "223",
      mean_concentration / 1000,
      mean_concentration
    ),
    max_concentration = if_else(
      reference_number == "223",
      max_concentration / 1000,
      max_concentration
    )
  )

# import supplemental environmental antibiotics data
supplemental <- read_csv(
  "data/raw/environmental_data/China_Environmental_Supplemental.csv",
  show_col_types = FALSE
) %>%
  mutate(
    sample_year = as.character(sample_year),
    season = as.character(season),
    reference_number = as.character(reference_number),
    mean_concentration = as.numeric(mean_concentration),
    max_concentration = as.numeric(max_concentration)
  )

combined_data <- zhang_formatted %>%
  bind_rows(supplemental) %>%
  filter(!is.na(mean_concentration) | !is.na(max_concentration)) %>%
  filter(!is.na(antibiotic), str_squish(antibiotic) != "")

n_antibiotics_before <- n_distinct(combined_data$antibiotic)

environmental_clean <- combined_data %>%

  #### Harmonize sample_type
  mutate(
    sample_type = iconv(sample_type, from = "", to = "UTF-8", sub = ""),
    sample_type = str_squish(sample_type),
    sample_type = str_to_lower(sample_type),
    sample_type = str_replace_all(sample_type, "_", " "),
    sample_type = str_replace_all(sample_type, "\\s+", " ")
  ) %>%
  mutate(
    sample_type = case_when(
      # --- soil ---
      sample_type %in% c("agricultural soil", "farm soil", "farmland soil") ~ "agricultural soil",
      sample_type %in% c("soil", "surface soil", "animal feed", "manure") ~ "soil",

      # --- sediment ---
      sample_type %in% c("sediments") ~ "sediment",
      sample_type %in% c("soil/sediment combined", "soil / sediment combined") ~ "sediment",
      sample_type %in% c("sediment", "surface sediment") ~ "sediment",
      sample_type %in% c("river sediment", "sediment (river)", "riverbed sediment") ~ "river sediment",
      sample_type %in% c("lake sediment", "sediment (lake)") ~ "lake sediment",
      sample_type %in% c("coastal/river sediment", "coastal sediment", "estuary sediment") ~ "coastal/river sediment",
      sample_type %in% c(
        "surface water sediment",
        "sediment (surface water)",
        "surface-water sediment",
        "surfacewater sediment"
      ) ~ "surface water sediment",
      sample_type %in% c("aquaculture pond sediment", "aquaculture sediment") ~ "aquaculture pond sediment",
      sample_type %in% c("mariculture sediment") ~ "mariculture sediment",
      sample_type %in% c(
        "river water (suspended matter fraction)",
        "suspended matter (river water)",
        "suspended matter fraction (river water)"
      ) ~ "river water (suspended matter fraction)",
      sample_type %in% c(
        "suspended particulate matter (river)",
        "suspended particulate matter",
        "spm (river)"
      ) ~ "suspended particulate matter (river)",

      # --- surface water ---
      sample_type %in% c("surface water", "surfacewater", "canal surface water") ~ "surface water",
      sample_type %in% c("river water", "river") ~ "river water",
      sample_type %in% c("coastal water", "coastal seawater", "seawater", "sea water") ~ "coastal water",
      sample_type %in% c("inland lake water", "lake water") ~ "inland lake water",
      sample_type %in% c("aquaculture pond water", "aquaculture water", "pond water") ~ "aquaculture pond water",
      sample_type %in% c("mariculture water") ~ "mariculture water",
      sample_type %in% c("groundwater", "ground water") ~ "groundwater",

      # --- sludge ---
      sample_type %in% c("municipal sludge", "sewage sludge") ~ "municipal sludge",
      sample_type %in% c("industrial sludge") ~ "industrial sludge",
      sample_type %in% c("msw incineration sludge") ~ "MSW incineration sludge",
      sample_type %in% c("fermentation residue") ~ "fermentation residue",

      # --- wastewater influent ---
      sample_type %in% c(
        "municipal wastewater influent",
        "urban wastewater (influent)",
        "wastewater influent",
        "wwtp influent",
        "influent"
      ) ~ "wastewater influent",
      sample_type %in% c("municipal wastewater (urban sewer)") ~ "municipal wastewater (urban sewer)",
      sample_type %in% c("msw incineration leachate") ~ "MSW incineration leachate",

      # --- wastewater effluent ---
      sample_type %in% c(
        "municipal wastewater effluent",
        "urban wastewater (effluent)",
        "wastewater effluent",
        "wwtp effluent",
        "effluent"
      ) ~ "municipal wastewater effluent",
      sample_type %in% c("msw incineration leachate (nf treated)") ~ "MSW incineration leachate (NF treated)",
      sample_type %in% c(
        "msw incineration leachate (treated effluent)",
        "msw incineration leachate (treatment effluent)"
      ) ~ "MSW incineration leachate (treated effluent)",
      sample_type %in% c("livestock wastewater") ~ "livestock wastewater",
      sample_type %in% c("hospital wastewater") ~ "hospital wastewater",
      sample_type %in% c("pharmaceutical wastewater") ~ "pharmaceutical wastewater",
      sample_type %in% c("aquaculture wastewater") ~ "aquaculture wastewater",

      TRUE ~ sample_type
    )
  ) %>%

  #### Harmonize province
  mutate(
    province = iconv(province, from = "", to = "UTF-8", sub = ""),
    province = str_squish(province),
    province_key = str_to_lower(province),
    province_key = str_replace_all(province_key, "[\u2018\u2019`']", "'"),
    province = case_when(
      is.na(province_key) | province_key == "" ~ NA_character_,
      str_detect(
        province_key,
        "multiple|nationwide|n/a|/|basin|delta|corridor|coastal provinces"
      ) ~ NA_character_,
      province_key %in% c("hong kong", "macau", "taiwan") ~ NA_character_,
      province_key %in% names(province_map) ~ unname(province_map[province_key]),
      TRUE ~ NA_character_
    )
  ) %>%
  select(-province_key) %>%
  filter(!is.na(province)) %>%

  #### Harmonize sample_year
  mutate(sample_year = parse_sample_year(sample_year)) %>%

  #### Harmonize antibiotic
  # Removed because it is not clear if Zhi Su used Penicillin G or Penicillin V.
  # These are chemically distinct.
  {
    df <- .
    if ("source_dataset" %in% names(df)) {
      df <- df %>%
        filter(
          !(source_dataset == "Su_2025_Table2_PDF_text" & antibiotic == "Penicillin (PEN)")
        )
    }
    df
  } %>%
  mutate(
    antibiotic = iconv(antibiotic, from = "", to = "UTF-8", sub = ""),
    antibiotic = str_squish(antibiotic),
    antibiotic_raw = antibiotic,

    antibiotic = case_when(
      antibiotic %in% c("NOR (summer)", "NOR (winter)", "NOR") ~ "Norfloxacin",
      antibiotic %in% c("OFX (summer)", "OFX (winter)", "OFX") ~ "Ofloxacin",
      antibiotic == "ROX" ~ "Roxithromycin",
      antibiotic == "SMX" ~ "Sulfamethoxazole",
      TRUE ~ antibiotic
    ),

    antibiotic = str_remove(
      antibiotic,
      regex(" (?i)(hydrochloride|hyclate|mesylate|tosylate|phosphate|sodium salt|sodium)$")
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
      "Cefotaxime Sodium" = "Cefotaxime",
      "Penicillin" = "Penicillin G",
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
      "Pipemidicacid" = "Pipemidic Acid",

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
      "Sulfadimethazine" = "Sulfadimidine",
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
      "Sulfachloropyridine" = "Sulfachloropyridazine",
      "Sulfaquinoline" = "Sulfaquinoxaline",
      "Sulphathiazole" = "Sulfathiazole",
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
      "Trimethoprimg" = "Trimethoprim",

      # --- Tetracycline class totals (proxy as Tetracycline) ---
      "Tetracyclines (sum)" = "Tetracycline",
      "Tetracyclines (total)" = "Tetracycline",
      "Tetracyclines (dominant class)" = "Tetracycline",
      "Tetracyclines" = "Tetracycline",
      "Total Tetracyclines (TCs)" = "Tetracycline",
      "Total Tetracyclines" = "Tetracycline",
      "Total tetracyclines (class sum)" = "Tetracycline",

      .default = antibiotic
    ),

    # Catch remaining class-level (Sum)/(Total) variants
    antibiotic = str_replace(antibiotic, " \\((Sum|Total)\\)$", ""),
    antibiotic = str_replace(antibiotic, " \\((Sum|Total), [^)]+\\)$", ""),
    antibiotic = str_replace(antibiotic, " \\(Dominant Class\\)$", ""),
    antibiotic = str_replace(antibiotic, " \\(Dominant Class in Sediment\\)$", ""),
    antibiotic = str_replace(antibiotic, " \\(Median\\)$", ""),
    antibiotic = str_replace(antibiotic, regex(" \\(class summary\\)$", ignore_case = TRUE), ""),
    antibiotic = str_replace(antibiotic, regex(" \\(class sum\\)$", ignore_case = TRUE), ""),

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
  {
    df <- .
    if ("source_dataset" %in% names(df)) {
      df <- df %>%
        mutate(
          antibiotic = if_else(
            source_dataset == "Wu_2025.csv" & antibiotic == "Penicillin",
            "Penicillin G",
            antibiotic
          )
        )
    }
    df
  } %>%
  {
    df <- .
    n_before_agg <- nrow(df)
    df <- df %>%
      filter(
        !vapply(antibiotic, is_aggregate_antibiotic, logical(1)),
        !vapply(antibiotic, is_tetracycline_class_total, logical(1))
      )
    message(
      "Removed ", n_before_agg - nrow(df),
      " aggregate / composite measurement(s)"
    )
    df
  } %>%
  select(-antibiotic_raw) %>%
  filter(!antibiotic %in% c(
    "Atenolol",
    "Metoprolol",
    "Propranolol",
    "Fluconazole",
    "Pimaricin",
    "Monensin",
    "Narasin",
    "Nicarbazin",
    "Chloramphenicol derivatives",
    "Cyromazine",
    "Thiabendazole",
    "multiple classes",
    "sulfonamides"
  )) %>%
  # Removed solid waste: different antibiotics concentration pattern from other
  # matrices and only n = 2 samples.
  filter(sample_type != "solid waste") %>%
  mutate(
    antibiotic = case_when(
      antibiotic == "Penicillin" ~ "Penicillin G",
      antibiotic == "Tetracyclines" ~ "Tetracycline",
      TRUE ~ antibiotic
    ),
    antibiotic_class = assign_antibiotic_group(antibiotic),
    .after = antibiotic
  ) %>%
  select(-any_of("group_of_antibiotic")) %>%

  #### Harmonize concentration_unit
  mutate(
    matrix = assign_environmental_matrix(sample_type, reference_number),
    .after = sample_type
  ) %>%
  # unclear how to convert measurments measured in liquid sample to solid sludge,
  # removed 7 measurements from sources N69, N92, N451
  filter(
    !(
      matrix == "sludge" &
        normalize_unit_string(concentration_unit) %in% c("mg/l", "µg/l")
    )
  ) %>%
  # unclear how to convert measurments measured in solid sample to liquid surface water,
  # removed 5 measurements from sources N453 and N459
  filter(
    !(
      matrix == "surface water" &
        normalize_unit_string(concentration_unit) %in% c("mg/kg", "ng/kg")
    )
  ) %>%
  # unclear how to convert measurments measured in solid sample to liquid surface water,
  # removed 21 measurements from sources N154, N453, N455, and N457
  filter(
    !(
      matrix == "surface water" &
        grepl("^ng/g", normalize_unit_string(concentration_unit))
    )
  ) %>%
  mutate(
    .unit_norm = normalize_unit_string(concentration_unit),
    .is_dw = unit_is_dry_weight(.unit_norm),
    previous_unit = concentration_unit,
    recalculated_factor = unit_conversion_factor(concentration_unit),
    mean_concentration = mean_concentration * recalculated_factor,
    max_concentration = max_concentration * recalculated_factor,
    concentration_unit = target_concentration_unit(matrix, .is_dw),
  # Keep source unit when scale factor is 1 but labels differ (e.g. µg/kg dw → ng/g dw).
    previous_unit = if_else(
      recalculated_factor == 1 &
        normalize_unit_string(previous_unit) !=
          normalize_unit_string(concentration_unit),
      previous_unit,
      if_else(recalculated_factor == 1, NA_character_, previous_unit)
    )
  ) %>%
  select(-.unit_norm, -.is_dw) %>%
  relocate(recalculated_factor, .after = concentration_unit) %>%
  relocate(previous_unit, .after = recalculated_factor)

# Clean location / season labels before any site-level aggregation / dedup.
environmental_clean <- environmental_clean %>%
  mutate(
    location = clean_location_label(location, province),
    season = clean_season_label(season)
  )

# Site-level snapshot (location + season + coordinates) before province-oriented
# collapse. Downstream analysis still uses environmental_cleaned.csv.
n_before_site_dedup <- nrow(environmental_clean)
environmental_by_site <- environmental_clean %>%
  mutate(.source_rank = reference_source_rank(reference_number)) %>%
  group_by(
    sample_type,
    matrix,
    province,
    location,
    season,
    sample_year,
    antibiotic,
    mean_concentration,
    max_concentration
  ) %>%
  slice_max(.source_rank, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-.source_rank, -recalculated_factor, -previous_unit) %>%
  relocate(location, season, .after = province) %>%
  relocate(lon, lat, .after = season) %>%
  arrange(
    sample_type,
    province,
    location,
    season,
    sample_year,
    antibiotic,
    reference_number
  )

message(
  "Site-level rows: ", n_before_site_dedup, " -> ", nrow(environmental_by_site),
  " (kept latest source per site/season)"
)
write_csv_reproducible(
  environmental_by_site,
  "data/intermediate/environmental_by_site.csv"
)

# Coordinates stay only on the site product; province collapse / audit unchanged.
environmental_clean <- environmental_clean %>%
  select(-lon, -lat)

# Province-oriented collapse (unchanged): drop season so same-year seasonal
# duplicates collapse, then keep one source per province measurement key.
environmental_clean <- environmental_clean %>%
  select(-season) %>%
  unique()

n_before_measurement_dedup <- nrow(environmental_clean)
environmental_clean <- environmental_clean %>%
  mutate(.source_rank = reference_source_rank(reference_number)) %>%
  group_by(
    sample_type,
    matrix,
    province,
    sample_year,
    antibiotic,
    mean_concentration,
    max_concentration
  ) %>%
  slice_max(.source_rank, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  select(-.source_rank)

message(
  "Removed ", n_before_measurement_dedup - nrow(environmental_clean),
  " duplicate measurement(s) (kept latest source)"
)

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
if (any(is.na(environmental_clean$recalculated_factor))) {
  stop(
    "Unparseable concentration_unit on ",
    sum(is.na(environmental_clean$recalculated_factor)),
    " row(s)"
  )
}

# Unit-conversion audit (uses validation columns before they are dropped).
validation_dir <- "data/intermediate/validation"
dir.create(validation_dir, recursive = TRUE, showWarnings = FALSE)

conversion_audit <- flag_environmental_concentrations(
  environmental_clean,
  from_cleaned = TRUE
)

suspicious_output_cols <- c(
  "reference_number",
  "province",
  "location",
  "sample_type",
  "matrix",
  "sample_year",
  "antibiotic",
  "antibiotic_class",
  "unit_raw",
  "mean_raw",
  "max_raw",
  "conversion_factor",
  "unit_converted",
  "mean_converted",
  "max_converted",
  "recalculated_factor",
  "previous_unit",
  "flag_codes",
  "flag_reasons",
  "suggested_unit_note"
)

suspicious <- conversion_audit$suspicious %>%
  dplyr::select(dplyr::any_of(suspicious_output_cols))

suspicious_path <- file.path(validation_dir, "suspicious_conversions.csv")
audit_path <- file.path(validation_dir, "unit_conversion_audit.csv")
summary_path <- file.path(validation_dir, "unit_conversion_audit_summary.txt")

write_csv_reproducible(suspicious, suspicious_path)
write_csv_reproducible(conversion_audit$audited, audit_path)

# Keep prior generated timestamp when regenerating so committed summaries stay stable.
prev_generated <- character()
if (file.exists(summary_path)) {
  prev_generated <- grep(
    "^generated:",
    readLines(summary_path, warn = FALSE),
    value = TRUE
  )
}
audit_summary_lines <- c(
  if (length(prev_generated) > 0) {
    prev_generated[[1]]
  } else {
    paste0("generated: ", Sys.time())
  },
  paste0("rows_audited: ", nrow(conversion_audit$audited)),
  paste0("rows_flagged: ", nrow(suspicious)),
  ""
)
if (nrow(suspicious) > 0) {
  flag_tbl <- sort(
    table(unlist(strsplit(suspicious$flag_codes, ";"))),
    decreasing = TRUE
  )
  audit_summary_lines <- c(
    audit_summary_lines,
    "flag_counts:",
    paste0("  ", names(flag_tbl), ": ", flag_tbl),
    ""
  )
}
if (
  !file.exists(summary_path) ||
    !identical(audit_summary_lines, readLines(summary_path, warn = FALSE))
) {
  writeLines(audit_summary_lines, summary_path)
}

message(
  "Unit conversion audit: ", nrow(suspicious), " flagged of ",
  nrow(conversion_audit$audited), " rows with mean concentration ",
  "(see ", validation_dir, "/)"
)

environmental_clean <- environmental_clean %>%
  dplyr::select(
    -location,
    -recalculated_factor,
    -previous_unit
  )

write_csv_reproducible(
  environmental_clean,
  "data/intermediate/environmental_cleaned.csv"
)
