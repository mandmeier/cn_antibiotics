# Yearbook metric name parsing, unit extraction, and harmonization helpers.

yearbook_units <- sort(unique(c(
  "unit",
  "10000_person_times_mixed", "100_million_person_times_mixed", "10000_person_times",
  "person_times_mixed", "person_times",
  "100_million_cu_m", "100_million_kWh", "100_million_yuan", "100_million_pieces",
  "100_million_units", "100_million_m",
  "10000_cu_m", "10000_sq_m", "10000_gigajoules", "10000_hectares", "10000_tons",
  "10000_persons", "10000_heads", "10000_yuan", "10000_units", "10000_beds",
  "10000_couples", "10000_kiloliters", "10000_weight_cases", "10000_kW",
  "1000_hectares", "1000_units", "100_tons",
  "cu_m_per_day", "ton_per_day", "ton_per_hour", "persons_per_sq_km",
  "km_per_sq_km", "Mega_Watts", "man_year",
  "preceding_year_100",
  "percent_mixed", "day_mixed",
  "per_mille", "per_sq_km",
  "hectare", "sq_km", "sq_m",
  "10000_person_times", "10000_gigajoules",
  "persons", "person", "heads", "beds", "bed", "tons", "ton", "kg",
  "couples", "pieces", "piece", "point",
  "yuan", "units", "number", "mixed", "percent", "pct",
  "day", "km", "liter", "GB",
  "100", "m"
)))
yearbook_units <- yearbook_units[order(nchar(yearbook_units), decreasing = TRUE)]

parse_yearbook_unit <- function(metric) {
  for (u in yearbook_units) {
    if (endsWith(metric, paste0("_", u))) {
      return(u)
    }
  }
  NA_character_
}

strip_yearbook_unit_suffix <- function(metric, unit) {
  if (is.na(unit)) {
    return(metric)
  }
  suffix <- paste0("_", unit)
  if (endsWith(metric, suffix)) {
    return(substr(metric, 1L, nchar(metric) - nchar(suffix)))
  }
  metric
}

harmonize_yearbook_metric <- function(metric) {
  metric <- str_replace_all(metric, " ", "_")
  metric <- str_remove(metric, "\\([^)]*\\)")
  metric <- gsub("Year_end", "year_end", metric, fixed = TRUE)

  metric <- recode(
    metric,
    "E13_13_Cloth_100illion_m" = "E13_13_Cloth_100_million_m",
    "E10_18_Production_and_Supply_of_Electricity_Heat_Gas_and_Water" =
      "E10_18_Production_and_Supply_of_Electricity_Heating_Gas_and_Water",
    "E10_18_Service_to_Households_Repair_and_Other_Services" =
      "E10_18_Services_to_Households_Repair_and_Other_Services"
  )

  metric
}

strip_residual_metric_suffixes <- function(metric, unit) {
  case_when(
    unit == "per_sq_km" & endsWith(metric, "_person") ~
      str_remove(metric, "_person$"),
    unit == "cu_m_per_day" & endsWith(metric, "_10000") ~
      str_remove(metric, "_10000$"),
    TRUE ~ metric
  )
}

apply_yearbook_units <- function(metric, unit = NA_character_) {
  parsed <- parse_yearbook_unit(metric)
  unit_out <- coalesce(parsed, unit)
  metric_out <- strip_yearbook_unit_suffix(metric, parsed)
  list(metric = metric_out, unit = unit_out)
}

parse_strip_and_normalize_units <- function(metric, unit = NA_character_) {
  applied <- apply_yearbook_units(metric, unit)
  metric_out <- applied$metric
  unit_out <- applied$unit
  unit_out <- if_else(unit_out == "unit", "counts", unit_out)
  metric_out <- strip_residual_metric_suffixes(metric_out, unit_out)
  list(metric = metric_out, unit = unit_out)
}

strip_residual_metric_suffixes_df <- function(data) {
  mutate(
    data,
    metric = case_when(
      unit == "per_sq_km" & endsWith(metric, "_person") ~
        str_remove(metric, "_person$"),
      unit == "cu_m_per_day" & endsWith(metric, "_10000") ~
        str_remove(metric, "_10000$"),
      TRUE ~ metric
    )
  )
}

parse_and_strip_yearbook_units <- function(data) {
  parsed <- vapply(data$metric, parse_yearbook_unit, character(1))

  data %>%
    mutate(
      unit = coalesce(parsed, unit),
      metric = mapply(
        strip_yearbook_unit_suffix,
        metric,
        parsed,
        USE.NAMES = FALSE
      ),
      unit = if_else(unit == "unit", "counts", unit)
    )
}

apply_yearbook_unit_parsing <- function(data) {
  if (!"unit" %in% names(data)) {
    data <- mutate(data, unit = NA_character_)
  }

  if (!all(is.na(data$unit))) {
    data <- strip_residual_metric_suffixes_df(data)
  }

  data <- parse_and_strip_yearbook_units(data)
  strip_residual_metric_suffixes_df(data)
}

audit_yearbook_metrics <- function(data, label = "yearbook") {
  metrics <- unique(data$metric)
  cat(sprintf("\n--- %s metric audit ---\n", label))
  cat("rows:", nrow(data), " unique metrics:", length(metrics), "\n")
  cat("with parentheses:", sum(grepl("[()]", metrics)), "\n")
  cat("with spaces:", sum(grepl(" ", metrics)), "\n")
  cat("Year_end (should be 0):", sum(grepl("Year_end", metrics)), "\n")
  cat("NA units:", sum(is.na(data$unit)), "\n")

  suffix_metrics <- metrics[!vapply(metrics, function(m) {
    is.na(parse_yearbook_unit(m))
  }, logical(1))]
  allow <- grepl("100_percent$|_Units$|1_to_10000", suffix_metrics)
  bad_suffix <- suffix_metrics[!allow]
  cat("metrics still ending with parseable unit suffix:", length(bad_suffix), "\n")
  if (length(bad_suffix) > 0) {
    print(bad_suffix)
  }

  invisible(list(
    n_metrics = length(metrics),
    paren = sum(grepl("[()]", metrics)),
    spaces = sum(grepl(" ", metrics)),
    na_units = sum(is.na(data$unit)),
    bad_suffix = bad_suffix
  ))
}

validate_yearbook_metrics <- function(data) {
  audit <- audit_yearbook_metrics(data, label = "validation")
  problems <- character()
  if (audit$paren > 0) {
    problems <- c(problems, "parentheses in metric names")
  }
  if (audit$spaces > 0) {
    problems <- c(problems, "spaces in metric names")
  }
  if (audit$na_units > 0) {
    problems <- c(problems, "missing units")
  }
  if (length(audit$bad_suffix) > 0) {
    problems <- c(problems, "unit suffixes remain in metric names")
  }
  if (length(problems) > 0) {
    stop(
      "Yearbook metric validation failed: ",
      paste(problems, collapse = "; ")
    )
  }
  cat("Yearbook metric validation passed.\n")
}
