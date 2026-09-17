# Flag suspicious unit conversions on environmental concentration data.

suppressPackageStartupMessages({
  library(dplyr)
  library(stringr)
})
source("R/utils/environmental_units.R")
source("R/utils/reproducible_csv.R")

PLAUSIBLE_MIN <- 0.01
PLAUSIBLE_MAX <- 1e6
EXTREME_CONVERTED <- 1e9
HIGH_CONVERTED_SOLID <- 1e6
HIGH_CONVERTED_LIQUID <- 1e5
IMPLAUSIBLE_G_PER_KG <- 10
LARGE_VALUE_HIGH_FACTOR <- 1000

in_range <- function(x, min_val = PLAUSIBLE_MIN, max_val = PLAUSIBLE_MAX) {
  !is.na(x) & x >= min_val & x <= max_val
}

labeled_unit_label <- function(unit_norm) {
  dplyr::case_when(
    grepl("^g/kg", unit_norm) ~ "g/kg",
    grepl("^mg/kg", unit_norm) ~ "mg/kg",
    grepl("^(ug|µg)/kg", unit_norm) ~ "µg/kg",
    grepl("^ng/g", unit_norm) ~ "ng/g",
    grepl("^(ug|µg)/g", unit_norm) ~ "µg/g",
    grepl("^mg/l", unit_norm) ~ "mg/L",
    grepl("^(ug|µg)/l", unit_norm) ~ "µg/L",
    grepl("^ng/l", unit_norm) ~ "ng/L",
    TRUE ~ unit_norm
  )
}

best_alternative_note <- function(
  mean_raw,
  unit_norm,
  matrix,
  applied_factor,
  unit_converted
) {
  if (is.na(mean_raw)) {
    return(NA_character_)
  }

  alt <- if (is_liquid_matrix(matrix) || liquid_based_unit_norm(unit_norm)) {
    alternative_liquid_factors()
  } else if (is_solid_matrix(matrix) || mass_based_unit_norm(unit_norm)) {
    alternative_mass_factors()
  } else {
    return(NA_character_)
  }

  applied_conv <- mean_raw * applied_factor
  if (in_range(applied_conv)) {
    return(NA_character_)
  }

  notes <- character()
  for (label in names(alt)) {
    alt_factor <- alt[[label]]
    if (is.na(applied_factor) || alt_factor == applied_factor) {
      next
    }
    alt_conv <- mean_raw * alt_factor
    plausible_alt <- in_range(alt_conv) ||
      (alt_conv < applied_conv / 100 && alt_conv < EXTREME_CONVERTED)
    if (plausible_alt) {
      notes <- c(
        notes,
        paste0(
          "if ", label, " not ", labeled_unit_label(unit_norm),
          ", mean would be ", signif(alt_conv, 4), " ", unit_converted
        )
      )
    }
  }

  if (length(notes) == 0) {
    NA_character_
  } else {
    paste(notes, collapse = "; ")
  }
}

evaluate_conversion_flags <- function(
  mean_raw,
  max_raw,
  unit_raw,
  matrix,
  conversion_factor,
  mean_converted,
  max_converted,
  unit_converted
) {
  flags <- character()
  reasons <- character()
  unit_norm <- normalize_unit_string(unit_raw)
  conv_vals <- c(mean_converted, max_converted)
  conv_vals <- conv_vals[!is.na(conv_vals)]

  if (is.na(conversion_factor)) {
    flags <- c(flags, "unparseable_unit")
    reasons <- c(reasons, "Unit string has no matching conversion factor")
    return(list(
      flag_codes = paste(flags, collapse = ";"),
      flag_reasons = paste(reasons, collapse = "; "),
      suggested_unit_note = NA_character_
    ))
  }

  if (length(conv_vals) > 0 && any(conv_vals > EXTREME_CONVERTED)) {
    flags <- c(flags, "extreme_converted")
    reasons <- c(
      reasons,
      paste0(
        "Converted concentration exceeds ", format_sci(EXTREME_CONVERTED),
        " ng/g or ng/L"
      )
    )
  }

  high_thresh <- if (is_liquid_matrix(matrix)) {
    HIGH_CONVERTED_LIQUID
  } else {
    HIGH_CONVERTED_SOLID
  }
  if (length(conv_vals) > 0 && any(conv_vals > high_thresh)) {
    flags <- c(flags, "high_converted")
    reasons <- c(
      reasons,
      paste0(
        "Converted concentration exceeds ", format_sci(high_thresh),
        " (review band)"
      )
    )
  }

  if (grepl("^g/kg", unit_norm) && !is.na(mean_raw) && mean_raw > IMPLAUSIBLE_G_PER_KG) {
    flags <- c(flags, "implausible_g_per_kg")
    reasons <- c(
      reasons,
      paste0("Raw value > ", IMPLAUSIBLE_G_PER_KG, " with g/kg unit (physically implausible)")
    )
  }

  if (
    conversion_factor == 1e6 &&
      !is.na(mean_raw) &&
      mean_raw > LARGE_VALUE_HIGH_FACTOR
  ) {
    flags <- c(flags, "large_value_high_factor")
    reasons <- c(
      reasons,
      paste0(
        "Raw value > ", LARGE_VALUE_HIGH_FACTOR,
        " combined with g/kg or mg/L scale factor (1e6)"
      )
    )
  }

  alt_note <- best_alternative_note(
    mean_raw,
    unit_norm,
    matrix,
    conversion_factor,
    unit_converted
  )
  if (!is.na(alt_note)) {
    flags <- c(flags, "alt_unit_more_plausible")
    reasons <- c(
      reasons,
      "Another unit scale would place the converted value in the plausible range"
    )
  }

  if (length(flags) == 0) {
    return(list(
      flag_codes = NA_character_,
      flag_reasons = NA_character_,
      suggested_unit_note = NA_character_
    ))
  }

  list(
    flag_codes = paste(unique(flags), collapse = ";"),
    flag_reasons = paste(reasons, collapse = "; "),
    suggested_unit_note = alt_note
  )
}

flag_environmental_concentrations <- function(
  environmental,
  from_cleaned = FALSE
) {
  audited <- environmental |>
    dplyr::filter(!is.na(.data$mean_concentration))

  if (from_cleaned) {
    if (!"matrix" %in% names(audited)) {
      audited <- audited |>
        dplyr::mutate(
          matrix = assign_environmental_matrix(.data$sample_type, .data$reference_number)
        )
    }
    audited <- audited |>
      dplyr::mutate(
        unit_raw = dplyr::if_else(
          is.na(.data$previous_unit) | .data$previous_unit == "",
          .data$concentration_unit,
          .data$previous_unit
        ),
        conversion_factor = .data$recalculated_factor,
        unit_converted = .data$concentration_unit,
        mean_converted = .data$mean_concentration,
        max_converted = .data$max_concentration,
        mean_raw = .data$mean_concentration / .data$recalculated_factor,
        max_raw = dplyr::if_else(
          is.na(.data$max_concentration),
          NA_real_,
          .data$max_concentration / .data$recalculated_factor
        )
      )
  } else {
    audited <- audited |>
      dplyr::mutate(
        matrix = assign_environmental_matrix(.data$sample_type, .data$reference_number),
        unit_norm = normalize_unit_string(.data$concentration_unit),
        is_dw = unit_is_dry_weight(.data$unit_norm),
        conversion_factor = unit_conversion_factor(.data$concentration_unit),
        unit_converted = target_concentration_unit(.data$matrix, .data$is_dw),
        mean_converted = .data$mean_concentration * .data$conversion_factor,
        max_converted = .data$max_concentration * .data$conversion_factor,
        unit_raw = .data$concentration_unit,
        mean_raw = .data$mean_concentration,
        max_raw = .data$max_concentration
      )
  }

  flag_results <- lapply(seq_len(nrow(audited)), function(i) {
    evaluate_conversion_flags(
      mean_raw = audited$mean_raw[i],
      max_raw = audited$max_raw[i],
      unit_raw = audited$unit_raw[i],
      matrix = audited$matrix[i],
      conversion_factor = audited$conversion_factor[i],
      mean_converted = audited$mean_converted[i],
      max_converted = audited$max_converted[i],
      unit_converted = audited$unit_converted[i]
    )
  })

  audited <- audited |>
    dplyr::mutate(
      flag_codes = vapply(flag_results, function(x) x$flag_codes, character(1)),
      flag_reasons = vapply(flag_results, function(x) x$flag_reasons, character(1)),
      suggested_unit_note = vapply(
        flag_results,
        function(x) x$suggested_unit_note,
        character(1)
      )
    )

  suspicious <- audited |>
    dplyr::filter(!is.na(.data$flag_codes)) |>
    dplyr::arrange(dplyr::desc(.data$mean_converted))

  list(audited = audited, suspicious = suspicious)
}

print_flag_audit_summary <- function(audited, suspicious) {
  cat("Rows audited:", nrow(audited), "\n")
  cat("Rows flagged:", nrow(suspicious), "\n\n")

  if (nrow(suspicious) == 0) {
    return(invisible(NULL))
  }

  cat("Top 10 by converted mean:\n")
  print(suspicious[1:min(10, nrow(suspicious)), c(
    "reference_number",
    "antibiotic",
    "unit_raw",
    "mean_raw",
    "mean_converted",
    "flag_codes"
  )])

  cat("\nFlag counts:\n")
  print(sort(
    table(unlist(strsplit(suspicious$flag_codes, ";"))),
    decreasing = TRUE
  ))

  cat("\nReferences with most flags:\n")
  print(head(sort(table(suspicious$reference_number), decreasing = TRUE), 15))
}
