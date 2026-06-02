# Shared environmental sample matrix and concentration unit conversion helpers.

normalize_unit_string <- function(unit) {
  unit |>
    str_to_lower() |>
    str_replace_all("μ", "µ") |>
    str_replace_all("^ug", "µg")
}

unit_is_dry_weight <- function(unit_norm) {
  grepl("dw", unit_norm, fixed = TRUE)
}

unit_conversion_factor <- function(unit) {
  unit_norm <- normalize_unit_string(unit)
  dplyr::case_when(
    grepl("^g/kg", unit_norm) ~ 1e6,
    grepl("^mg/kg", unit_norm) ~ 1e3,
    grepl("^(ug|µg)/kg", unit_norm) ~ 1,
    grepl("^ng/g", unit_norm) ~ 1,
    grepl("^ng/kg", unit_norm) ~ 0.001,
    grepl("^(ug|µg)/g", unit_norm) ~ 1e3,
    grepl("^mg/l", unit_norm) ~ 1e6,
    grepl("^(ug|µg)/l", unit_norm) ~ 1e3,
    grepl("^ng/l", unit_norm) ~ 1,
    TRUE ~ NA_real_
  )
}

target_concentration_unit <- function(matrix, is_dw) {
  dplyr::case_when(
    matrix %in% c("soil", "sediment", "sludge") & is_dw ~ "ng/g dw",
    matrix %in% c("soil", "sediment", "sludge") ~ "ng/g",
    matrix %in% c(
      "surface water", "wastewater influent", "wastewater effluent"
    ) ~ "ng/L",
    TRUE ~ NA_character_
  )
}

assign_environmental_matrix <- function(sample_type, reference_number) {
  matrix <- dplyr::case_when(
    sample_type %in% c("agricultural soil", "soil") ~ "soil",
    sample_type %in% c(
      "sediment",
      "surface sediment",
      "river sediment",
      "lake sediment",
      "coastal/river sediment",
      "surface water sediment",
      "aquaculture pond sediment",
      "mariculture sediment",
      "river water (suspended matter fraction)",
      "suspended particulate matter (river)"
    ) ~ "sediment",
    sample_type %in% c(
      "surface water",
      "river water",
      "coastal water",
      "inland lake water",
      "aquaculture pond water",
      "mariculture water",
      "groundwater"
    ) ~ "surface water",
    sample_type %in% c(
      "municipal sludge",
      "industrial sludge",
      "MSW incineration sludge",
      "fermentation residue"
    ) ~ "sludge",
    sample_type %in% c(
      "municipal wastewater influent",
      "urban wastewater (influent)",
      "wastewater influent",
      "municipal wastewater (urban sewer)",
      "MSW incineration leachate"
    ) ~ "wastewater influent",
    sample_type %in% c(
      "municipal wastewater effluent",
      "urban wastewater (effluent)",
      "MSW incineration leachate (NF treated)",
      "MSW incineration leachate (treated effluent)",
      "livestock wastewater",
      "hospital wastewater",
      "pharmaceutical wastewater",
      "aquaculture wastewater"
    ) ~ "wastewater effluent",
    TRUE ~ NA_character_
  )
  dplyr::if_else(reference_number == "N79", "surface water", matrix)
}

is_solid_matrix <- function(matrix) {
  matrix %in% c("soil", "sediment", "sludge")
}

is_liquid_matrix <- function(matrix) {
  matrix %in% c("surface water", "wastewater influent", "wastewater effluent")
}

mass_based_unit_norm <- function(unit_norm) {
  grepl("/kg$|/g$", unit_norm) | grepl("^ng/g", unit_norm)
}

liquid_based_unit_norm <- function(unit_norm) {
  grepl("/l$", unit_norm)
}

alternative_mass_factors <- function() {
  c(
    "g/kg" = 1e6,
    "mg/kg" = 1e3,
    "µg/kg" = 1,
    "ng/g" = 1
  )
}

alternative_liquid_factors <- function() {
  c(
    "mg/L" = 1e6,
    "µg/L" = 1e3,
    "ng/L" = 1
  )
}

convert_concentration_values <- function(
  mean_concentration,
  max_concentration,
  concentration_unit,
  matrix
) {
  unit_norm <- normalize_unit_string(concentration_unit)
  is_dw <- unit_is_dry_weight(unit_norm)
  factor <- unit_conversion_factor(concentration_unit)

  list(
    unit_raw = concentration_unit,
    unit_norm = unit_norm,
    is_dw = is_dw,
    conversion_factor = factor,
    unit_converted = target_concentration_unit(matrix, is_dw),
    mean_converted = mean_concentration * factor,
    max_converted = max_concentration * factor
  )
}
