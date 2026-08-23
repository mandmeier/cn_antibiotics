# Constants and path helpers for cluster-pathways Module 1.2 analysis.

CP_BASE_DIR <- "data/analysis_ready/cluster_pathways"

CP_PROVINCES <- c(
  "Beijing", "Tianjin", "Hebei", "Shanxi", "Inner Mongolia", "Liaoning", "Jilin",
  "Heilongjiang", "Shanghai", "Jiangsu", "Zhejiang", "Anhui", "Fujian", "Jiangxi",
  "Shandong", "Henan", "Hubei", "Hunan", "Guangdong", "Guangxi", "Hainan",
  "Chongqing", "Sichuan", "Guizhou", "Yunnan", "Tibet", "Shaanxi", "Gansu",
  "Qinghai", "Ningxia", "Xinjiang"
)

CP_CLUSTER_NAMES <- c(
  "1" = "Eastern production-infrastructure belt",
  "2" = "Urban-industrial wastewater core",
  "3" = "Inland surveillance-gap belt"
)

CP_SENTINEL_ENDPOINTS <- c(
  "A. baumannii | Levofloxacin",
  "E. coli | Ciprofloxacin",
  "E. coli | Levofloxacin",
  "E. coli | Trimethoprim/Sulfamethoxazole",
  "K. pneumoniae | Ciprofloxacin",
  "K. pneumoniae | Levofloxacin",
  "P. aeruginosa | Ciprofloxacin",
  "S. aureus | Erythromycin"
)

CP_DRIVER_COMPOSITES <- c(
  "WastewaterHealthIndex",
  "LivestockAquacultureIndex",
  "EconomicScaleIndex"
)

CP_INTERPRETABLE_PATHWAY_DRIVERS <- c(
  "hospitals_count_2024",
  "hospital_beds_2024",
  "sewage_disposal_capacity_2024",
  "sewage_pipe_length_2024",
  "urban_env_infra_investment_2024",
  "urban_share_2024",
  "hogs_year_end_2024",
  "pork_output_2024",
  "milk_output_2024",
  "large_animals_year_end_2024",
  "total_aquatic_products_2024",
  "freshwater_aquatic_products_2024",
  "chemical_fertilizers_2024",
  "population_2024",
  "grp_2024",
  "grp_per_capita_2024",
  CP_DRIVER_COMPOSITES
)

# Pathway filter thresholds (analysis-ready shortlist).
CP_MIN_PROVINCES <- 10L
CP_MIN_LOO_SIGN_STABILITY <- 0.95

CP_ENV_MATRICES <- c(
  "soil", "sediment", "sludge", "surface water",
  "wastewater influent", "wastewater effluent"
)

CP_EXCLUDED_PATHWAY_FILTER_MATRICES <- c("wastewater influent", "wastewater effluent")
CP_FIGURE3_MATRICES <- c("surface water", "soil", "sludge", "sediment")
CP_PATHWAY_FILTER_MATRICES <- CP_FIGURE3_MATRICES
CP_EXCLUDED_PATHWAY_ENV_CLASSES <- c("other", "multiple classes")
CP_SIMPLE_FIG3_ROWS_PER_MATRIX <- 3L

CP_PATHWAY_SCENARIO <- "all_years"

CP_MATRIX_LABELS <- c(
  "soil" = "Soil",
  "sludge" = "Sludge",
  "surface water" = "Surface water",
  "sediment" = "Sediment",
  "wastewater influent" = "Wastewater influent",
  "wastewater effluent" = "Wastewater effluent"
)

# Yearbook metric code -> interpretable driver alias (2024 values).
CP_DRIVER_METRIC_ALIASES <- c(
  "E25_08_Daily_Disposal_Capacity_of_City_Sewage" = "sewage_disposal_capacity_2024",
  "E25_08_Length_of_City_Sewage_Pipes" = "sewage_pipe_length_2024",
  "E22_01_Hospitals" = "hospitals_count_2024",
  "E22_06_Hospitals" = "hospitals_count_2024",
  "E22_06_Total" = "hospital_beds_2024",
  "E08_33_Investment_in_Urban_Environmental_Infrastructure" = "urban_env_infra_investment_2024",
  "E02_06_Urban_Population_Proportion" = "urban_share_2024",
  "E12_13_Hogs_year_end" = "hogs_year_end_2024",
  "E12_14_Pork" = "pork_output_2024",
  "E12_14_Milk" = "milk_output_2024",
  "E12_13_Large_Animals_year_end" = "large_animals_year_end_2024",
  "E12_15_Total_Aquatic_Products" = "total_aquatic_products_2024",
  "E12_15_Freshwater_Aquatic_Products" = "freshwater_aquatic_products_2024",
  "E12_05_Consumption_of_Chemical_Fertilizers" = "chemical_fertilizers_2024",
  "E02_05_Population_at_year_end" = "population_2024",
  "E03_09_Gross_Regional_Product" = "grp_2024",
  "E03_09_Per_Capita_Gross_Regional_Product" = "grp_per_capita_2024",
  "E20_07_Expenditure_on_R_and_D" = "rd_expenditure_2024"
)

CP_WASTEWATER_HEALTH_COMPONENTS <- c(
  "E25_08_Daily_Disposal_Capacity_of_City_Sewage",
  "E25_08_Length_of_City_Sewage_Pipes",
  "E08_33_Investment_in_Urban_Environmental_Infrastructure",
  "E02_06_Urban_Population_Proportion"
)

CP_LIVESTOCK_AQUACULTURE_COMPONENTS <- c(
  "E12_13_Hogs_year_end",
  "E12_14_Pork",
  "E12_14_Milk",
  "E12_13_Large_Animals_year_end",
  "E12_15_Total_Aquatic_Products",
  "E12_15_Freshwater_Aquatic_Products"
)

CP_ECONOMIC_SCALE_COMPONENTS <- c(
  "E03_09_Gross_Regional_Product",
  "E02_05_Population_at_year_end",
  "E20_07_Expenditure_on_R_and_D"
)

cp_step_dir <- function(step_name) {
  file.path(CP_BASE_DIR, step_name)
}

cp_read_csv <- function(path) {
  readr::read_csv(path, show_col_types = FALSE)
}

cp_write_csv <- function(data, path) {
  readr::write_csv(data, path)
}

cp_env_features_path <- function(scenario = CP_PATHWAY_SCENARIO) {
  file.path(
    cp_step_dir("07_environmental_features"),
    paste0("environmental_matrix_class_features_", scenario, ".csv")
  )
}

cp_filter_scenario <- function(df, scenario = CP_PATHWAY_SCENARIO) {
  if (is.null(df) || !nrow(df) || !("scenario" %in% names(df))) {
    return(df)
  }
  df[df$scenario == scenario, , drop = FALSE]
}

load_province_groups <- function() {
  groups <- cp_read_csv("data/cleaned/province_groups.csv") %>%
    dplyr::filter(.data$province %in% CP_PROVINCES) %>%
    dplyr::mutate(
      cluster_id = as.integer(.data$k3_groups),
      cluster_name = unname(CP_CLUSTER_NAMES[as.character(.data$cluster_id)])
    ) %>%
    dplyr::select(
      "province", "cluster_id", "cluster_name",
      "NBS_region", "RR_ranking"
    )
  groups
}

`%||%` <- function(x, y) {
  if (is.null(x)) y else x
}
