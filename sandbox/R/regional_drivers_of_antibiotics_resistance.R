# ============================================================================
# REGIONAL DRIVERS OF ANTIBIOTIC RESISTANCE - MAIN PLOT
# Requires: carss_cleaned.csv, yearbook_cleaned.csv, qu_cleaned.csv
# ============================================================================

# Load libraries
library(dplyr)
library(ggplot2)
library(readr)

# Load your cleaned data
carss <- read_csv("data/cleaned/carss_cleaned.csv")
yearbook <- read_csv("data/cleaned/yearbook_cleaned.csv")
qu <- read_csv("data/cleaned/qu_cleaned.csv")

# Aggregate CARSS data to provincial level (average resistance across all bacteria)
resistance_data <- carss %>%
  group_by(province, antibiotic) %>%
  summarise(avg_resistance = mean(resistant_pc, na.rm = TRUE),
            .groups = "drop")


#
# yb_vars <- data.frame(var = colnames(yearbook)) %>%
#   filter(grepl("Urban_Population_10000_persons", var))
#
# Total_Population_10000_persons
# Urban_Population_10000_persons

province_stats <- qu %>%
  left_join(yearbook, by = "province") %>%
  # Calculate key derived variables
  mutate(
    # Livestock density (approximate heads per km²)
    livestock_density = (`Hogs_10000_heads_year-end` + Cattle_and_Buffaloes_10000_heads) /
      (Cultivated_Land_1000_ha * 10),
    # Hospital density (hospitals per 10,000 people)
    hospital_density = (Total_Health_Care_Institutions / Total_Population_10000_persons) * 10000,
    # Urbanization rate (%)
    urbanization_rate = (Urban_Population_10000_persons / Total_Population_10000_persons) * 100,
    # Classify dominant driver based on thresholds
    dominant_driver = case_when(
      livestock_density > quantile(livestock_density, 0.6, na.rm = TRUE) &
        NBS_region %in% c("Northern", "Northeastern", "Central") ~ "Agriculture",
      hospital_density > quantile(hospital_density, 0.6, na.rm = TRUE) &
        NBS_region %in% c("Eastern", "Southern") ~ "Hospitals",
      TRUE ~ "Mixed"
    )
  )


# Merge all datasets
analysis_data <- resistance_data %>%
  left_join(province_stats, by = "province")




# Create the main plot
p <- ggplot(analysis_data %>% filter(antibiotic == "Tetracycline"), aes(x = hospital_density, y = avg_resistance,
                               color = dominant_driver, size = livestock_density)) +
  # Points
  geom_point(alpha = 0.7, stroke = 1.5) +
  # Trend lines by region
  geom_smooth(aes(group = dominant_driver), method = "lm", se = TRUE, alpha = 0.2) +
  # Labels
  geom_text(aes(label = province), hjust = -0.1, vjust = 0, size = 3) +
  # Theme and labels
  scale_color_manual(values = c("Agriculture" = "#E31A1C", "Hospitals" = "#1F78B4", "Mixed" = "#666666")) +
  scale_size_continuous(range = c(2, 8)) +
  labs(
    title = "Regional Drivers of Antibiotic Resistance in China",
    subtitle = "North: Agriculture-driven | South: Hospital-driven | West: Mixed",
    x = "Hospital Density (per 10,000 population)",
    y = "Average Antibiotic Resistance (%)",
    color = "Dominant Driver",
    size = "Livestock Density\n(heads/km²)"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(size = 16, face = "bold"),
    plot.subtitle = element_text(size = 12),
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

p
