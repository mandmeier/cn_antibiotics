
#### Plot data by Province ####


library(sf)
library(cowplot)  # for get_legend()


#### mean ntibiotics concentration by province Zhang_2022 ####

zhang <- read_excel("antibiotics_data/clean_data/clean_data.xlsx", sheet = "Zhang_2022")

zhang <- zhang %>%
  mutate(ABX_conc_mean = ifelse(grepl("ND", ABX_conc_mean), NA, ABX_conc_mean)) %>%
  mutate(ABX_conc_mean = ifelse(grepl("E-2", ABX_conc_mean), as.numeric(sub("E-.*$", "", ABX_conc_mean))/100, ABX_conc_mean)) %>%
  mutate(ABX_conc_mean = ifelse(grepl("E-3", ABX_conc_mean), as.numeric(sub("E-.*$", "", ABX_conc_mean))/1000, ABX_conc_mean)) %>%
  mutate(ABX_conc_mean = as.numeric(ABX_conc_mean)) %>%
  mutate(unit = "μg/kg") %>%
  # fill ABX subcat where only ABX cat is measured
  mutate(ABX_subcat = ifelse(is.na(ABX_subcat) & !is.na(ABX_cat), paste(ABX_cat, "(total)"), ABX_subcat)) %>%
  # fixes
  mutate(ABX_subcat = ifelse(ABX_subcat == "Cephalexin", "Cefalexin", ABX_subcat))

means_by_province <- zhang %>%
  select(province, sample_type, ABX_subcat, ABX_conc_mean, unit) %>%
  filter(!is.na(ABX_subcat)) %>%
  filter(!is.na(ABX_conc_mean)) %>%
  filter(!is.na(province)) %>%
  unique() %>%
  group_by(province, sample_type, ABX_subcat) %>%
  mutate(mean_conc = mean(ABX_conc_mean, na.rm = TRUE), .after = "ABX_subcat") %>%
  mutate(se_conc = sd(ABX_conc_mean, na.rm = TRUE) / sqrt(sum(!is.na(ABX_conc_mean))), .after = "mean_conc") %>%
  select(-ABX_conc_mean) %>%
  unique()


# interesting antibiotics, Amoxicillin and all beta lactams

blactams <- zhang %>%
  filter(grepl("lactam", ABX_cat)) %>%
  filter(!is.na(ABX_conc_mean)) %>%
  select(ABX_subcat) %>%
  unique() %>%
  pull()

dat <- means_by_province %>%
  #filter(sample_type == "sediments") %>%
  filter(ABX_subcat %in% c("Amoxicillin", blactams))

#
# p <- ggplot(dat, aes(x = province, y = mean_conc, group = ABX_subcat)) +
#   geom_errorbar(aes(ymin = mean_conc - se_conc, ymax = mean_conc + se_conc), width = 0.5, position = position_dodge(width = 0.05)) +
#   geom_line(aes(color = ABX_subcat)) +
#   geom_point(aes(color = ABX_subcat)) +
#   facet_wrap(~ sample_type, ncol = 1, scales = "free_y") +
#   #geom_col(position = position_dodge(width = 0.8), width = 0.7) +
#   #facet_grid(province ~ sample_type, scales = "free") +
#   labs(x = "", y = expression("Mean concentration ("*mu*"g/kg)"), color = "Antibiotic") +
#   theme_bw() +
#   theme(
#     strip.text = element_text(face = "bold"),
#     axis.text.x = element_text(angle = 45, hjust = 1)
#   )
#
# p
#
#
#
#
# p2 <- ggplot(dat, aes(x = province, y = mean_conc, group = sample_type)) +
#   geom_errorbar(aes(ymin = mean_conc - se_conc, ymax = mean_conc + se_conc), width = 0.5, position = position_dodge(width = 0.05)) +
#   geom_line(aes(color = sample_type)) +
#   geom_point(aes(color = sample_type)) +
#   facet_wrap(~ ABX_subcat, ncol = 1, scales = "free_y") +
#   #geom_col(position = position_dodge(width = 0.8), width = 0.7) +
#   #facet_grid(province ~ sample_type, scales = "free") +
#   labs(x = "", y = expression("Mean concentration ("*mu*"g/kg)"), color = "Sample Type") +
#   theme_bw() +
#   theme(
#     strip.text = element_text(face = "bold"),
#     axis.text.x = element_text(angle = 45, hjust = 1)
#   )
#
# p2




china_prov <- sf::st_read("antibiotics_data/cn_shp/cn.shp")

china_prov_names <- china_prov %>%
  mutate(name = sub(" .*", "", name))




map_df <- china_prov_names %>%
  select(name, geometry) %>%
  left_join(ABX_dat, by = c("name" = "province")) %>%
  #filter(sample_type == "sediments") %>%
  # log 10 transform values to show better on map
  mutate(mean_conc_log = ifelse(!is.na(mean_concentration) & mean_concentration > 0, log10(mean_concentration), NA)) %>%
  filter(!is.na(mean_concentration))




# 1️⃣ Get all unique values
all_names <- sort(unique(china_prov_names$name))
all_names <- all_names[!is.na(all_names)]
all_sample_types <- sort(unique(map_df$sample_type))
all_sample_types <- all_sample_types[!is.na(all_sample_types)]
all_abx <- sort(unique(map_df$ABX))
all_abx <- all_abx[!is.na(all_abx)]


length(all_names) * length(all_sample_types) * length(all_abx)

# 2️⃣ Create all combinations
all_combinations <- expand_grid(
  name = all_names,
  sample_type = all_sample_types,
  ABX = all_abx
)


# 3️⃣ Join geometries from original map_df (by 'name')
# We'll take only one geometry per province
province_geom <- china_prov_names %>% select(name, geometry) %>% distinct(name, .keep_all = TRUE)


map_df_complete <- all_combinations %>%
  left_join(map_df %>% filter(ABX %in% c("Erythromycin", "Tetracycline")) %>% filter(sample_type == "municipal sludge"), by = c("name", "sample_type", "ABX")) %>%  # join existing data
  select(-geometry) %>%
  left_join(province_geom, by = "name") %>%  # ensure geometry exists
  mutate(
    # If mean_conc_log is NA, set fill_value to NA (will map to grey)
    fill_value = mean_conc_log
  ) %>%
  # remove measurements where ALL provinces are NA
  group_by(sample_type, ABX) %>%
  filter(any(!is.na(mean_conc_log))) %>%
  ungroup() %>%
  st_as_sf()  # convert back to sf if needed







# 1️⃣ Main map WITHOUT legend
map_plot <- ggplot(map_df_complete) +
  geom_sf(aes(fill = fill_value), color = "grey50", size = 0.2) +
  facet_grid(sample_type ~ ABX) +
  scale_fill_viridis_c(
    option = "D",
    na.value = "grey90",  # light grey for missing data
    name = expression("Log10 mean concentration ("*mu*"g/kg)")
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )

# Display main map
map_plot





map_df_complete_res <- all_combinations %>%
  select(-sample_type) %>%
  unique() %>%
  left_join(map_df %>% select(-mean_concentration, -sample_type) %>% unique() %>% filter(ABX %in% c("Erythromycin", "Tetracycline")), by = c("name", "ABX")) %>%  # join existing data
  select(-geometry) %>%
  left_join(province_geom, by = "name") %>%  # ensure geometry exists
  mutate(
    # If resistant_pc is NA, set fill_value to NA (will map to grey)
    fill_value = resistant_pc
  ) %>%
  # remove measurements where ALL provinces are NA
  group_by(ABX) %>%
  filter(any(!is.na(resistant_pc))) %>%
  ungroup() %>%
  st_as_sf()  # convert back to sf if needed





# 1️⃣ Main map WITHOUT legend
map_plot_res <- ggplot(map_df_complete_res) +
  geom_sf(aes(fill = fill_value), color = "grey50", size = 0.2) +
  facet_grid( ~ ABX) +
  scale_fill_viridis_c(
    option = "D",
    na.value = "grey90",  # light grey for missing data
    name = "resistant strains [%]"
  ) +
  theme_minimal() +
  theme(
    strip.text = element_text(face = "bold"),
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )

# Display main map
map_plot_res












#
# # 2️⃣ Extract legend only
# legend_plot <- get_legend(
#   ggplot(map_df_complete) +
#     geom_sf(aes(fill = fill_value), color = "grey50", size = 0.2) +
#     scale_fill_viridis_c(
#       option = "C",
#       na.value = "grey90",
#       name = expression("Log10 mean concentration ("*mu*"g/kg)")
#     ) +
#     theme_minimal() +
#     theme(
#       legend.position = "right",
#       legend.title = element_text(face = "bold"),
#       legend.text = element_text(size = 10)
#     )
# )
#
#
# legend_only_plot <- ggdraw() +
#   draw_grob(legend_plot)
#
# # Display legend only
# legend_only_plot



#
# #### Plot data over time ####
#
#
#
# demographic_years <- read_excel("antibiotics_data/2000-2024 data china.xlsx", sheet = "by_year")
#
#
#
# antibiotics_years <- read_excel("antibiotics_data/presence of antibiotics.xlsx", sheet = "by_year")
#
#
# yearly_data <- demographic_years %>%
#   left_join(antibiotics_years, by = "year")
#
#
#
# aby <- antibiotics_years %>%
#   select(year, antibiotics, matrix, unit, conc_lower, conc_upper) %>%
#   mutate(label = paste0(antibiotics," ", matrix," (", unit, ")"))
#
#
# # define a small value
# epsilon <- min(aby$conc_lower[aby$conc_lower > 0], na.rm = TRUE) / 10
#
# df_plot <- aby %>%
#   mutate(
#     conc_lower_log = ifelse(conc_lower <= 0, epsilon, conc_lower),
#     conc_upper_log = ifelse(conc_upper <= 0, epsilon, conc_upper)
#   ) %>%
#   # complete in case a year is missing
#   complete(
#     year = seq(min(year), max(year), by = 1)
#   ) %>%
#   mutate(
#     year = factor(year, levels = sort(unique(year)))
#   ) %>%
#   mutate(
#     label_y = sqrt(conc_lower_log * conc_upper_log)
#   )
#
#
# dodge <- position_dodge(width = 0.8)
#
# ggplot(df_plot, aes(x = year, group = label)) +
#   geom_errorbar(
#     aes(
#       ymin = conc_lower_log,
#       ymax = conc_upper_log
#     ),
#     color = "#333",
#     width = 0.25,
#     position = dodge,
#     na.rm = TRUE,
#     linewidth = 0.6
#   ) +
#   geom_text(
#     aes(
#       x = as.numeric(year) - 0.15,  # shift left
#       y = 0.01,
#       label = label
#     ),
#     color = "darkblue",
#     position = dodge,
#     angle = 90,
#     vjust = 0,   # vertical anchor at center
#     hjust = 0,   # horizontal anchor at center
#     size = 3,
#     na.rm = TRUE
#   ) +
#   scale_y_log10() +
#   labs(
#     x = "Year",
#     y = "Concentration (log10 scale)"
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
#   ) +
#   coord_cartesian(clip = "off") +
#   theme(plot.margin = margin(t = 20))
#
#
#
#
#
#
# df_plot <- demographic_years %>%
#   mutate(
#     year = factor(year, levels = sort(unique(year)))
#   ) %>%
#   pivot_longer(`Total Population (10,000 persons)`:`Production of Antibiotics in China (tonnes)`, names_to = "metric") %>%
#   filter(!is.na(value)) %>%
#   filter(metric != "Irrigated Area of Cultivated Land (1,000 hectares)") %>%
#   # remove non numeric values
#   mutate(
#     value_num = as.numeric(value)  # non-numeric values become NA
#   ) %>%
#   filter(!is.na(value_num)) %>%     # keep only numeric rows
#   select(-value) %>%                 # optionally remove old column
#   rename(value = value_num)
#
# demo_plot <- ggplot(df_plot, aes(x = year, y = value, group = metric)) +
#   geom_line() +
#   facet_wrap(~ metric, ncol = 4, scales = "free_y") +
#   scale_y_continuous(
#     breaks = function(x) pretty(x, n = 5)  # 5 nicely spaced ticks per facet
#   ) +
#   theme_minimal() +
#   theme(
#     axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1)
#   ) +
#   labs(
#     x = NULL,  # remove x-axis label
#     y = NULL   # remove y-axis label
#   )
#
# demo_plot
#

