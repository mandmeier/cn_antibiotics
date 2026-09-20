# Fig. 2C — Geographical distribution of environmental measurements
#
# Choropleth of n measurements per province from supporting/env_records.csv
# (after the QC filters in Fig. 2B). Log colour scale so coastal highs do not
# wash out western lows.
#
# RStudio: WD = cn_antibiotics/; requires R/01 output.
# Geometry: data/raw/cn_shp/cn.shp (deposit shapefile).

library(dplyr)
library(ggplot2)
library(readr)
library(sf)
library(scales)

conflicts_prefer(dplyr::filter)

source("R/utils/plot_on_china_map.R")

# ---- paths ------------------------------------------------------------------

out_dir <- "figures"
dir.create(out_dir, showWarnings = FALSE)

env_path <- "data/output/supporting/env_records.csv"
shp_path <- "data/raw/cn_shp/cn.shp"

# ---- coverage counts --------------------------------------------------------

env_records <- read_csv(env_path, show_col_types = FALSE)

coverage <- env_records %>%
  group_by(province) %>%
  summarise(
    n_measurements = n(),
    n_compounds = n_distinct(antibiotic),
    .groups = "drop"
  ) %>%
  arrange(desc(n_measurements))

coverage
coverage %>% arrange(n_measurements) %>% slice_head(n = 5)

# ---- map --------------------------------------------------------------------

fig02c <- plot_on_china_map(
  plot_data = coverage,
  plot_variable = "n_measurements",
  legend_title = "n measurements",
  border_col = "#4A5560",
  color_pallette = "C",
  china_geometry_shapefile_path = shp_path
)

fig02c <- fig02c +
  scale_fill_viridis_c(
    option = "C",
    trans = "log10",
    na.value = "#E5E7EB",
    name = "n measurements",
    breaks = c(10, 30, 100, 300, 1000),
    labels = c("10", "30", "100", "300", "1,000"),
    guide = guide_colorbar(
      title.position = "top",
      barwidth = unit(12, "lines"),
      barheight = unit(0.55, "lines")
    )
  ) +
  coord_sf(expand = FALSE) +
  labs(
    title = "Sample density | geographical skew",
    subtitle = "n measurements in supporting/env_records.csv (31 study provinces)"
  ) +
  theme(
    legend.position = "bottom",
    plot.title = element_text(face = "bold", colour = "#1F2933"),
    plot.subtitle = element_text(colour = "#5A6A76"),
    plot.margin = margin(6, 10, 6, 6)
  )

fig02c

# ---- save -------------------------------------------------------------------

ggsave(
  file.path(out_dir, "fig02c_env_measurement_map.pdf"),
  fig02c, width = 9.5, height = 8.2, units = "in", bg = "white"
)
ggsave(
  file.path(out_dir, "fig02c_env_measurement_map.png"),
  fig02c, width = 9.5, height = 8.2, units = "in", dpi = 300, bg = "white"
)

message("Wrote figures/fig02c_env_measurement_map.pdf/.png")
message(
  "n_measurements range: ",
  min(coverage$n_measurements), "-", max(coverage$n_measurements),
  " | max province: ", coverage$province[[1]]
)
