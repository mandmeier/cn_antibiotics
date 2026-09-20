# Fig. 3C — Environmental antibiotics coverage by province
#
# Choropleth of n distinct antibiotics per province from env_province.csv.
# Same eastern/coastal skew as Fig. 2C (measurement counts), but for compound
# richness. Province labels show the count.
#
# RStudio: WD = cn_antibiotics/; requires R/04 output.
# Geometry: data/raw/cn_shp/cn.shp (deposit shapefile).

library(dplyr)
library(ggplot2)
library(readr)
library(sf)
library(scales)

conflicts_prefer(dplyr::filter)

# ---- paths ------------------------------------------------------------------

out_dir <- "figures"
dir.create(out_dir, showWarnings = FALSE)

env_path <- "data/output/primary/env_province.csv"
shp_path <- "data/raw/cn_shp/cn.shp"

PAL_ENV <- c("#E7F1EA", "#8FBF9A", "#2F6B45")
COL_INK <- "#1F2933"
COL_BORDER <- "#4A5560"
COL_NA <- "#E5E7EB"

# ---- helpers ----------------------------------------------------------------

log_colour <- function(n, n_min, n_max, palette) {
  n <- as.numeric(n)
  if (!is.finite(n_min) || !is.finite(n_max) || n_min <= 0 || n_max <= 0) {
    return(rep(palette[[1]], length(n)))
  }
  if (n_min == n_max) {
    return(rep(scales::colour_ramp(palette)(0.65), length(n)))
  }
  t <- (log10(pmax(n, n_min)) - log10(n_min)) / (log10(n_max) - log10(n_min))
  t <- pmin(pmax(t, 0), 1)
  scales::colour_ramp(palette)(t)
}

luma <- function(hex) {
  rgb <- col2rgb(hex) / 255
  as.numeric(0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ])
}

load_china_provinces <- function(path) {
  sf::st_read(path, quiet = TRUE) %>%
    mutate(name = sub(" .*", "", name)) %>%
    rename(province = name) %>%
    mutate(
      province = case_when(
        province == "Inner" ~ "Inner Mongolia",
        province == "Hong" ~ "Hong Kong",
        TRUE ~ province
      )
    ) %>%
    select(province, geometry)
}

# ---- counts -----------------------------------------------------------------

env_province <- read_csv(env_path, show_col_types = FALSE)

env_counts <- env_province %>%
  distinct(province, antibiotic) %>%
  count(province, name = "n_abx") %>%
  arrange(desc(n_abx))

env_counts
stopifnot(nrow(env_counts) == 31L)

# ---- map --------------------------------------------------------------------

china <- load_china_provinces(shp_path)
dat <- china %>% left_join(env_counts, by = "province")

n_min <- min(env_counts$n_abx, na.rm = TRUE)
n_max <- max(env_counts$n_abx, na.rm = TRUE)

dat <- dat %>%
  mutate(
    fill_col = if_else(
      is.na(n_abx),
      COL_NA,
      log_colour(n_abx, n_min, n_max, PAL_ENV)
    ),
    label_col = if_else(
      is.na(n_abx),
      NA_character_,
      if_else(luma(fill_col) < 0.55, "white", COL_INK)
    )
  )

dat_proj <- st_transform(dat, 3857)
label_sf <- dat_proj %>%
  filter(!is.na(n_abx)) %>%
  st_point_on_surface() %>%
  st_transform(4326)

fig03c <- ggplot(dat) +
  geom_sf(
    aes(geometry = geometry),
    fill = dat$fill_col,
    colour = COL_BORDER,
    linewidth = 0.2
  ) +
  geom_sf_text(
    data = label_sf,
    aes(geometry = geometry, label = n_abx, colour = label_col),
    size = 2.4,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_colour_identity() +
  coord_sf(expand = FALSE) +
  labs(
    title = "Environmental antibiotics coverage by province",
    subtitle = "n distinct antibiotics in primary/env_province.csv"
  ) +
  theme_void(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", colour = COL_INK, hjust = 0),
    plot.subtitle = element_text(colour = "#5A6A76", hjust = 0),
    plot.margin = margin(8, 8, 8, 8),
    plot.background = element_rect(fill = "white", colour = NA),
    panel.background = element_rect(fill = "white", colour = NA)
  )

fig03c

# ---- save -------------------------------------------------------------------

ggsave(
  file.path(out_dir, "fig03c_env_antibiotics_map.pdf"),
  fig03c, width = 8.5, height = 7.2, units = "in", bg = "white"
)
ggsave(
  file.path(out_dir, "fig03c_env_antibiotics_map.png"),
  fig03c, width = 8.5, height = 7.2, units = "in", dpi = 300, bg = "white"
)

message("Wrote figures/fig03c_env_antibiotics_map.pdf/.png")
message(
  "n_abx range: ", n_min, "-", n_max,
  " | max province: ", env_counts$province[[1]]
)
