# Fig. 2A — Unique values before vs after harmonization
#
# Circle area scaled within each column (Before ↔ After).
# Green = environmental; blue = CARSS; purple = yearbook.
#
# RStudio: WD = cn_antibiotics/; run line by line or source this file.
# Requires curated outputs from R/01–R/07 plus raw inputs for "before".

library(dplyr)
library(ggplot2)
library(readr)
library(readxl)
library(stringr)
library(scales)

conflicts_prefer(dplyr::filter)

# ---- colours ----------------------------------------------------------------

COL_ENV <- "#2F6B45"
COL_RES <- "#2F5F7A"
COL_YB <- "#6B4C7A"
COL_INK <- "#1F2933"
COL_MUTED <- "#5A6A76"

out_dir <- "figures"
dir.create(out_dir, showWarnings = FALSE)

# ---- inputs -----------------------------------------------------------------

env_records <- read_csv(
  "data/output/supporting/env_records.csv",
  show_col_types = FALSE
)
resistance <- read_csv(
  "data/output/primary/resistance_province.csv",
  show_col_types = FALSE
)
yearbook <- read_csv(
  "data/output/primary/yearbook_province.csv",
  show_col_types = FALSE
)
carss_raw <- read_csv(
  "data/raw/resistance_data/carss_drug_resistance_full.csv",
  show_col_types = FALSE
)
yearbook_raw <- read_csv(
  "data/raw/yearbook_data/yearbook_data_combined.csv",
  show_col_types = FALSE
)

# ---- reconstruct "before" from raw environmental sources --------------------

zhang_raw <- read_excel(
  "data/raw/environmental_data/Zhang_2022.xls",
  sheet = "Records"
)

zhang_early <- zhang_raw %>%
  transmute(
    antibiotic = ABX_subcat,
    province = as.character(loc_l1),
    sample_type = sem_type,
    mean_concentration = as.numeric(ABX_conc_mean),
    max_concentration = as.numeric(ABX_conc_max),
    reference_number = as.character(pub_id),
    concentration_unit = "ng/g"
  ) %>%
  mutate(
    concentration_unit = case_when(
      reference_number == "223" ~ "mg/kg dw",
      reference_number == "113" ~ "µg/kg dw",
      TRUE ~ concentration_unit
    )
  )

supp_early <- read_csv(
  "data/raw/environmental_data/China_Environmental_Supplemental.csv",
  show_col_types = FALSE
) %>%
  transmute(
    antibiotic,
    province = as.character(province),
    sample_type,
    mean_concentration = as.numeric(mean_concentration),
    max_concentration = as.numeric(max_concentration),
    reference_number = as.character(reference_number),
    concentration_unit
  )

before_env <- bind_rows(zhang_early, supp_early) %>%
  filter(!is.na(mean_concentration) | !is.na(max_concentration)) %>%
  filter(!is.na(antibiotic), str_squish(antibiotic) != "") %>%
  mutate(
    antibiotic = str_squish(antibiotic),
    sample_type = str_squish(as.character(sample_type)),
    province = str_squish(province),
    province = if_else(province == "", NA_character_, province)
  )

carss_prov_before <- carss_raw %>% distinct(province) %>% pull(province)
carss_abx_before <- carss_raw %>%
  filter(province != "National") %>%
  distinct(antibiotic) %>%
  mutate(
    antibiotic = if_else(antibiotic == "Polymixin B", "Polymyxin B", antibiotic)
  ) %>%
  pull(antibiotic)

# ---- unique-value counts ----------------------------------------------------

n_env_abx_b <- n_distinct(before_env$antibiotic)
n_env_abx_a <- n_distinct(env_records$antibiotic)
n_env_matrix_b <- n_distinct(before_env$sample_type)
n_env_matrix_a <- n_distinct(env_records$matrix)
n_env_unit_b <- n_distinct(before_env$concentration_unit)
n_env_unit_a <- n_distinct(env_records$concentration_unit)
n_env_prov_b <- n_distinct(before_env$province)
n_env_prov_a <- n_distinct(env_records$province)

n_carss_abx_b <- length(carss_abx_before)
n_carss_abx_a <- n_distinct(resistance$antibiotic)
n_carss_prov_b <- length(carss_prov_before) # includes National
n_carss_prov_a <- n_distinct(resistance$province)

n_yb_prov_b <- n_distinct(yearbook_raw$province)
n_yb_prov_a <- n_distinct(yearbook$province)

panel_data <- tibble(
  domain = c(rep("env", 8), rep("CARSS", 4), rep("Yearbook", 2)),
  variable = c(
    "antibiotic", "antibiotic",
    "matrix", "matrix",
    "concentration_unit", "concentration_unit",
    "province", "province",
    "antibiotic", "antibiotic",
    "province", "province",
    "province", "province"
  ),
  stage = rep(c("Before", "After"), 7),
  n = c(
    n_env_abx_b, n_env_abx_a,
    n_env_matrix_b, n_env_matrix_a,
    n_env_unit_b, n_env_unit_a,
    n_env_prov_b, n_env_prov_a,
    n_carss_abx_b, n_carss_abx_a,
    n_carss_prov_b, n_carss_prov_a,
    n_yb_prov_b, n_yb_prov_a
  )
) %>%
  mutate(
    domain = factor(domain, levels = c("env", "CARSS", "Yearbook")),
    stage = factor(stage, levels = c("After", "Before")),
    col = factor(
      case_when(
        domain == "env" & variable == "antibiotic" ~ "1",
        domain == "env" & variable == "matrix" ~ "2",
        domain == "env" & variable == "concentration_unit" ~ "3",
        domain == "env" & variable == "province" ~ "4",
        domain == "CARSS" & variable == "antibiotic" ~ "5",
        domain == "CARSS" & variable == "province" ~ "6",
        domain == "Yearbook" & variable == "province" ~ "7"
      ),
      levels = as.character(1:7)
    ),
    circle_col = case_when(
      domain == "env" ~ COL_ENV,
      domain == "CARSS" ~ COL_RES,
      domain == "Yearbook" ~ COL_YB
    ),
    circle_fill = alpha(circle_col, 0.35)
  ) %>%
  group_by(col) %>%
  mutate(pt_size = 5 + 22 * (n / max(n))) %>%
  ungroup()

header_labs <- tibble(
  col = factor(as.character(1:7), levels = as.character(1:7)),
  label = c(
    "antibiotic", "matrix", "concentration_unit", "province",
    "antibiotic", "province", "province"
  )
)

domain_labs <- tibble(
  x = c(2.5, 5.5, 7),
  label = c("Environmental", "CARSS", "Yearbook"),
  colour = c(COL_ENV, COL_RES, COL_YB)
)

# ---- plot -------------------------------------------------------------------

fig02a <- ggplot(panel_data, aes(x = col, y = stage)) +
  geom_point(
    aes(size = pt_size, colour = circle_col, fill = circle_fill),
    shape = 21,
    stroke = 0.7
  ) +
  geom_text(
    aes(label = comma(n)),
    size = 3.4,
    fontface = "bold",
    colour = COL_INK
  ) +
  geom_text(
    data = header_labs,
    aes(x = col, label = label),
    y = 2.52,
    vjust = 0,
    size = 2.9,
    fontface = "bold",
    colour = COL_INK,
    inherit.aes = FALSE
  ) +
  geom_text(
    data = domain_labs,
    aes(x = x, label = label, colour = colour),
    y = 0.28,
    size = 3.3,
    fontface = "bold",
    inherit.aes = FALSE
  ) +
  geom_segment(
    data = tibble(x = c(4.5, 6.5)),
    aes(x = x, xend = x, y = 0.72, yend = 2.28),
    colour = COL_MUTED,
    linewidth = 0.4,
    linetype = "22",
    inherit.aes = FALSE
  ) +
  scale_colour_identity() +
  scale_fill_identity() +
  scale_size_identity() +
  scale_x_discrete(
    labels = NULL,
    expand = expansion(mult = c(0.04, 0.02), add = c(0.55, 0.15))
  ) +
  scale_y_discrete(limits = c("After", "Before")) +
  coord_cartesian(ylim = c(0.5, 2.72), clip = "off") +
  labs(
    title = "Unique values before vs after harmonization",
    subtitle = "Environmental → CARSS → Yearbook; circle area scaled within each column"
  ) +
  theme_minimal(base_size = 10) +
  theme(
    plot.title = element_text(face = "bold", colour = COL_INK),
    plot.subtitle = element_text(colour = COL_MUTED),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    legend.position = "none",
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(
      face = "bold", colour = COL_INK, size = 10, margin = margin(r = 14)
    ),
    plot.margin = margin(8, 22, 24, 8)
  )

fig02a

# ---- save -------------------------------------------------------------------

ggsave(
  file.path(out_dir, "fig02a_harmonization_uniques.pdf"),
  fig02a, width = 8.5, height = 4.2, units = "in", bg = "white"
)
ggsave(
  file.path(out_dir, "fig02a_harmonization_uniques.png"),
  fig02a, width = 8.5, height = 4.2, units = "in", dpi = 300, bg = "white"
)

message("Wrote figures/fig02a_harmonization_uniques.pdf/.png")
message(
  "env: abx ", n_env_abx_b, "→", n_env_abx_a,
  " | matrix ", n_env_matrix_b, "→", n_env_matrix_a,
  " | unit ", n_env_unit_b, "→", n_env_unit_a,
  " | province ", n_env_prov_b, "→", n_env_prov_a
)
message(
  "CARSS: abx ", n_carss_abx_b, "→", n_carss_abx_a,
  " | province ", n_carss_prov_b, "→", n_carss_prov_a
)
message("Yearbook: province ", n_yb_prov_b, "→", n_yb_prov_a)
