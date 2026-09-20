# Fig. 1 — Schematic of the data pipeline
#
# Three input domains → harmonization → three primary province-level CSVs.
# Box stats are computed live from the curated tables.
#
# RStudio: open the cn_antibiotics project (WD = package root), then
# run this file line by line or: source("R/figures/fig01_pipeline_schematic.R")

library(dplyr)
library(ggplot2)
library(readr)
library(tibble)
library(scales)

# ---- paths ------------------------------------------------------------------

out_dir <- "figures"
dir.create(out_dir, showWarnings = FALSE)

env_province <- read_csv("data/output/primary/env_province.csv", show_col_types = FALSE)
resistance <- read_csv("data/output/primary/resistance_province.csv", show_col_types = FALSE)
yearbook <- read_csv("data/output/primary/yearbook_province.csv", show_col_types = FALSE)
env_records <- read_csv("data/output/supporting/env_records.csv", show_col_types = FALSE)

# ---- live output-box stats --------------------------------------------------

fmt_n <- function(x) comma(as.integer(x))

env_stats <- paste(
  c(
    paste0(fmt_n(nrow(env_province)), " records"),
    paste0(n_distinct(env_province$province), " provinces"),
    paste0(n_distinct(env_province$antibiotic), " antibiotics"),
    paste0(n_distinct(env_records$matrix), " matrices"),
    paste0(n_distinct(env_province$reference_number), " source refs"),
    paste0(
      "Years ",
      min(env_province$sample_year, na.rm = TRUE), "-",
      max(env_province$sample_year, na.rm = TRUE)
    )
  ),
  collapse = "\n"
)

res_stats <- paste(
  c(
    paste0(fmt_n(nrow(resistance)), " records"),
    paste0(n_distinct(resistance$province), " provinces"),
    paste0(n_distinct(resistance$antibiotic), " antibiotics"),
    paste0(n_distinct(resistance$bacteria_species), " pathogens"),
    paste0(
      "Years ",
      min(resistance$year, na.rm = TRUE), "-",
      max(resistance$year, na.rm = TRUE)
    )
  ),
  collapse = "\n"
)

yb_stats <- paste(
  c(
    paste0(fmt_n(nrow(yearbook)), " records"),
    paste0(n_distinct(yearbook$province), " provinces"),
    paste0(n_distinct(yearbook$metric), " metrics"),
    paste0(
      "Years ",
      min(yearbook$year, na.rm = TRUE), "-",
      max(yearbook$year, na.rm = TRUE)
    )
  ),
  collapse = "\n"
)

# Domain colours (match manuscript ODP)
COL_ENV <- "#D9EBD7"
COL_RES <- "#D6E4F0"
COL_YB <- "#E8DDF0"
COL_HARM <- "#F7E8D0"
COL_INK <- "#1F2933"
COL_MUTED <- "#5A6A76"

# ---- layout boxes -----------------------------------------------------------

boxes <- tribble(
  ~id, ~label, ~x, ~y, ~w, ~h, ~fill,
  "in_env", paste0(
    "Literature\n",
    "Environmental antibiotics\nconcentrations"
  ), 1.7, 5.5, 2.6, 1.6, COL_ENV,
  "in_carss", paste0(
    "CARSS\n",
    "Clinical microbial\nantibiotics resistance"
  ), 1.7, 3.4, 2.6, 1.6, COL_RES,
  "in_nbs", paste0(
    "NBS\n",
    "Statistical Yearbook\nSocioeconomic metrics"
  ), 1.7, 1.3, 2.6, 1.6, COL_YB,
  "harm", paste(
    "Standardization & harmonization",
    "",
    "• Consistent antibiotic names (INN)",
    "• Mapping to antibiotic classes",
    "• Unifying sample types (matrices)",
    "• Harmonizing provinces",
    "• Standardized matrix-specific units",
    sep = "\n"
  ), 5.6, 3.4, 3.4, 4.4, COL_HARM,
  "out_env", paste0("env_province.csv\n", env_stats),
  9.7, 5.5, 3.0, 2.0, COL_ENV,
  "out_res", paste0("resistance_province.csv\n", res_stats),
  9.7, 3.4, 3.0, 1.8, COL_RES,
  "out_yb", paste0("yearbook_province.csv\n", yb_stats),
  9.7, 1.35, 3.0, 1.6, COL_YB
)

arrows <- tribble(
  ~x, ~y, ~xend, ~yend,
  3.0, 5.5, 3.9, 4.6,
  3.0, 3.4, 3.9, 3.4,
  3.0, 1.3, 3.9, 2.2,
  7.3, 4.6, 8.2, 5.5,
  7.3, 3.4, 8.2, 3.4,
  7.3, 2.2, 8.2, 1.35
)

column_labels <- tribble(
  ~x, ~y, ~label,
  1.7, 6.75, "Input data",
  5.6, 6.75, "Harmonization",
  9.7, 6.75, "Province-level interoperable datasets"
)

# ---- plot -------------------------------------------------------------------

fig01 <- ggplot() +
  geom_text(
    data = column_labels,
    aes(x = x, y = y, label = label),
    fontface = "bold",
    size = 3.8,
    colour = "#2F3A44"
  ) +
  geom_rect(
    data = boxes,
    aes(
      xmin = x - w / 2, xmax = x + w / 2,
      ymin = y - h / 2, ymax = y + h / 2,
      fill = fill
    ),
    colour = "#3D4A54",
    linewidth = 0.55,
    inherit.aes = FALSE
  ) +
  scale_fill_identity() +
  geom_text(
    data = boxes,
    aes(x = x, y = y, label = label),
    size = 2.55,
    colour = COL_INK,
    lineheight = 1.05
  ) +
  geom_segment(
    data = arrows,
    aes(x = x, y = y, xend = xend, yend = yend),
    arrow = arrow(length = unit(0.18, "cm"), type = "closed"),
    colour = COL_MUTED,
    linewidth = 0.55,
    lineend = "round"
  ) +
  annotate(
    "text",
    x = 5.6, y = -0.15,
    label = paste(
      "Supporting: env_records, env_site*, yearbook_full",
      "·  Meta: codebook, join_key, antibiotic_classes"
    ),
    size = 2.5,
    colour = COL_MUTED
  ) +
  coord_equal(xlim = c(0.1, 11.4), ylim = c(-0.45, 7.15), expand = FALSE) +
  theme_void() +
  theme(
    plot.background = element_rect(fill = "white", colour = NA),
    plot.margin = margin(8, 10, 8, 10)
  )

fig01

# ---- save -------------------------------------------------------------------

ggsave(
  file.path(out_dir, "fig01_pipeline_schematic.pdf"),
  fig01, width = 10.0, height = 6.4, units = "in", bg = "white"
)
ggsave(
  file.path(out_dir, "fig01_pipeline_schematic.png"),
  fig01, width = 10.0, height = 6.4, units = "in", dpi = 300, bg = "white"
)

message("Wrote figures/fig01_pipeline_schematic.pdf/.png")
