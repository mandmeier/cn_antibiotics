# Fig. 3A — Antibiotics coverage by dataset and sample groups
#
# Two columns (side by side) for horizontal efficiency:
#   Left  — classes through Macrolides (by env class order)
#   Right — remaining classes
#
# Each antibiotic row: class band | env total + per-matrix counts |
# CARSS total + per-pathogen counts. Shared (env ∩ CARSS) names are bold.
#
# RStudio: WD = cn_antibiotics/; run line by line or source this file.

library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(scales)
library(stringr)
library(patchwork)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)

source("R/utils/environmental_units.R")

out_dir <- "figures"
dir.create(out_dir, showWarnings = FALSE)

# ---- inputs -----------------------------------------------------------------

env_records <- read_csv(
  "data/output/supporting/env_records.csv",
  show_col_types = FALSE
)
env_province <- read_csv(
  "data/output/primary/env_province.csv",
  show_col_types = FALSE
) %>%
  mutate(matrix = assign_environmental_matrix(sample_type, reference_number))

resistance <- read_csv(
  "data/output/primary/resistance_province.csv",
  show_col_types = FALSE
)

# ---- counts + ordering ------------------------------------------------------

n_env <- env_province %>%
  count(antibiotic, name = "n_env")

n_res <- resistance %>%
  count(antibiotic, name = "n_res")

n_env_matrix <- env_province %>%
  count(antibiotic, matrix, name = "n")

n_res_bact <- resistance %>%
  count(antibiotic, bacteria_species, name = "n")

matrix_order <- n_env_matrix %>%
  group_by(matrix) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  arrange(desc(n), matrix) %>%
  pull(matrix)

bact_order <- n_res_bact %>%
  group_by(bacteria_species) %>%
  summarise(n = sum(n), .groups = "drop") %>%
  arrange(desc(n), bacteria_species) %>%
  pull(bacteria_species)

class_lookup <- bind_rows(
  env_records %>% distinct(antibiotic, antibiotic_class),
  resistance %>% distinct(antibiotic, antibiotic_class)
) %>%
  distinct(antibiotic, antibiotic_class)

abx_all <- tibble(
  antibiotic = sort(union(unique(env_records$antibiotic), unique(resistance$antibiotic)))
) %>%
  left_join(class_lookup, by = "antibiotic") %>%
  left_join(n_env, by = "antibiotic") %>%
  left_join(n_res, by = "antibiotic") %>%
  mutate(
    n_env = replace_na(n_env, 0L),
    n_res = replace_na(n_res, 0L),
    in_env = n_env > 0L,
    in_res = n_res > 0L
  )

stopifnot(nrow(abx_all) == 128L)
stopifnot(!anyNA(abx_all$antibiotic_class))
stopifnot(sum(abx_all$in_env & abx_all$in_res) == 15L)
stopifnot(sum(abx_all$in_res & !abx_all$in_env) == 19L)
stopifnot(sum(!abx_all$in_res) == 94L)

class_order <- abx_all %>%
  group_by(antibiotic_class) %>%
  summarise(class_env = sum(n_env), .groups = "drop") %>%
  arrange(desc(class_env), antibiotic_class) %>%
  pull(antibiotic_class)

stopifnot("Macrolides" %in% class_order)
split_at <- match("Macrolides", class_order)
classes_left <- class_order[seq_len(split_at)]
classes_right <- class_order[seq(split_at + 1L, length(class_order))]

# ---- shared column defs -----------------------------------------------------

col_levels <- c(
  "env_total",
  paste0("env_", matrix_order),
  "res_total",
  paste0("res_", bact_order)
)

matrix_labs <- matrix_order %>%
  str_replace("^wastewater influent$", "WW influent") %>%
  str_replace("^wastewater effluent$", "WW effluent")

col_labs <- c(
  "total",
  matrix_labs,
  "total",
  bact_order %>%
    str_replace("^K\\. pneumoniae$", "K. pneum.") %>%
    str_replace("^A\\. baumannii$", "A. baum.") %>%
    str_replace("^P\\. aeruginosa$", "P. aerug.")
)
n_env_cols <- 1L + length(matrix_order)

CLASS_PALETTE <- c(
  "#D4B483", "#7A9E9F", "#C97B7B", "#8FA3B8", "#B08D6A",
  "#8A9A6B", "#A88B9C", "#6E8B7E", "#C4A574", "#7E8C9A",
  "#9B7E6A", "#5F8A8B"
)
CLASS_COLS <- setNames(CLASS_PALETTE[seq_along(class_order)], class_order)

COL_ENV <- "#4F7A5A"
COL_RES <- "#3D6B8A"
COL_INK <- "#1F2933"
COL_MUTED <- "#5A6A76"

# Log-scale sequential fills (totals scaled independently of breakdown cols)
PAL_ENV <- c("#E7F1EA", "#8FBF9A", "#2F6B45")
PAL_RES <- c("#E4EEF5", "#7FA8C4", "#2F5F7A")

# Vertical density kept; data-column pitch matched to row height (visual).
# Squares via geom_point(shape=22).
# CLASS_W_SEED is only used to keep data-column COL_GAP unchanged.
CLASS_W_SEED <- 1.95
NAME_GAP <- 0.06
SQ_PT <- 2.45
COUNT_PT <- 1.05

# Expected panel size when two panels share fig_w × fig_h (see ggsave below)
PANEL_W_IN <- 6.5
PANEL_H_IN <- 9.0

# ---- log colour helpers -----------------------------------------------------

log_colour <- function(n, n_min, n_max, palette) {
  # Map counts on a log scale onto a sequential palette.
  n <- pmax(as.numeric(n), n_min)
  t <- (log10(n) - log10(n_min)) / (log10(n_max) - log10(n_min))
  t <- pmin(pmax(t, 0), 1)
  scales::colour_ramp(palette)(t)
}

luma <- function(hex) {
  rgb <- col2rgb(hex) / 255
  as.numeric(0.2126 * rgb[1, ] + 0.7152 * rgb[2, ] + 0.0722 * rgb[3, ])
}

scale_limits <- list(
  env_total = c(
    min(n_env$n_env),
    max(n_env$n_env)
  ),
  env_detail = c(
    min(n_env_matrix$n),
    max(n_env_matrix$n)
  ),
  res_total = c(
    min(n_res$n_res),
    max(n_res$n_res)
  ),
  res_detail = c(
    min(n_res_bact$n),
    max(n_res_bact$n)
  )
)

cell_fill <- function(n, scale_id) {
  vapply(seq_along(n), function(i) {
    lim <- scale_limits[[scale_id[[i]]]]
    pal <- if (startsWith(scale_id[[i]], "env")) PAL_ENV else PAL_RES
    log_colour(n[[i]], lim[[1]], lim[[2]], pal)
  }, character(1))
}

# ---- panel builder ----------------------------------------------------------

make_panel <- function(abx_in, class_levels_panel, y_scale_n, shared_abx) {
  abx <- abx_in %>%
    filter(antibiotic_class %in% class_levels_panel) %>%
    mutate(
      antibiotic_class = factor(antibiotic_class, levels = class_levels_panel),
      is_shared = antibiotic %in% shared_abx
    ) %>%
    arrange(antibiotic_class, desc(n_env), antibiotic)

  n_panel <- nrow(abx)
  # Top-align within the shared y scale so row spacing matches the left panel
  abx <- abx %>%
    mutate(y = as.numeric(seq(y_scale_n, y_scale_n - n_panel + 1L, by = -1L)))

  cells <- bind_rows(
    abx %>%
      filter(in_env) %>%
      transmute(
        antibiotic, y,
        col_id = "env_total", family = "env", n = n_env
      ),
    n_env_matrix %>%
      inner_join(abx %>% select(antibiotic, y), by = "antibiotic") %>%
      transmute(
        antibiotic, y,
        col_id = paste0("env_", matrix), family = "env", n
      ),
    abx %>%
      filter(in_res) %>%
      transmute(
        antibiotic, y,
        col_id = "res_total", family = "res", n = n_res
      ),
    n_res_bact %>%
      inner_join(abx %>% select(antibiotic, y), by = "antibiotic") %>%
      transmute(
        antibiotic, y,
        col_id = paste0("res_", bacteria_species), family = "res", n
      )
  ) %>%
    mutate(
      col_id = factor(col_id, levels = col_levels),
      scale_id = case_when(
        col_id == "env_total" ~ "env_total",
        family == "env" ~ "env_detail",
        col_id == "res_total" ~ "res_total",
        TRUE ~ "res_detail"
      ),
      fill_col = cell_fill(n, scale_id),
      edge_col = if_else(family == "env", COL_ENV, COL_RES),
      label_col = if_else(luma(fill_col) < 0.55, "white", COL_INK)
    )

  # Names right-aligned flush against the class column (no dead gap).
  name_span <- max(nchar(abx$antibiotic)) * 0.088
  X_CLASS_L <- name_span + NAME_GAP

  header_band <- 8.5
  y_range <- y_scale_n + header_band + 0.5
  n_cols <- length(col_levels)

  # Keep data-column COL_GAP exactly as when squares looked right (seed class width).
  x_fixed <- name_span + NAME_GAP + CLASS_W_SEED + 0.20 + 0.25 + 0.40
  gap_frac <- n_cols * PANEL_H_IN / (y_range * PANEL_W_IN)
  COL_GAP <- (x_fixed * PANEL_H_IN / (y_range * PANEL_W_IN)) / (1 - gap_frac)
  COL_GAP <- max(COL_GAP, 0.08)

  # Size class column to fit "Diaminopyrimidines" only — do not change COL_GAP.
  x_rest <- name_span + NAME_GAP + 0.20 + n_cols * COL_GAP + 0.25 * COL_GAP + 0.40
  need_in <- nchar("Diaminopyrimidines") * 0.048 + 0.10
  CLASS_W <- need_in * x_rest / (PANEL_W_IN - need_in)
  X_CLASS_R <- X_CLASS_L + CLASS_W

  x_offsets <- (seq_along(col_levels) - 1L) * COL_GAP
  x_offsets[seq(n_env_cols + 1L, length(col_levels))] <-
    x_offsets[seq(n_env_cols + 1L, length(col_levels))] + 0.25 * COL_GAP

  x0 <- X_CLASS_R + 0.20
  col_x <- tibble(
    col_id = factor(col_levels, levels = col_levels),
    col_lab = col_labs,
    family = if_else(str_starts(col_levels, "env"), "env", "res"),
    x = x0 + x_offsets
  )

  cells <- cells %>%
    left_join(col_x %>% select(col_id, x), by = "col_id")

  class_boxes <- abx %>%
    group_by(antibiotic_class) %>%
    summarise(
      ymin = min(y) - 0.45,
      ymax = max(y) + 0.45,
      ymid = (min(y) + max(y)) / 2,
      n_in_class = n(),
      .groups = "drop"
    ) %>%
    mutate(
      xmin = X_CLASS_L,
      xmax = X_CLASS_R,
      box_h = ymax - ymin,
      label_vertical = box_h >= nchar(as.character(antibiotic_class)) * 0.55
    )

  # Data-column headers only (no section titles): left-aligned, just above first row.
  y_sub <- y_scale_n + 0.55
  y_top <- y_scale_n + header_band

  sub_headers <- col_x %>%
    transmute(x, y = y_sub, label = col_lab)

  ggplot() +
    geom_rect(
      data = class_boxes,
      aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = antibiotic_class),
      colour = NA
    ) +
    geom_text(
      data = class_boxes %>% filter(label_vertical),
      aes(x = (xmin + xmax) / 2, y = ymid, label = antibiotic_class),
      angle = 90,
      size = 2.0,
      colour = COL_INK,
      fontface = "bold",
      lineheight = 0.9
    ) +
    geom_text(
      data = class_boxes %>% filter(!label_vertical),
      aes(x = (xmin + xmax) / 2, y = ymid, label = antibiotic_class),
      angle = 0,
      size = 1.35,
      colour = COL_INK,
      fontface = "bold"
    ) +
    # Right-aligned names sitting against the class boxes
    geom_text(
      data = abx,
      aes(
        x = X_CLASS_L - NAME_GAP,
        y = y,
        label = antibiotic,
        fontface = if_else(is_shared, "bold", "plain")
      ),
      hjust = 1,
      size = 1.65,
      colour = COL_INK
    ) +
    # True squares; fill = log-scaled green/blue (totals on independent scales)
    geom_point(
      data = cells,
      aes(x = x, y = y),
      shape = 22,
      size = SQ_PT,
      fill = cells$fill_col,
      colour = alpha(cells$edge_col, 0.70),
      stroke = 0.25
    ) +
    geom_text(
      data = cells,
      aes(x = x, y = y, label = comma(n)),
      size = COUNT_PT,
      colour = cells$label_col
    ) +
    geom_text(
      data = sub_headers,
      aes(x = x, y = y, label = label),
      angle = 90,
      hjust = 0,
      vjust = 1,
      size = 1.55,
      colour = COL_MUTED
    ) +
    scale_fill_manual(values = CLASS_COLS, guide = "none", drop = FALSE) +
    coord_cartesian(
      xlim = c(0, max(col_x$x) + 0.45),
      ylim = c(0.02, y_top),
      expand = FALSE,
      clip = "off"
    ) +
    theme_void(base_size = 10) +
    theme(
      plot.margin = margin(4, 0, 2, 0),
      plot.background = element_rect(fill = "white", colour = NA),
      panel.background = element_rect(fill = "white", colour = NA),
      panel.grid = element_blank()
    )
}

# ---- assemble ---------------------------------------------------------------

shared_abx <- sort(intersect(
  unique(env_records$antibiotic),
  unique(resistance$antibiotic)
))
stopifnot(length(shared_abx) == 15L)

n_left <- sum(abx_all$antibiotic_class %in% classes_left)
n_right <- sum(abx_all$antibiotic_class %in% classes_right)
y_scale_n <- n_left # shared row scale (left is taller)

panel_left <- make_panel(abx_all, classes_left, y_scale_n, shared_abx) +
  theme(plot.margin = margin(4, 0, 2, 4))
panel_right <- make_panel(abx_all, classes_right, y_scale_n, shared_abx) +
  theme(plot.margin = margin(4, 4, 2, 0))

fig_s <- panel_left | panel_right

fig_s

# ---- save -------------------------------------------------------------------

out_pdf <- file.path(out_dir, "fig03a_antibiotic_coverage.pdf")
out_png <- file.path(out_dir, "fig03a_antibiotic_coverage.png")

# Two-panel layout; vertical density kept
fig_w <- 13.5
fig_h <- PANEL_H_IN
ggsave(out_pdf, fig_s, width = fig_w, height = fig_h, units = "in", bg = "white")
ggsave(out_png, fig_s, width = fig_w, height = fig_h, units = "in", dpi = 300, bg = "white")

message("Wrote ", out_pdf)
message("Wrote ", out_png)
message(
  "left classes: ", paste(classes_left, collapse = "; "),
  " | right classes: ", paste(classes_right, collapse = "; "),
  " | size=", fig_w, "x", fig_h, "in"
)
message(
  "left n=", n_left,
  " | right n=", n_right,
  " | y_scale_n=", y_scale_n,
  " | shared bold=", length(shared_abx)
)
message(
  "scales log10 | env_total ", paste(scale_limits$env_total, collapse = "-"),
  " | env_detail ", paste(scale_limits$env_detail, collapse = "-"),
  " | res_total ", paste(scale_limits$res_total, collapse = "-"),
  " | res_detail ", paste(scale_limits$res_detail, collapse = "-")
)
