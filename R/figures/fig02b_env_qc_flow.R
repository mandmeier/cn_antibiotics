# Fig. 2B — Environmental records QC filter funnel
#
# Horizontal stacked bars: retained (left) vs removed (right) at each stage.
# Counts come from R/01 (data/intermediate/validation/flow_stages.csv).
#
# RStudio: WD = cn_antibiotics/; run after R/01_clean_environmental_data.R

library(dplyr)
library(ggplot2)
library(readr)
library(tidyr)
library(scales)

conflicts_prefer(dplyr::lag)
conflicts_prefer(dplyr::filter)

# ---- inputs -----------------------------------------------------------------

out_dir <- "figures"
dir.create(out_dir, showWarnings = FALSE)

flow_stages <- read_csv(
  "data/intermediate/validation/flow_stages.csv",
  show_col_types = FALSE
)

flow_stages

# Optional: friendlier y-axis labels for the manuscript panel
label_map <- c(
  "Combined (Zhang unique + supplemental)" =
    "Inputs: Raw environmental measurements",
  "After missing conc. / name" =
    "Missing concentration or antibiotic name excluded",
  "After non-province place drop" =
    "Non-province place excluded",
  "After class totals / non-antibiotics / solid waste" =
    "Class totals / non-antibiotics / solid waste",
  "After incompatible unit–matrix pairs" =
    "Incompatible unit–matrix pairs",
  "After duplicates (source-rank / season)" =
    "Duplicates (source-rank / season collapse)",
  "env_records (cleaned)" =
    "Retained env_records: cleaned measurements"
)

COL_RETAINED <- "#A8C99A"
COL_REMOVED <- "#C97B7B"

# ---- long table for stacked bars --------------------------------------------

comp_levels <- c("retained", "removed")
comp_labels <- c("Retained", "Removed")

bars <- flow_stages %>%
  mutate(
    stage_lab = ifelse(
      stage_lab %in% names(label_map),
      unname(label_map[stage_lab]),
      stage_lab
    )
  ) %>%
  pivot_longer(
    cols = c(retained, removed),
    names_to = "component_key",
    values_to = "n"
  ) %>%
  mutate(
    stack_order = match(component_key, comp_levels),
    component = factor(
      component_key,
      levels = comp_levels,
      labels = comp_labels
    ),
    stage_lab = factor(stage_lab, levels = rev(unique(stage_lab))),
    n = as.integer(n)
  ) %>%
  filter(n > 0L) %>%
  arrange(stage_lab, stack_order)

bars <- bars %>%
  group_by(stage_lab) %>%
  mutate(
    xmin = lag(cumsum(n), default = 0),
    xmax = cumsum(n),
    xmid = (xmin + xmax) / 2,
    bar_end = max(xmax),
    y = as.numeric(stage_lab),
    label = comma(n),
    segment_width = xmax - xmin
  ) %>%
  ungroup()

# Wide segments: number centered; narrow removed segments sit outside the bar
narrow_width <- 600
outside_gap <- 150

bars <- bars %>%
  mutate(
    label_outside = segment_width < narrow_width,
    x_label = if_else(label_outside, bar_end + outside_gap, xmid),
    label_hjust = if_else(label_outside, 0, 0.5)
  )

bars

# ---- plot -------------------------------------------------------------------

fig02b <- ggplot(bars) +
  geom_rect(
    aes(
      ymin = y - 0.28,
      ymax = y + 0.28,
      xmin = xmin,
      xmax = xmax,
      fill = component
    ),
    colour = "#3D4A54",
    linewidth = 0.25
  ) +
  geom_text(
    aes(x = x_label, y = y, label = label, hjust = label_hjust),
    colour = "#1F2933",
    size = 2.8
  ) +
  scale_y_continuous(
    breaks = seq_along(levels(bars$stage_lab)),
    labels = levels(bars$stage_lab)
  ) +
  scale_fill_manual(
    values = c("Retained" = COL_RETAINED, "Removed" = COL_REMOVED),
    breaks = comp_labels,
    guide = guide_legend(title = NULL, nrow = 1)
  ) +
  scale_x_continuous(
    labels = comma,
    expand = expansion(mult = c(0.02, 0.10))
  ) +
  labs(title = "Environmental records QC filters") +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    panel.grid.minor = element_blank(),
    plot.title = element_text(face = "bold", colour = "#2F3A44"),
    axis.title = element_blank()
  )

fig02b

# ---- save -------------------------------------------------------------------

ggsave(
  file.path(out_dir, "fig02b_env_qc_flow.pdf"),
  fig02b, width = 10.5, height = 7.0, units = "in", bg = "white"
)
ggsave(
  file.path(out_dir, "fig02b_env_qc_flow.png"),
  fig02b, width = 10.5, height = 7.0, units = "in", dpi = 300, bg = "white"
)

message("Wrote figures/fig02b_env_qc_flow.pdf/.png")
