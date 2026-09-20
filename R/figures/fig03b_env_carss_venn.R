# Fig. 3B — Environmental vs CARSS antibiotics (Venn)
#
# Exact antibiotic-name intersection of cleaned env_records and
# resistance_province (15 shared join-key compounds).
# Shared names are bold in Fig. 3A.
#
# RStudio: WD = cn_antibiotics/; run line by line or source this file.

library(dplyr)
library(ggplot2)
library(readr)
library(scales)

conflicts_prefer(dplyr::filter)
conflicts_prefer(dplyr::lag)

COL_ENV <- "#2F6B45"
COL_RES <- "#2F5F7A"
COL_INK <- "#1F2933"
COL_MUTED <- "#5A6A76"

out_dir <- "figures"
dir.create(out_dir, showWarnings = FALSE)

env_records <- read_csv(
  "data/output/supporting/env_records.csv",
  show_col_types = FALSE
)
resistance <- read_csv(
  "data/output/primary/resistance_province.csv",
  show_col_types = FALSE
)

env_abx <- sort(unique(env_records$antibiotic))
carss_abx <- sort(unique(resistance$antibiotic))
shared_abx <- sort(intersect(env_abx, carss_abx))
n_env_only <- length(setdiff(env_abx, carss_abx))
n_carss_only <- length(setdiff(carss_abx, env_abx))
n_shared <- length(shared_abx)

stopifnot(n_shared == 15L)

circle_df <- function(cx, cy, r, n = 240) {
  tibble(
    x = cx + r * cos(seq(0, 2 * pi, length.out = n)),
    y = cy + r * sin(seq(0, 2 * pi, length.out = n))
  )
}

r <- 1.15
c1 <- circle_df(-0.55, 0, r)
c2 <- circle_df(0.55, 0, r)

venn_labels <- tibble(
  x = c(-1.05, 0, 1.05),
  y = c(0.05, 0.05, 0.05),
  label = c(
    as.character(n_env_only),
    as.character(n_shared),
    as.character(n_carss_only)
  ),
  size = c(4.8, 5.6, 4.8)
)

fig <- ggplot() +
  geom_polygon(
    data = c1,
    aes(x, y),
    fill = alpha(COL_ENV, 0.45),
    colour = COL_ENV,
    linewidth = 0.55
  ) +
  geom_polygon(
    data = c2,
    aes(x, y),
    fill = alpha(COL_RES, 0.40),
    colour = COL_RES,
    linewidth = 0.55
  ) +
  geom_text(
    data = venn_labels,
    aes(x, y, label = label, size = size),
    colour = COL_INK,
    fontface = "bold",
    show.legend = FALSE
  ) +
  scale_size_identity() +
  annotate(
    "text",
    x = -1.05,
    y = -1.55,
    label = paste0("Environmental\n(", length(env_abx), ")"),
    size = 3.4,
    colour = COL_ENV,
    fontface = "bold",
    lineheight = 0.95
  ) +
  annotate(
    "text",
    x = 1.05,
    y = -1.55,
    label = paste0("CARSS\n(", length(carss_abx), ")"),
    size = 3.4,
    colour = COL_RES,
    fontface = "bold",
    lineheight = 0.95
  ) +
  annotate(
    "text",
    x = 0,
    y = 1.45,
    label = "15 shared",
    size = 3.4,
    fontface = "bold",
    colour = COL_INK
  ) +
  coord_equal(xlim = c(-2.1, 2.1), ylim = c(-1.9, 1.7), expand = FALSE) +
  labs(
    title = "Environmental vs CARSS compounds",
    subtitle = "Exact name overlap (15 shared join-key compounds)"
  ) +
  theme_void(base_size = 11) +
  theme(
    plot.title = element_text(face = "bold", colour = COL_INK, hjust = 0.5),
    plot.subtitle = element_text(colour = COL_MUTED, hjust = 0.5, size = 9),
    plot.margin = margin(10, 12, 10, 12),
    plot.background = element_rect(fill = "white", colour = NA)
  )

fig

out_pdf <- file.path(out_dir, "fig03b_env_carss_venn.pdf")
out_png <- file.path(out_dir, "fig03b_env_carss_venn.png")
ggsave(out_pdf, fig, width = 6.0, height = 5.2, units = "in", bg = "white")
ggsave(out_png, fig, width = 6.0, height = 5.2, units = "in", dpi = 300, bg = "white")

message("Wrote ", out_pdf)
message("Wrote ", out_png)
message(
  "env-only=", n_env_only,
  " shared=", n_shared,
  " carss-only=", n_carss_only,
  " | env=", length(env_abx),
  " carss=", length(carss_abx)
)
message("shared: ", paste(shared_abx, collapse = "; "))
