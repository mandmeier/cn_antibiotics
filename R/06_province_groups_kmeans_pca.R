# K-means and PCA of by-province environmental and clinical antibiotic metrics.
#
# 1) log10-transform values (concentrations may span orders of magnitude)
# 2) z-score the log values (center around China mean)
# 3) PCA on the province x feature matrix; k-means on the first 4 PCs

suppressPackageStartupMessages({
  library(cluster)
  library(dplyr)
  library(ggplot2)
  library(ggrepel)
  library(patchwork)
  library(readr)
  library(tibble)
  library(tidyr)
})

figures_dir <- "data/output/figures"
dir.create(figures_dir, recursive = TRUE, showWarnings = FALSE)

antibiotic_metrics_china <- read_csv(
  "data/output/antibiotic_metrics_china.csv",
  show_col_types = FALSE
)

china_metrics <- antibiotic_metrics_china %>%
  filter(province != "Hong Kong") %>%
  mutate(log10_value = log10(value)) %>%
  mutate(z_score = as.numeric(scale(log10_value)))

# Wide matrix: rows = provinces, columns = antibiotic x metric
df_wide_mat <- china_metrics %>%
  mutate(feature = paste(antibiotic, metric, sep = "__")) %>%
  select(province, feature, z_score) %>%
  pivot_wider(
    names_from = feature,
    values_from = z_score
  ) %>%
  column_to_rownames("province") %>%
  as.matrix()

df_wide_mat[is.na(df_wide_mat)] <- 0

pca <- prcomp(df_wide_mat, center = FALSE, scale. = FALSE)
pc_scores <- pca$x[, 1:4]
set.seed(42)

# Elbow and silhouette diagnostics for choosing k
k_max <- min(10, nrow(pc_scores) - 1)
k_values <- seq_len(k_max)

kmeans_elbow <- lapply(k_values, function(k) {
  km <- kmeans(pc_scores, centers = k, nstart = 25)
  data.frame(
    k = k,
    tot_withinss = km$tot.withinss,
    betweenss = km$betweenss,
    totss = km$totss
  )
})
kmeans_elbow <- bind_rows(kmeans_elbow)

kmeans_silhouette <- lapply(k_values[k_values >= 2], function(k) {
  km <- kmeans(pc_scores, centers = k, nstart = 25)
  sil <- cluster::silhouette(km$cluster, dist(pc_scores))
  data.frame(k = k, avg_silhouette = mean(sil[, 3]))
})
kmeans_elbow <- left_join(
  kmeans_elbow,
  bind_rows(kmeans_silhouette),
  by = "k"
)

p_elbow <- ggplot(kmeans_elbow, aes(k, tot_withinss)) +
  geom_line(linewidth = 0.8) +
  geom_point(size = 2.5) +
  scale_x_continuous(breaks = k_values) +
  labs(
    title = "K-means elbow (PCA scores, first 4 PCs)",
    subtitle = "Look for the knee: diminishing drop in within-cluster SS as k increases",
    x = "Number of clusters (k)",
    y = "Total within-cluster sum of squares"
  ) +
  theme_minimal(base_size = 12)

p_silhouette <- ggplot(
  kmeans_elbow %>% filter(!is.na(avg_silhouette)),
  aes(k, avg_silhouette)
) +
  geom_line(linewidth = 0.8, colour = "steelblue") +
  geom_point(size = 2.5, colour = "steelblue") +
  scale_x_continuous(breaks = k_values) +
  labs(
    title = "Mean silhouette width by k",
    subtitle = "Higher is better separated clusters (k >= 2 only)",
    x = "Number of clusters (k)",
    y = "Average silhouette width"
  ) +
  theme_minimal(base_size = 12)

ggsave(
  file.path(figures_dir, "kmeans_elbow.png"),
  p_elbow,
  width = 7,
  height = 4.5,
  dpi = 150
)
ggsave(
  file.path(figures_dir, "kmeans_silhouette.png"),
  p_silhouette,
  width = 7,
  height = 4.5,
  dpi = 150
)

write_csv(kmeans_elbow, file.path(figures_dir, "kmeans_k_diagnostics.csv"))

# Fit k = 2, 3, 4
clusters2 <- kmeans(pc_scores, centers = 2, nstart = 25)$cluster
clusters3 <- kmeans(pc_scores, centers = 3, nstart = 25)$cluster
clusters4 <- kmeans(pc_scores, centers = 4, nstart = 25)$cluster

align_cluster_labels <- function(new_labels, old_labels, provinces) {
  old_labels <- as.integer(old_labels)
  new_labels <- as.integer(new_labels)
  old_ids <- sort(unique(old_labels[!is.na(old_labels)]))
  new_ids <- sort(unique(new_labels))
  if (!length(old_ids) || !length(new_ids)) {
    return(new_labels)
  }
  overlap <- outer(
    old_ids,
    new_ids,
    Vectorize(function(old_id, new_id) {
      sum(old_labels == old_id & new_labels == new_id, na.rm = TRUE)
    })
  )
  rownames(overlap) <- old_ids
  colnames(overlap) <- new_ids
  map <- stats::setNames(rep(NA_integer_, length(new_ids)), new_ids)
  for (old_id in old_ids) {
    col_idx <- which.max(overlap[as.character(old_id), , drop = TRUE])
    new_id <- new_ids[col_idx]
    if (is.na(map[as.character(new_id)])) {
      map[as.character(new_id)] <- old_id
    }
  }
  unmapped_new <- new_ids[is.na(map[as.character(new_ids)])]
  unmapped_old <- setdiff(old_ids, unname(map[!is.na(map)]))
  if (length(unmapped_new) && length(unmapped_old)) {
    for (i in seq_along(unmapped_new)) {
      map[as.character(unmapped_new[[i]])] <- unmapped_old[[i]]
    }
  }
  unname(map[as.character(new_labels)])
}

# Align labels to a previous output if present (stable cluster numbering across reruns)
output_groups_path <- "data/output/province_groups.csv"
if (file.exists(output_groups_path)) {
  province_groups_prev <- read_csv(output_groups_path, show_col_types = FALSE)
  prev_lookup <- province_groups_prev %>%
    select(province, any_of(c("k2_groups", "k3_groups", "k4_groups")))

  if (all(c("k2_groups", "k3_groups", "k4_groups") %in% names(prev_lookup))) {
    aligned <- data.frame(
      province = rownames(pc_scores),
      stringsAsFactors = FALSE
    ) %>%
      left_join(prev_lookup, by = "province")
    clusters2 <- align_cluster_labels(clusters2, aligned$k2_groups, aligned$province)
    clusters3 <- align_cluster_labels(clusters3, aligned$k3_groups, aligned$province)
    clusters4 <- align_cluster_labels(clusters4, aligned$k4_groups, aligned$province)
  }
}

cluster_df <- data.frame(
  province = rownames(pc_scores),
  k2_groups = factor(clusters2),
  k3_groups = factor(clusters3),
  k4_groups = factor(clusters4)
) %>%
  remove_rownames() %>%
  arrange(k3_groups)

province_metadata <- read_csv(
  "data/raw/reference/province_metadata.csv",
  show_col_types = FALSE
)

province_groups <- province_metadata %>%
  left_join(cluster_df, by = "province")

write_csv(province_groups, output_groups_path)
message("Wrote ", output_groups_path)

# PCA scatter (k = 3) and China map
clusters <- clusters3
var_exp <- summary(pca)$importance[2, ]
pc1_lab <- paste0("PC1 (", round(var_exp[1] * 100, 1), "%)")
pc2_lab <- paste0("PC2 (", round(var_exp[2] * 100, 1), "%)")

plot_df <- data.frame(
  province = rownames(pca$x),
  PC1 = pca$x[, 1],
  PC2 = pca$x[, 2],
  cluster = as.factor(clusters)
)

my_cols <- c(
  "1" = "#88e788",
  "2" = "#F78793",
  "3" = "#537490"
)

p_pca <- ggplot(plot_df, aes(PC1, PC2, color = cluster)) +
  geom_point(size = 2.5, alpha = 0.85) +
  geom_text_repel(aes(label = province), size = 3, max.overlaps = 20) +
  scale_colour_manual(values = my_cols) +
  labs(x = pc1_lab, y = pc2_lab, color = "Cluster") +
  theme_minimal(base_size = 12) +
  theme(legend.position = "none", panel.grid.minor = element_blank())

source("R/utils/plot_on_china_map.R")

plot_data <- province_groups %>%
  mutate(k3_groups = as.factor(k3_groups))

china_plot <- plot_on_china_map(
  plot_data,
  plot_variable = "k3_groups",
  breaks = c("1", "2", "3", "no data"),
  na_value = "no data",
  labels = c("Cluster 1", "Cluster 2", "Cluster 3", "No data"),
  legend_title = "",
  color_pallette = my_cols
)

p_combined <- p_pca + china_plot +
  theme(legend.position = "right")

ggsave(
  file.path(figures_dir, "kmeans_pca_china_map.png"),
  p_combined,
  width = 12,
  height = 6,
  dpi = 150
)

message("Wrote figures to ", figures_dir, "/")
