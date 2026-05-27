# k-means and PCA of by-province environmental and clinical antibiotics metrics


## use zscores of all metrics to cluster provinces (k-means)
## rationale: the higher these mertics, the more of an antibiotic problem they have
## use these groups for comparisons.
## resulting groups of provinces differ by "how severe of an antibiotics problem they have"

# 1) log transform values (because antibiotics concentrtions may differ by orders of magnitude)
# 2) calculate z-scores of log transformed values (centers around mean of 0):
# pos.values: antibiotics problem worse than china avarage
# neg.values: antibiotics problem better than china avarage


antibiotic_metrics_china <- read_csv("data/analysis_ready/antibiotic_metrics_china.csv")

china_metrics <- antibiotic_metrics_china %>%
  # remove Hong Kong (no data for antibiotics resistance)
  filter(province != "Hong Kong") %>%


  # use only resistance data %>%
  #filter(grepl("resistance", metric)) %>%

  # use only soil data
  # filter(!grepl("resistance", metric)) %>%

  # exclude municipal sludge
  #filter(metric != "municipal sludge_concentration") %>%

  # only municipal sludge
  # filter(metric == "municipal sludge_concentration") %>%
  # failed, only 2

  # only sediment
  # filter(metric == "sediment_concentration") %>%

  # only soil
  # filter(metric == "soil_concentration") %>%


  mutate(log10_value = log10(value)) %>%
  mutate(z_score = as.numeric(scale(log10_value)))


### create wide format matrix
# rows: provinces, columns: antibiotic x metric
df_wide_mat <- china_metrics %>%
  mutate(feature = paste(antibiotic, metric, sep = "__")) %>%
  select(province, feature, z_score) %>%
  pivot_wider(
    names_from = feature,
    values_from = z_score
  ) %>%
  column_to_rownames("province") %>%
  as.matrix()
# fill gaps with 0 (best for z_scores)

df_wide_mat[is.na(df_wide_mat)] <- 0


z_scores_antibiotics_metrics <- as.data.frame(df_wide_mat) %>%
  rownames_to_column(var = "province")

# write_csv(z_scores_antibiotics_metrics, "data/analysis_ready/z_scores_antibiotics_metrics.csv")


# calculate principal components from z-score matrix
pca <- prcomp(df_wide_mat, center = FALSE, scale. = FALSE)



# use first few PCs
pc_scores <- pca$x[,1:4]
set.seed(42)

# K-means clustering


kmeans2_pca <- kmeans(pc_scores, centers = 2, nstart = 25)
clusters2 <- kmeans2_pca$cluster


kmeans3_pca <- kmeans(pc_scores, centers = 3, nstart = 25)
clusters3 <- kmeans3_pca$cluster


kmeans4_pca <- kmeans(pc_scores, centers = 4, nstart = 25)
clusters4 <- kmeans4_pca$cluster



cluster_df <- data.frame(
  province = rownames(pc_scores),
  k2_groups = factor(clusters2),
  k3_groups = factor(clusters3),
  k4_groups = factor(clusters4)
) %>%
  remove_rownames() %>%
  arrange(k3_groups)
#




province_groups <- read_csv("data/cleaned/province_groups.csv")



# add cluster grouping to province groups
province_groups <- province_groups %>%
  select(-k2_groups, -k3_groups, -k4_groups) %>%
  left_join(cluster_df)

# write_csv(province_groups, "data/cleaned/province_groups.csv")



library(ggplot2)
library(ggrepel)
library(patchwork)



clusters <- clusters2

plot_df <- data.frame(
  province = rownames(pca$x),
  PC1 = pca$x[,1],
  PC2 = pca$x[,2],
  PC3 = pca$x[,3],
  #PC3 = pca$x[,4],
  cluster = as.factor(clusters)
)

var_exp <- summary(pca)$importance[2,]

pc1_lab <- paste0("PC1 (", round(var_exp[1] * 100, 1), "%)")
pc2_lab <- paste0("PC2 (", round(var_exp[2] * 100, 1), "%)")
pc3_lab <- paste0("PC3 (", round(var_exp[3] * 100, 1), "%)")

# PC1 vs PC2
p1 <- ggplot(plot_df, aes(PC1, PC2, color = cluster)) +
  geom_point(size = 2, alpha = 0.85) +
  geom_text_repel(aes(label = province),
                  size = 3,
                  max.overlaps = 20) +
  scale_color_brewer(palette = "Set1") +
  labs(
    title = "PC1 vs PC2",
    x = pc1_lab,
    y = pc2_lab,
    color = "Cluster"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "right",
    panel.grid.minor = element_blank()
  )

p1


# PC2 vs PC3
p2 <- ggplot(plot_df, aes(PC2, PC3, color = cluster)) +
  geom_point(size = 2, alpha = 0.85) +
  geom_text_repel(aes(label = province),
                  size = 2,
                  max.overlaps = 20) +
  scale_color_brewer(palette = "Set1") +
  labs(
    #title = "PC2 vs PC3",
    x = pc2_lab,
    y = pc3_lab,
    color = "Cluster"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

p2

# combine plots
p_combined <- p1 + p2 +
  plot_layout(guides = "collect") &
  theme(legend.position = "right")

p_combined




## 3D plot first 3 PCs

library(plotly)

d3 <- plot_ly(
  x = pca$x[,1],
  y = pca$x[,2],
  z = pca$x[,3],
  type = "scatter3d",
  mode = "markers",
  color = as.factor(clusters),
  colors = "Set1",
  marker = list(size = 5),
  text = rownames(df_wide_mat)
) %>%
  layout(
    scene = list(
      xaxis = list(title = "PC1"),
      yaxis = list(title = "PC2"),
      zaxis = list(title = "PC3")
    )
  )


d3




#### Figure Cluster PCA next to China plot


# PC plot
# PC1 vs PC2
p1 <- ggplot(plot_df, aes(PC1, PC2, color = cluster)) +
  geom_point(size = 2.5, alpha = 0.85) +
  geom_text_repel(aes(label = province),
                  size = 3,
                  max.overlaps = 20) +
  scale_colour_manual(values = c("#537490", "#F78793")) +
  labs(
    #title = "PC1 vs PC2",
    x = pc1_lab,
    y = pc2_lab,
    color = "Cluster"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    legend.position = "none",
    panel.grid.minor = element_blank()
  )

p1




source("R/utils/plot_on_china_map.R")
#
# my_cols <- c(
#   "1" = "#F78793",
#   "2" = "#537490"
# )


my_cols <- c(
  "1" = "#537490",
  "2" = "#F78793"
)





plot_data <- province_groups %>%
  mutate(k2_groups = as.factor(k2_groups))


china_plot <- plot_on_china_map(
  plot_data,
  plot_variable = "k2_groups",
  breaks = c("1", "2", "no data"),
  na_value = "no data",
  labels = c("Cluster 1", "Cluster 2", "No data"),
  legend_title = "",
  color_pallette = my_cols
)

china_plot





# combine plots
p_combined <- p1 + china_plot +
  #plot_layout(guides = "collect") &
  theme(legend.position = "right")

p_combined

