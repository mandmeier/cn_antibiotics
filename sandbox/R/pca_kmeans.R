# PCA and clustering: see if there are clusters of samples in CARSS data

#
#
# library(FactoMineR)
# library(factoextra)


# decision tree, random forest, support vector machine




carss_cleaned <- read_csv("antibiotics_data/clean_data/carss_cleaned.csv")



# reshape
data_wide <- carss_cleaned %>%
  pivot_wider(names_from = c(bacteria, abx),
              values_from = resistant_pc)

# remove province column for clustering
mat <- as.matrix(data_wide[,-1])
rownames(mat) <- data_wide$province

mat <- scale(mat)
# distance + clustering
dist_mat <- dist(mat, method = "euclidean")
hc <- hclust(dist_mat, method = "ward.D2")

plot(hc)


mat[is.na(mat)] <- mean(mat[, col(mat)[is.na(mat)]], na.rm = TRUE)
pca <- prcomp(mat, scale. = TRUE)

# plot(pca, type = "l")

# visualize
# plot(pca$x[,1:2])

# cluster on PCA scores
# km <- kmeans(pca$x[,1:5], centers = 3)

#
# plot(pca$x[,1:2],
#      xlab = "PC1",
#      ylab = "PC2",
#      pch = 19)
#
# text(pca$x[,1:2],
#      labels = rownames(mat),
#      pos = 3,
#      cex = 0.7)


set.seed(123)

# use first few PCs (e.g., 3–5)
pc_scores <- pca$x[,1:4]

km <- kmeans(pc_scores, centers = 4)

# plot with colors
plot(pca$x[,1:2],
     col = km$cluster,
     pch = 19,
     xlab = "PC1",
     ylab = "PC2")

text(pca$x[,1:2],
     labels = rownames(mat),
     pos = 3,
     cex = 0.7)



# which have higher vs lower total resistance?



cluster_df <- data.frame(
  province = rownames(mat),
  cluster = factor(km$cluster)
)

carss_groups <- carss_cleaned %>%
  left_join(cluster_df, by = "province")

province_summary <- carss_groups %>%
  group_by(province, cluster) %>%
  summarise(mean_resistance = mean(resistant_pc, na.rm = TRUE),
            .groups = "drop")

#
# antibiotic_summary <- carss_groups %>%
#   group_by(province, abx) %>%
#   summarise(mean_resistance = mean(resistant_pc, na.rm = TRUE),
#             .groups = "drop")




library(dplyr)

prov_ab_summary <- carss_groups %>%
  group_by(abx, province, cluster) %>%
  summarise(mean_resistance = mean(resistant_pc, na.rm = TRUE),
            .groups = "drop")

library(tidytext)  # for reorder_within

prov_ab_summary <- prov_ab_summary %>%
  mutate(province_ordered = reorder_within(province,
                                           mean_resistance,
                                           antibiotic))

install.packages("tidytext")


province_summary <- province_summary %>%
  arrange(mean_resistance) %>%
  mutate(province = factor(province, levels = province))

cluster_colors <- c(
  "1" = "#000000",
  "2" = "#df536b",
  "3" = "#61d04f",
  "4" = "#2297e6"
)

ggplot(province_summary,
       aes(x = province,
           y = mean_resistance,
           fill = cluster)) +
  geom_col() +
  coord_flip() +
  scale_fill_manual(values = cluster_colors) +
  labs(x = "Province",
       y = "Mean Resistance (%)",
       fill = "Cluster",
       title = "Mean Antibiotic Resistance by Province (Ranked)") +
  theme_minimal()


# plot on map


cluster_df <- data.frame(
  province = rownames(mat),
  cluster = factor(km$cluster)
)

library(ggplot2)
library(maps)
library(mapdata)

china_map <- map_data("china")














df <- carss %>%

  mutate(sample_id = paste(province, bacteria, sep = "_"), .before = 1)


test <- df %>%
  dplyr::summarise(n = dplyr::n(), .by = c(CARSS_abx, sample_id)) %>%
  dplyr::filter(n > 1L)

# wider format for PCA
df_wide <- df %>%
  pivot_wider(
    id_cols = CARSS_abx,
    names_from = sample_id,
    values_from = resistant_pc,
    values_fill = NA
  )

df_numeric <- df_wide %>%
  select(-sample_id) %>%
  select(where(~ var(., na.rm = TRUE) > 0))
#
# df_numeric_log <- df_numeric %>%
#   mutate(across(everything(), ~log10(. + 1)))
#


pca_res <- prcomp(df_numeric, scale. = TRUE)


pca_df <- as.data.frame(pca_res$x)
pca_df$sample_id <- df_wide$sample_id

pca_df$sample_type <- df$sample_type[match(pca_df$sample_id, df$sample_id)]


ggplot(pca_df, aes(PC1, PC2, color = sample_type)) +
  geom_point()



# Kmeans clustering

set.seed(123)
km_res <- kmeans(df_numeric, centers = 3)  # try different k

fviz_cluster(km_res, data = df_numeric_log)


