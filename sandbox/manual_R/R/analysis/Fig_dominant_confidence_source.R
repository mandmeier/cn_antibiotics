


source("R/utils/plot_on_china_map.R")

confidence_source <- read_csv("data/raw/province_source_confidence.csv")


###

province_ranked <- confidence_source %>%
  select(province, dominant_source_label, relative_margin) %>%
  group_by(dominant_source_label) %>%
  arrange(dominant_source_label, -relative_margin)



## add province geometries from shapefile
china_prov <- sf::st_read("data/raw/cn_shp/cn.shp")

china_prov_names <- china_prov %>%
  mutate(name = sub(" .*", "", name)) %>%
  rename(province = name) %>%
  select(province, geometry) %>%
  mutate(province = ifelse(province == "Inner", "Inner Mongolia", province)) %>%
  mutate(province = ifelse(province == "Hong", "Hong Kong", province))

plot_data <- province_ranked %>%
  full_join(china_prov_names, by = "province")



ggplot() +

  geom_sf(data = subset(plot_data, dominant_source_label == "Agricultural"),
          aes(fill = relative_margin, geometry = geometry)) +
  scale_fill_gradientn(colors = c("#d4e9dd", "#55A376")) +

  ggnewscale::new_scale_fill() +

  geom_sf(data = subset(plot_data, dominant_source_label == "Hospital/municipal"),
          aes(fill = relative_margin, geometry = geometry)) +
  scale_fill_gradientn(colors = c("#cae3ee", "#388BAE")) +

  ggnewscale::new_scale_fill() +

  geom_sf(data = subset(plot_data, dominant_source_label == "Industrial"),
          aes(fill = relative_margin, geometry = geometry)) +
  scale_fill_gradientn(colors = c("#f1d5d6", "#C6565A", "#8a2f32")) +

  ggnewscale::new_scale_fill() +

  geom_sf(data = subset(plot_data, dominant_source_label == "Mixed"),
          aes(fill = relative_margin, geometry = geometry)) +
  scale_fill_gradientn(colors = c("#eeeeee", "#bbbbbb", "#666666")) +

  theme_minimal() +
  theme(
    axis.text = element_blank(),
    axis.ticks = element_blank(),
    panel.grid = element_blank(),
    legend.position = "bottom"
  )




plot_data <- confidence_source %>%
  select(province, dominant_source_label, top_score, confidence_tier) %>%
  mutate(confidence_tier2 = case_when(confidence_tier == "high" ~ "***",
                                     confidence_tier == "moderate" ~ "**",
                                     confidence_tier == "low" ~ "*",
                                     confidence_tier == "indeterminate" ~ "indet.")) %>%
  #group_by(dominant_source_label, province) %>%
  arrange(dominant_source_label, desc(top_score)) %>%
  mutate(top_score = ifelse(top_score < 0, 1, top_score)) %>%
  mutate(
    province = factor(province, levels = rev(unique(province)))
  ) %>%
  mutate(dominant_source_label = factor(dominant_source_label, levels = rev(c("Agricultural", "Hospital/municipal", "Industrial", "Mixed"))))


 my_cols <- c(
   "Agricultural" = "#55A376",
   "Hospital/municipal" = "#388BAE",
   "Industrial" = "#C6565A",
   "Mixed" = "#666666"
 )



ggplot(plot_data, aes(x = top_score, y = province, fill = dominant_source_label)) +
  geom_col() +

  # text labels to the right of bars
  geom_text(
    aes(label = confidence_tier2),
    hjust = -0.1,   # push text slightly outside bar
    size = 3
  ) +

  scale_fill_manual(values = my_cols) +

  scale_x_log10() +

  labs(
    x = "Top score",
    y = NULL,
    fill = "Source"
  ) +

  theme_minimal() +
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(size = 10),
    legend.position = "bottom"
  ) +

  guides(fill = guide_legend(reverse = TRUE)) +

  # allow space for text outside bars
  expand_limits(x = max(plot_df$top_score) * 1.1)


