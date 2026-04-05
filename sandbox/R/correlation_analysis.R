# Spearman Rank Correlation



# load integrated data
province_stats <- read_csv("antibiotics_data/clean_data/province_stats.csv")



summary(vars)

# create correlation matrix
vars <- province_stats %>%
  dplyr::select(-province) %>%
  # log transform
  mutate(across(ends_with("ug_kg"), ~ log10(.x + 0.01)))

cor_mat <- cor(vars, use = "pairwise.complete.obs", method = "spearman")

library(Hmisc)

res <- rcorr(as.matrix(vars), type = "spearman")

cor_mat <- res$r
p_mat  <- res$P



# convert to data frame
cor_df <- cor_mat %>%
  as.data.frame() %>%
  mutate(var1 = rownames(.)) %>%
  pivot_longer(-var1, names_to = "var2", values_to = "correlation") %>%
  filter(var1 != var2) %>%
  # remove duplicates
  mutate(pair = paste(pmin(var1, var2), pmax(var1, var2))) %>%
  distinct(pair, .keep_all = TRUE) %>%
  select(-pair)


# convert to data frame
p_df <- p_mat %>%
  as.data.frame() %>%
  mutate(var1 = rownames(.)) %>%
  pivot_longer(-var1, names_to = "var2", values_to = "p_value") %>%
  filter(var1 != var2) %>%
  # remove duplicates
  mutate(pair = paste(pmin(var1, var2), pmax(var1, var2))) %>%
  distinct(pair, .keep_all = TRUE) %>%
  select(-pair)


correlations <- cor_df %>%
  left_join(p_df) %>%
  # subset only interesting comparisons
  filter(grepl("sediments|municipal|soil|resistant_pc", var1)) %>%
  filter(!grepl("sediments|municipal|soil", var2))

municipal_sludge <- correlations %>%
  filter(grepl("municipal sludge", var1)) %>%
  filter(p_value < 0.05)


sediments <- correlations %>%
  filter(grepl("sediments", var1)) %>%
  filter(p_value < 0.05) %>%
  filter(abs(correlation) > 0.8)

soil <- correlations %>%
  filter(grepl("soil", var1))



# Treatment_of_Solid_Waste_10000_yuan

treatment <- correlations %>%
  filter(grepl("Treatment_of_Solid_Waste", var2)) %>%
  filter(p_value < 0.05)



total <- correlations %>%
  filter(p_value < 0.05) %>%
  group_by(var1) %>%
  add_tally()
