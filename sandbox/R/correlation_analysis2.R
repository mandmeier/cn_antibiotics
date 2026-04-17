


province_data_wide <- read_csv("data/analysis_ready/province_data_wide.csv")


# function to count complete pairs
count_complete_pairs <- function(data) {
  vars <- data %>%
    dplyr::select(-province) %>%
    mutate(across(everything(), ~ log10(.x + 0.01)))

  var_names <- names(vars)

  # all unique pairs
  pairs <- t(combn(var_names, 2)) %>%
    as.data.frame(stringsAsFactors = FALSE)

  names(pairs) <- c("var1", "var2")

  pairs %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      n_complete = sum(
        is.finite(vars[[var1]]) &
          is.finite(vars[[var2]])
      )
    ) %>%
    dplyr::ungroup()
}

pair_counts <- count_complete_pairs(province_data_wide)

# create correlation matrix
vars <- province_data_wide %>%
  dplyr::select(-province) %>%
  # log transform
  mutate(across(ends_with("ug_kg"), ~ log10(.x + 0.01)))


cor_mat <- cor(vars, use = "pairwise.complete.obs", method = "spearman")


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
  filter(!grepl("sediments|municipal|soil", var2)) %>%
  # filter only correlations that have data in BOTH var1 and var2 for at least n provinces
  left_join(pair_counts, by = c("var1", "var2")) %>%
  filter(n_complete >= 10)

municipal_sludge <- correlations %>%
  filter(grepl("municipal sludge", var1)) %>%
  filter(p_value < 0.05)


sediments <- correlations %>%
  filter(grepl("sediments", var1)) %>%
  filter(p_value < 0.05) %>%
  mutate(antibiotic = str_extract(var1, "(?<=_)[^_]+(?=_)")) %>%
  rowwise() %>%
  mutate(same = grepl(antibiotic, var2)) #%>%
  #filter(same)

soil <- correlations %>%
  filter(grepl("soil", var1)) %>%
  mutate(antibiotic = str_extract(var1, "(?<=_)[^_]+(?=_)")) %>%
  rowwise() %>%
  mutate(same = grepl(antibiotic, var2)) %>%
  filter(abs(correlation) > 0.7) %>%
  filter(p_value < 0.05) #%>%
  #filter(same)


## AB concentrations vs yearbook values

yb <- read_csv(file = "data/cleaned/yearbook_cleaned.csv")


yb_cor <- correlations %>%
  filter(!grepl("_pc", var2)) %>%
  filter(abs(correlation) > 0.6) %>%
  filter(p_value < 0.05)







### Chloramphenicol correlation plot

soil_Chloramphenicol_ug_kg


plot_dat <- province_data_wide %>%
  select(province, soil_Chloramphenicol_ug_kg, E_coli_Chloramphenicol_resistant_pc)


ggplot(plot_dat, aes(x = soil_Chloramphenicol_ug_kg, y = E_coli_Chloramphenicol_resistant_pc)) +
  geom_point(aes(color = province), alpha = 0.7) +
  geom_smooth(method = "lm", se = TRUE) +
  #facet_wrap( ~ ABX, scales = "free") +
  scale_x_log10() +
  stat_cor(method = "pearson", label.x.npc = "left", label.y.npc = "top") +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(
    x = "Chloramphenicol concentration in soil [μg/kg]",
    y = "% Chloramphenicol Resistant Microbial Strains in Clinical Samples",
    color = "Province"
  )







plot_correlation <- function(var1, var2, data = province_data_wide) {

  # keep province for plotting
  df <- data %>%
    dplyr::select(province, all_of(c(var1, var2)))

  # compute correlation on transformed data (exclude province)
  df_log <- df %>%
    dplyr::select(-province) %>%
    mutate(across(everything(), ~ log10(.x + 0.01)))

  res <- Hmisc::rcorr(as.matrix(df_log), type = "spearman")
  cor_val <- res$r[1, 2]
  p_val   <- res$P[1, 2]

  # filter for plotting (log scale needs positive finite values)
  df_plot <- df %>%
    filter(
      .data[[var1]] > 0,
      .data[[var2]] > 0,
      is.finite(.data[[var1]]),
      is.finite(.data[[var2]])
    )

  # label
  label <- sprintf("Spearman ρ = %.2f\np = %.2g", cor_val, p_val)

  ggplot(df_plot, aes(x = .data[[var1]], y = .data[[var2]])) +
    geom_point(aes(color = province), alpha = 0.7) +
    geom_smooth(method = "lm", se = TRUE, color = "blue") +
    scale_x_log10() +
    scale_y_log10() +
    annotate(
      "text",
      x = Inf, y = Inf,
      label = label,
      hjust = 1.1, vjust = 1.5,
      size = 5
    ) +
    labs(
      x = var1,
      y = var2,
      color = "Province",
      title = paste("Correlation:", var1, "vs", var2)
    ) +
    theme_minimal() +
    theme(legend.position = "bottom")
}


plot_correlation("soil_Chloramphenicol_ug_kg", "E_coli_Chloramphenicol_resistant_pc")




plot_correlation(var1 = "sediments_Sulfamerazine_ug_kg", var2 = "S_aureus_Cefoxitin_resistant_pc")

