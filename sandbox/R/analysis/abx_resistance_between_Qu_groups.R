# test if antibiotic resistance (CARSS) differs between Qu groups

carss <- read_csv("data/cleaned/carss_cleaned.csv")
qu <- read_csv("data/cleaned/province_groups.csv")



# Merge resistance data with Qu categories
resistance_with_groups <- carss %>%
  left_join(qu, by = "province") %>%
  mutate(qu_category = as.factor(qu_category))



# Kruskal-Wallis test (non-parametric ANOVA) for resistance
resistance_stats <- resistance_with_groups %>%
  group_by(bacteria, antibiotic) %>%
  summarise(
    kruskal_p = kruskal.test(resistant_pc, qu_category)$p.value,
    mean_by_cat = list(tapply(resistant_pc, qu_category, mean, na.rm = TRUE)),
    .groups = 'drop'
  )

resistance_stats <- resistance_with_groups %>%
  group_by(bacteria, antibiotic) %>%
  mutate(kruskal_p = kruskal.test(resistant_pc, qu_category)$p.value) %>%
  group_by(bacteria, antibiotic, qu_category) %>%
  summarise(
    mean_by_cat = mean(resistant_pc, na.rm = TRUE),
    kruskal_p = first(kruskal_p),
    .groups = "drop"
  )

# View significant differences
sign <- resistance_stats %>%
  select(-qu_category, -mean_by_cat) %>%
  unique() %>%
  dplyr::filter(kruskal_p < 0.05)



library(FSA)

# For a specific antibiotic-bacteria combination
dunnTest(resistant_pc ~ qu_category,
         data = dplyr::filter(resistance_with_groups,
                       bacteria == "S. aureus",
                       antibiotic == "Penicillin G"),
         method = "bh")




# Boxplot: Resistance distribution by category for key antibiotic-bacteria
p1 <- ggplot(dplyr::filter(resistance_with_groups,
                    bacteria == "S. aureus",
                    antibiotic %in% c("Ciprofloxacin", "Clindamycin", "Levofloxacin", "Penicillin G", "Rifampicin")),
             aes(x = qu_category, y = resistant_pc, fill = qu_category)) +
  geom_boxplot(alpha = 0.7) +
  facet_wrap(~ antibiotic, scales = "free_y") +
  labs(title = "Antibiotic Resistance by Qu Category",
       x = "Qu Category", y = "Resistance (%)") +
  theme_minimal() +
  theme(legend.position = "none")

p1




p2 <- ggplot(dplyr::filter(resistance_with_groups,
                    bacteria == "S. aureus",
                    antibiotic %in% c("Ciprofloxacin", "Clindamycin", "Levofloxacin",
                                      "Penicillin G", "Rifampicin")),
             aes(x = qu_category, y = resistant_pc, fill = qu_category)) +
  geom_boxplot(alpha = 0.7) +
  stat_compare_means(method = "t.test",
                     label = "p.signif",  # shows *, **, ***
                     hide.ns = TRUE,       # hide non-significant
                     comparisons = list(
                       c("First", "Fourth"),
                       c("Second", "Fourth"),
                       c("First", "Second")
                     )) +
  facet_wrap(~ antibiotic, scales = "free_y") +
  labs(title = "Antibiotic Resistance by Qu Category",
       x = "Qu Category",
       y = "Resistance (%)") +
  theme_minimal() +
  theme(legend.position = "none")

p2




# First, ensure qu_category is an ordered factor
resistance_with_groups <- resistance_with_groups %>%
  mutate(qu_category = factor(qu_category,
                              levels = c("First", "Second", "Third", "Fourth"),
                              ordered = TRUE))

# Check the data distribution
resistance_with_groups %>%
  dplyr::filter(bacteria == "S. aureus",
         antibiotic %in% c("Clindamycin", "Penicillin G", "Rifampicin")) %>%
  group_by(antibiotic, qu_category) %>%
  summarise(n = n(), mean_res = mean(resistant_pc, na.rm = TRUE)) %>%
  print()

# Now create the plot with ALL pairwise comparisons
p <- ggplot(dplyr::filter(resistance_with_groups,
                   bacteria == "S. aureus",
                   antibiotic %in% c("Clindamycin", "Penicillin G", "Rifampicin")),
            aes(x = qu_category, y = resistant_pc, fill = qu_category)) +
  geom_boxplot(alpha = 0.7) +
  stat_compare_means(method = "t.test",
                     label = "p.signif",
                     hide.ns = TRUE,  # Set to FALSE to see all comparisons
                     comparisons = list(
                       c("First", "Second"),
                       c("First", "Third"),   # This should now appear
                       c("First", "Fourth"),
                       c("Second", "Third"),
                       c("Second", "Fourth"),
                       c("Third", "Fourth")
                     )) +
  facet_wrap(~ antibiotic, scales = "free_y") +
  labs(title = "S. Aureus Antibiotic Resistance by Qu Category",
       x = "Qu Category",
       y = "Resistance (%)") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 0.5))

p


### by region

# Now create the plot with ALL pairwise comparisons
p2 <- ggplot(dplyr::filter(resistance_with_groups,
                   bacteria == "S. aureus",
                   antibiotic %in% c("Clindamycin", "Penicillin G", "Rifampicin")),
            aes(x = NBS_region, y = resistant_pc, fill = NBS_region)) +
  geom_boxplot(alpha = 0.7) +
  stat_compare_means(method = "t.test",
                     label = "p.signif",
                     hide.ns = TRUE,  # Set to FALSE to see all comparisons
                     comparisons = list(
                       c("Central", "Eastern"),
                       c("Central", "Northeastern"),   # This should now appear
                       c("Central", "Western"),
                       c("Eastern", "Northeastern"),
                       c("Eastern", "Western"),
                       c("Northeastern", "Western")
                     )) +
  facet_wrap(~ antibiotic, scales = "free_y") +
  labs(title = "S. Aureus Antibiotic Resistance by NBS Region",
       x = "NBS Region",
       y = "Resistance (%)") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 0.5))

p2








# Now create the plot with ALL pairwise comparisons
p3 <- ggplot(dplyr::filter(resistance_with_groups,
                    bacteria %in% c("A. baumannii", "P. aeruginosa","S. aureus"),
                    antibiotic %in% c("Levofloxacin")),
             aes(x = qu_category, y = resistant_pc, fill = qu_category)) +
  geom_boxplot(alpha = 0.7) +
  stat_compare_means(method = "t.test",
                     label = "p.signif",
                     hide.ns = TRUE,  # Set to FALSE to see all comparisons
                     comparisons = list(
                       c("First", "Second"),
                       c("First", "Third"),   # This should now appear
                       c("First", "Fourth"),
                       c("Second", "Third"),
                       c("Second", "Fourth"),
                       c("Third", "Fourth")
                     )) +
  facet_wrap(~ bacteria, scales = "free_y") +
  labs(title = "Levofloxacin Resistance by NBS Region",
       x = "Qu Category",
       y = "Resistance (%)") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 0.5))

p3





# Now create the plot with ALL pairwise comparisons
p4 <- ggplot(filter(resistance_with_groups,
                    bacteria %in% c("A. baumannii", "P. aeruginosa","S. aureus"),
                    antibiotic %in% c("Levofloxacin")),
             aes(x = NBS_region, y = resistant_pc, fill = NBS_region)) +
  geom_boxplot(alpha = 0.7) +
  stat_compare_means(method = "t.test",
                     label = "p.signif",
                     hide.ns = TRUE,  # Set to FALSE to see all comparisons
                     comparisons = list(
                       c("Central", "Eastern"),
                       c("Central", "Northeastern"),   # This should now appear
                       c("Central", "Western"),
                       c("Eastern", "Northeastern"),
                       c("Eastern", "Western"),
                       c("Northeastern", "Western")
                     )) +
  facet_wrap(~ bacteria, scales = "free_y") +
  labs(title = "Levofloxacin Resistance by NBS Region",
       x = "NBS Region",
       y = "Resistance (%)") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  theme(legend.position = "none",
        axis.text.x = element_text(angle = 0, hjust = 0.5))

p4








