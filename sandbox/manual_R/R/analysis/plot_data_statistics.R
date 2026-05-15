### plot data statistics


env_abx_harmonized <- read.csv("data/cleaned/env_abx_harmonized.csv")

## Registered Environmental Records by Province

smry <- env_abx_harmonized %>%
  group_by(province ) %>%
  tally() %>%
  arrange(-n)



source("R/utils/plot_on_china_map.R")


my_cols <- c(
  "1" = "#FFF9BD",
  "3" = "#FC6832",
  "3" = "#F43E26",
  "3" = "#E6201D",
  "4" = "#800026"
)
plot_on_china_map(
  smry,
  plot_variable = "n",
  breaks = c(200, 400, 600, 800, 1000, 1200),
  #na_value = "no data",
  #labels = c("Cluster A", "Cluster B", "Cluster C", "No data"),
  legend_title = "Environmental\nRecords",
  color_pallette = my_cols,
  border_col = "#333333"
) +
  theme(legend.position = "right")




sample_smry <- env_abx_harmonized %>%
  group_by(sample_type) %>%
  dplyr::summarize(
    unique_antibiotics = length(unique(antibiotic)),
    n_records = n()) %>%
  mutate(sample_type = reorder(sample_type, -unique_antibiotics))

#
# ggplot(sample_smry, aes(x = sample_type,
#                y = unique_antibiotics,
#                size = n_records)) +
#   geom_point(alpha = 0.7) +
#   scale_size_continuous(range = c(2, 10)) +
#   theme_minimal() +
#   theme(axis.text.x = element_text(angle = 45, hjust = 1))
#



# 2. named color vector
sample_cols <- c(
  "municipal sludge" = "#8c510a",
  "municipal wastewater effluent" = "#01665e",
  "municipal wastewater influent" = "#5ab4ac",
  "sediment" = "#542788",
  "soil" = "#b2182b",
  "surface water" = "#2166ac"
)


# 3. plot
ggplot(sample_smry, aes(x = sample_type,
                        y = unique_antibiotics,
                        size = n_records,
                        color = sample_type)) +
  geom_point(alpha = 0.8) +
  scale_size_continuous(range = c(2, 10)) +
  scale_color_manual(values = sample_cols) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1),
        legend.title = element_blank())



ggplot(sample_smry, aes(x = 1,
                        y = unique_antibiotics,
                        size = n_records,
                        color = sample_type)) +
  geom_point(alpha = 0.8) +
  scale_size_continuous(range = c(2, 10)) +
  scale_color_manual(values = sample_cols) +
  scale_x_continuous(NULL, breaks = NULL) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.title.x = element_blank()
  )





library(forcats)


library(ggrepel)

ggplot(sample_smry, aes(x = n_records,
                        y = unique_antibiotics,
                        color = sample_type)) +
  geom_point(aes(size = n_records), alpha = 0.65) +
  scale_x_log10() +
  scale_size_continuous(range = c(2, 10)) +
  scale_color_manual(values = sample_cols) +

  geom_text_repel(
    aes(label = paste0(sample_type, " (n=", n_records, ")"),
        color = sample_type),
    size = 4,
    box.padding = 1.0,
    point.padding = 0.3,
    segment.color = "grey50",
    show.legend = FALSE
  ) +

  theme_minimal() +
  theme(legend.position = "none") +
  labs(x = "Number of records",
       y = "Unique antibiotics detected")


##



#Antibiotics: 100





carss <- read_csv("data/cleaned/carss_cleaned.csv")

env_abx_harmonized <- read_csv("data/cleaned/env_abx_harmonized.csv")

yearbook <- read_csv(file = "data/cleaned/yearbook_cleaned.csv")




# Environmental Records 5308, across 28 provinces
nrow(env_abx_harmonized)

# Registered antibiotics concentration values 7676
abx_measurements <- c(env_abx_harmonized$mean_concentration, env_abx_harmonized$max_concentration)
length(abx_measurements[!is.na(abx_measurements)])


# Sampling years 2004 -2025
min(env_abx_harmonized$sample_year, na.rm = TRUE)
#max(as.numeric(as.character(env_abx_harmonized$sample_year)), na.rm = TRUE)


# Environmental Matrices
# sample_type                     n_records
# 1 sediment                           3552
# 2 soil                               1150
# 3 municipal sludge                    490
# 4 surface water                        71
# 5 municipal wastewater influent        45

by_grp <- env_abx_harmonized %>%
  summarise(
    n_records = n(),
    .by = "sample_type"
  ) %>%
  arrange(-n_records)

# Clinical Antibiotics Resistance (CARSS)
# 5 bacteria
# 18 antibiotics
# 31 provinces
nrow(carss)

length(unique(carss$bacteria))
length(unique(carss$antibiotic))



# Statistical Yearbook Groups 93, 10 groups

length(colnames(yearbook))-1


nrow(carss)
length(unique(carss$antibiotic))


env_by_grp <- env_abx_harmonized %>%
  summarise(
    n_records = n(),
    .by = c("sample_type")
  ) %>%
  arrange(-n_records)



abx_by_grp <- carss %>%
  summarise(
    n_records = n(),
    .by = c("bacteria")
  ) %>%
  arrange(-n_records)



## unique antibiotics

env <- sort(unique(env_abx_harmonized$antibiotic))
res <- sort(unique(carss$antibiotic))

common <- intersect(env, res)
length(common)

env_only <- env[!env %in% res]
length(env_only)

res_only <- res[!res %in% env]
length(res_only)











test <- yearbook %>%
  pivot_longer(cols = 2:last_col(), names_to = "stat", values_to = "measurement") %>%
  filter(!is.na(measurement)) %>%
  nrow()



abx_grps <- env_abx_harmonized %>%
  summarise(
    n_abx = length(unique(antibiotic)),
    .by = c("group_of_antibiotic")
  ) %>%
  arrange(-n_abx)


unique(env_abx_harmonized$group_of_antibiotic)








# Prevalent antibiotics across major environmental matrices


library(dplyr)
library(tidytext)
library(ggplot2)

sample_abx_smry <- env_abx_harmonized %>%
  summarise(
    n_records = n(),
    .by = c(sample_type, antibiotic)
  ) %>%
  group_by(sample_type) %>%
  arrange(desc(n_records)) %>%
  slice_head(n = 10) %>%
  ungroup() %>%

  mutate(
    antibiotic = reorder_within(antibiotic, n_records, sample_type)
  ) %>%
  mutate(
    antibiotic = tidytext::reorder_within(antibiotic, n_records, sample_type)
  )

ggplot(sample_abx_smry, aes(x = antibiotic,
                            y = n_records,
                            fill = sample_type)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  facet_wrap(~ sample_type, scales = "free_y") +
  scale_fill_manual(values = sample_cols) +
  geom_text(aes(label = n_records),
            hjust = -0.2,
            size = 3) +
  scale_x_reordered() +
  scale_y_log10() +
  expand_limits(y = max(sample_abx_smry$n_records) * 1.8) +
  theme_minimal() +
  labs(x = "Antibiotic",
       y = "Number of records (log10 scale)")


