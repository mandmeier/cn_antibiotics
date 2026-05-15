#### Create ONE value per specimen_type, province, antibiotic
#### Select median measured value for each sample_type, province, antibiotic
# goal is to get ONE representative measurement per sample_type, antibiotic and province
# median does not exclude data, and as opposed to using mean or max, we are not susceptible to one-off high measurements from heavily polluted sites


env_abx_harmonized <- read_csv("data/cleaned/env_abx_harmonized.csv")

env_abx_per_province <- env_abx_harmonized %>%

  # remove measurements with no mean concentration
  filter(!is.na(mean_concentration)) %>%

  select(sample_type, province, location, antibiotic, mean_concentration, concentration_unit, sample_year, season, full_reference_name, reference_number) %>%
  unique() %>%
  group_by(sample_type, province, antibiotic) %>%

  # get median reported concentration for all specimens taken in the province
  # need to treat zero values in a defensible way. Likely not true zeroes, but below limit of detection.
  # estimating LOD as "lowest measured value among all measurements in a group / 2"
  ## in cases where NO values are reported > 0 we assume a real NA value (no data measured). These cases are removed
  mutate(
    lod_est = {
      positives <- mean_concentration[mean_concentration > 0]

      if (length(positives) == 0) {
        NA_real_
      } else {
        min(positives, na.rm = TRUE)
      }
    }
  ) %>%
  # replace with LLOD/2 if reported as zero
  mutate(
    mean_concentration = case_when(
      mean_concentration == 0 & !is.na(lod_est) ~ lod_est/2,
      mean_concentration == 0 & is.na(lod_est)  ~ NA_real_,
      TRUE ~ mean_concentration
    )
  ) %>%
  # calculate median
  mutate(median_concentration = median(mean_concentration)) %>%
  select(-mean_concentration) %>%
  # remove true NA measurements (nothing measured for this antibiotic in this province)
  filter(!is.na(median_concentration)) %>%
  unique() %>%
  add_tally() %>%
  # in case of tie use first publication with the reported value
  slice_min(reference_number, n = 1, with_ties = FALSE) %>%
  arrange(sample_type, province, antibiotic) %>%
  select(-n) %>%
  relocate(median_concentration, .after = "antibiotic") %>%
  ungroup() %>%
  select(-lod_est) %>%
  # filter missing antibiotics
  filter(!is.na(antibiotic))

write_csv(env_abx_per_province, "data/analysis_ready/env_abx_per_province.csv")
