#### Create ONE value per sample_type, location, season, antibiotic
#### Same median / LOD/2 rules as R/04_env_abx_per_province.R, keyed by site
# goal is to get ONE representative measurement per sample_type, antibiotic,
# location, and season for users who need sites rather than provinces.

source("R/utils/reproducible_csv.R")

environmental <- read_csv(
  "data/intermediate/environmental_by_site.csv",
  show_col_types = FALSE
)

env_abx_per_site <- environmental %>%

  # remove measurements with no mean concentration
  filter(!is.na(mean_concentration)) %>%

  # remove Liaoning/Tianjin or nationwide ambiguous location
  filter(province != "Liaoning/Tianjin") %>%
  filter(province != "Jiangsu/Zhejiang") %>%
  filter(!grepl("Nation", province)) %>%

  # require a site label; province-only rows belong in the province product
  filter(!is.na(location), nzchar(trimws(as.character(location)))) %>%

  select(
    sample_type,
    province,
    location,
    season,
    lon,
    lat,
    antibiotic,
    mean_concentration,
    concentration_unit,
    sample_year,
    reference_number
  ) %>%
  unique() %>%
  # Missing season is its own level (do not mix undated with seasonal rows).
  group_by(sample_type, location, season, antibiotic) %>%

  # get median reported concentration for all specimens at this site/season
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
      mean_concentration == 0 & !is.na(lod_est) ~ lod_est / 2,
      mean_concentration == 0 & is.na(lod_est) ~ NA_real_,
      TRUE ~ mean_concentration
    )
  ) %>%
  # calculate median
  mutate(median_concentration = median(mean_concentration, na.rm = TRUE)) %>%
  select(-mean_concentration) %>%
  # remove true NA measurements (nothing measured for this antibiotic at this site)
  filter(!is.na(median_concentration)) %>%
  unique() %>%
  add_tally() %>%
  # in case of tie use first publication with the reported value
  slice_min(reference_number, n = 1, with_ties = FALSE) %>%
  arrange(sample_type, province, location, season, antibiotic) %>%
  select(-n) %>%
  relocate(median_concentration, .after = "antibiotic") %>%
  ungroup() %>%
  select(-lod_est) %>%
  # filter missing antibiotics
  filter(!is.na(antibiotic)) %>%
  relocate(province, location, season, lon, lat, .after = sample_type)

write_csv_reproducible(env_abx_per_site, "data/output/env_abx_per_site.csv")

message(
  "Wrote env_abx_per_site.csv: ", nrow(env_abx_per_site), " rows; ",
  n_distinct(env_abx_per_site$location), " locations"
)
