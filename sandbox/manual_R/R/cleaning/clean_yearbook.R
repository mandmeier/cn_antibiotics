# clean Yearbook


yearbook_raw <- read_csv("data/raw/yearbook.csv")



yearbook_cleaned <- yearbook_raw %>%
  rename(province = Province) %>%
  # rename Tibet
  mutate(province = recode(province, "Xizang" = "Tibet")) %>%
  # remove non-numeric values
  mutate(across(-1, ~ as.numeric(.)))


readr::write_csv(yearbook_cleaned, "data/cleaned/yearbook_cleaned.csv")
