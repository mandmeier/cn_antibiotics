# Compare antibiotic naming between environmental and resistance cleaned datasets.
# Combo names in resistance (e.g. Trimethoprim/Sulfamethoxazole) are intentionally not split.

env_path <- "data/cleaned/environmental_cleaned.csv"
res_path <- "data/cleaned/resistance_clean.csv"

environmental <- read_csv(env_path, show_col_types = FALSE)
resistance <- read_csv(res_path, show_col_types = FALSE)

env_abx <- sort(unique(environmental$antibiotic))
res_abx <- sort(unique(resistance$antibiotic))
both <- intersect(env_abx, res_abx)
only_env <- setdiff(env_abx, res_abx)
only_res <- setdiff(res_abx, env_abx)

cat("Environmental antibiotics:", length(env_abx), "\n")
cat("Resistance antibiotics:", length(res_abx), "\n")
cat("Exact matches (both datasets):", length(both), "\n")
cat("Environmental only:", length(only_env), "\n")
cat("Resistance only:", length(only_res), "\n\n")

cat("Shared names:\n")
print(both)

spelling_flags <- character()
if (any(grepl("Polymixin", res_abx, fixed = TRUE))) {
  spelling_flags <- c(spelling_flags, "Polymixin (should be Polymyxin)")
}
if ("Penicillin" %in% env_abx) {
  spelling_flags <- c(spelling_flags, "Penicillin without G in environmental data")
}
if (length(spelling_flags) > 0) {
  cat("\nSpelling issues:\n")
  print(spelling_flags)
} else {
  cat("\nNo known spelling issues flagged.\n")
}

cat(
  "\nNote: resistance combo drugs (e.g. Trimethoprim/Sulfamethoxazole) and",
  "environmental-only monitoring targets are expected naming differences.\n"
)

if (length(spelling_flags) > 0) {
  stop("Antibiotic naming verification failed.")
}

cat("Antibiotic naming verification passed.\n")
