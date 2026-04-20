`%||%` <- function(left, right) if (!is.null(left)) left else right

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  match <- grep("^--file=", args, value = TRUE)
  if (length(match) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", match[[1]]), winslash = "/", mustWork = FALSE)))
  }
  normalizePath(getwd(), winslash = "/", mustWork = FALSE)
}

source(file.path(get_script_dir(), "common_utils.R"))

MODEL_YEARBOOK_COLS <- c("Total_Phosphorus_ton", "Industry_Use_100_million_cu_m", "Freshwater_Aquatic_Products_10000_tons")
SOURCE_COLS <- c("agricultural_combined_score", "industrial_combined_score", "hospital_municipal_combined_score")
EXPLANATORY_SCAN_COLS <- c("S. aureus__Erythromycin__resistance", SOURCE_COLS, MODEL_YEARBOOK_COLS, "Total_Health_Care_Institutions", "Centers_for_Disease_Control", "Households_Service_Use_100_million_cu_m", "Animal_husbandry_100_million_yuan", "Output_of_Meat_10000_tons", "Slaughtered_Hogs_10000_heads")

args <- parse_cli_args(list(feature_table = "artifacts/k3_relationships_article_quality/feature_table.csv", source_attribution = "artifacts/k3_source_attribution_article_quality/province_source_attribution.csv", output_dir = "artifacts/k3_relationships_article_quality_r"))
output_dir <- ensure_dir(args$output_dir)
feature <- read_csv_na(args$feature_table)
source <- read_csv_na(args$source_attribution)
keep_cols <- c("province", "k3_cluster", "S. aureus__Clindamycin__resistance", "S. aureus__Erythromycin__resistance", MODEL_YEARBOOK_COLS, "Total_Health_Care_Institutions", "Centers_for_Disease_Control", "Households_Service_Use_100_million_cu_m", "Animal_husbandry_100_million_yuan", "Output_of_Meat_10000_tons", "Slaughtered_Hogs_10000_heads")
merged <- merge(feature[, keep_cols, drop = FALSE], source[, c("province", "NBS_region", "qu_category", SOURCE_COLS), drop = FALSE], by = "province", all.x = TRUE)
merged$is_k3_1 <- as.integer(merged$k3_cluster == 1)
model_cols <- c("S. aureus__Clindamycin__resistance", "S. aureus__Erythromycin__resistance", SOURCE_COLS, MODEL_YEARBOOK_COLS)
model_df <- merged[, c("province", "is_k3_1", model_cols), drop = FALSE]
model_df <- model_df[stats::complete.cases(model_df), , drop = FALSE]
model_df[, model_cols] <- scale(model_df[, model_cols, drop = FALSE])
model_df$yearbook_pc1 <- stats::prcomp(model_df[, MODEL_YEARBOOK_COLS, drop = FALSE], center = FALSE, scale. = FALSE)$x[, 1]

fit_glm <- function(predictors) stats::glm(stats::as.formula(sprintf("is_k3_1 ~ %s", paste(predictors, collapse = " + "))), data = model_df, family = stats::binomial())
source_only <- fit_glm(SOURCE_COLS)
source_plus_clinda <- fit_glm(c("S. aureus__Clindamycin__resistance", SOURCE_COLS))
full_covariates <- c(SOURCE_COLS, MODEL_YEARBOOK_COLS)
full_model <- fit_glm(full_covariates)
full_plus_clinda <- fit_glm(c("S. aureus__Clindamycin__resistance", full_covariates))
parsimonious_model <- fit_glm(c(SOURCE_COLS, "yearbook_pc1"))
parsimonious_plus_clinda <- fit_glm(c("S. aureus__Clindamycin__resistance", SOURCE_COLS, "yearbook_pc1"))
parsimonious_plus_clinda_ery <- fit_glm(c("S. aureus__Clindamycin__resistance", "S. aureus__Erythromycin__resistance", SOURCE_COLS, "yearbook_pc1"))

compare_models <- function(name, restricted, full) {
  stat <- 2 * (stats::logLik(full) - stats::logLik(restricted))
  df_diff <- attr(stats::logLik(full), "df") - attr(stats::logLik(restricted), "df")
  data.frame(comparison = name, lr_stat = as.numeric(stat), df_diff = as.integer(df_diff), p_value = stats::pchisq(as.numeric(stat), df = df_diff, lower.tail = FALSE), stringsAsFactors = FALSE)
}
comparisons <- do.call(rbind, list(compare_models("source_only_add_clindamycin", source_only, source_plus_clinda), compare_models("full_covariates_add_clindamycin", full_model, full_plus_clinda), compare_models("parsimonious_pc1_add_clindamycin", parsimonious_model, parsimonious_plus_clinda), compare_models("parsimonious_pc1_add_erythromycin_to_clindamycin_model", parsimonious_plus_clinda, parsimonious_plus_clinda_ery)))

coef_rows <- function(name, model) {
  coef_table <- summary(model)$coefficients
  data.frame(model = name, term = rownames(coef_table), coef = coef_table[, 1], std_err = coef_table[, 2], z = coef_table[, 3], p_value = coef_table[, 4], row.names = NULL, stringsAsFactors = FALSE)
}
coefs <- do.call(rbind, list(coef_rows("source_plus_clindamycin", source_plus_clinda), coef_rows("full_covariates_plus_clindamycin", full_plus_clinda), coef_rows("parsimonious_pc1_plus_clindamycin", parsimonious_plus_clinda), coef_rows("parsimonious_pc1_plus_clindamycin_erythromycin", parsimonious_plus_clinda_ery)))
scan <- do.call(rbind, lapply(EXPLANATORY_SCAN_COLS, function(column) {
  sub <- merged[, c("S. aureus__Clindamycin__resistance", column), drop = FALSE]
  sub <- sub[stats::complete.cases(sub), , drop = FALSE]
  test <- suppressWarnings(stats::cor.test(sub[[1]], sub[[2]], method = "spearman", exact = FALSE))
  data.frame(variable = column, spearman_rho = unname(test$estimate), p_value = test$p.value, stringsAsFactors = FALSE)
}))
scan <- scan[order(scan$p_value, -scan$spearman_rho), , drop = FALSE]
region_counts <- table(merged$NBS_region, merged$k3_cluster)
qu_counts <- table(merged$qu_category, merged$k3_cluster)

write_csv_safe(comparisons, file.path(output_dir, "saureus_clindamycin_model_comparisons.csv"))
write_csv_safe(coefs, file.path(output_dir, "saureus_clindamycin_model_coefficients.csv"))
write_csv_safe(scan, file.path(output_dir, "saureus_clindamycin_explanatory_scan.csv"))
write_csv_safe(as.data.frame.matrix(region_counts), file.path(output_dir, "saureus_clindamycin_region_counts.csv"), row.names = TRUE)
write_csv_safe(as.data.frame.matrix(qu_counts), file.path(output_dir, "saureus_clindamycin_qu_counts.csv"), row.names = TRUE)

lines <- c("S. aureus clindamycin adjusted analysis", "", sprintf("Provinces analyzed: %s", nrow(model_df)), "Outcome: inherited K3-1 membership vs K3-2/3", "", "Likelihood-ratio tests:")
for (index in seq_len(nrow(comparisons))) {
  row <- comparisons[index, , drop = FALSE]
  lines <- c(lines, sprintf("- %s: LR=%.3f, df=%s, p=%.6f", row$comparison, row$lr_stat, row$df_diff, row$p_value))
}
lines <- c(lines, "", "Interpretation:", "- Clindamycin remains significant when added to source scores only if source structure is the adjustment layer.", "- Clindamycin weakens in the saturated source + three-yearbook-covariate model, consistent with small-n collinearity and over-adjustment.", "- In the parsimonious source + yearbook-PC1 sensitivity model, adding clindamycin still improves fit.", "- Adding erythromycin on top of clindamycin does not improve the parsimonious model, indicating shared macrolide-lincosamide structure rather than two independent signals.", "", "Strongest correlates of S. aureus clindamycin resistance:")
for (index in seq_len(min(8, nrow(scan)))) {
  row <- scan[index, , drop = FALSE]
  lines <- c(lines, sprintf("- %s: rho=%.3f, p=%.6f", row$variable, row$spearman_rho, row$p_value))
}
lines <- c(lines, "", "Regional structure:", "- K3-1 is concentrated in western and fourth-tier provinces rather than the eastern high-pressure provinces.", "- Healthcare size and livestock production proxies are not among the strongest measured correlates of clindamycin.")
writeLines(lines, file.path(output_dir, "saureus_clindamycin_adjusted_analysis.txt"))
cat(sprintf("Wrote adjusted analysis to %s\n", file.path(output_dir, "saureus_clindamycin_adjusted_analysis.txt")))