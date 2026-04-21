args <- commandArgs(trailingOnly = TRUE)

base_dir <- if (length(args) >= 1) args[[1]] else "."

summary_path <- file.path(base_dir, "artifacts", "matrix_reservoirs_article_quality", "matrix_reservoir_summary.csv")
prevalence_path <- file.path(base_dir, "artifacts", "matrix_reservoirs_article_quality", "matrix_antibiotic_prevalence.csv")
output_path <- file.path(base_dir, "artifacts", "paper_figures", "matrix_reservoir_table_for_excel.csv")

matrix_order <- c("municipal sludge", "sediment", "soil", "surface water")
matrix_labels <- c(
  "municipal sludge" = "Municipal sludge",
  "sediment" = "Sediment",
  "soil" = "Soil",
  "surface water" = "Surface water"
)
matrix_patterns <- c(
  "municipal sludge" = "Hotspot reservoir with the highest typical and extreme concentrations; dominated by high-burden fluoroquinolones and macrolides.",
  "sediment" = "Broad diffuse reservoir with the widest chemical and geographic spread across provinces.",
  "soil" = "Intermediate reservoir with lower intensity than sludge and strong tetracycline enrichment.",
  "surface water" = "Exposure-relevant but narrower compartment with weaker support as a long-term reservoir."
)

summary_df <- read.csv(summary_path, stringsAsFactors = FALSE, check.names = FALSE)
prevalence_df <- read.csv(prevalence_path, stringsAsFactors = FALSE, check.names = FALSE)

summary_df$matrix <- factor(summary_df$matrix, levels = matrix_order, ordered = TRUE)
summary_df <- summary_df[order(summary_df$matrix), , drop = FALSE]

prevalence_main <- prevalence_df[prevalence_df$records >= 2, , drop = FALSE]

format_prevalent <- function(matrix_name) {
  sub <- prevalence_main[prevalence_main$sample_type == matrix_name, , drop = FALSE]
  sub <- sub[order(-sub$prevalence_score, -sub$records, -sub$provinces), , drop = FALSE]
  sub <- head(sub, 5)
  if (nrow(sub) == 0) {
    return(NA_character_)
  }
  paste(
    sprintf(
      "%s (%s; n=%s; median=%.2f)",
      sub$antibiotic,
      sub$group_of_antibiotic,
      sub$records,
      sub$median_conc
    ),
    collapse = "; "
  )
}

format_hotspots <- function(matrix_name) {
  sub <- prevalence_df[prevalence_df$sample_type == matrix_name, , drop = FALSE]
  sub <- sub[order(-sub$max_conc, -sub$records), , drop = FALSE]
  sub <- head(sub, 3)
  if (nrow(sub) == 0) {
    return(NA_character_)
  }
  paste(
    sprintf(
      "%s (%s; max=%.2f)",
      sub$antibiotic,
      sub$group_of_antibiotic,
      sub$max_conc
    ),
    collapse = "; "
  )
}

table_df <- data.frame(
  Matrix = unname(matrix_labels[as.character(summary_df$matrix)]),
  Reservoir_role = summary_df$reservoir_role,
  Records = summary_df$records,
  Provinces = summary_df$provinces,
  Antibiotics = summary_df$antibiotics,
  Classes = summary_df$classes,
  Median_concentration = round(summary_df$median_conc, 2),
  P90_concentration = round(summary_df$p90_conc, 2),
  Maximum_concentration = round(summary_df$max_conc, 2),
  Reservoir_interpretation = unname(matrix_patterns[as.character(summary_df$matrix)]),
  Top_prevalent_antibiotics = vapply(as.character(summary_df$matrix), format_prevalent, character(1)),
  Top_concentration_hotspots = vapply(as.character(summary_df$matrix), format_hotspots, character(1)),
  stringsAsFactors = FALSE,
  check.names = FALSE
)

dir.create(dirname(output_path), recursive = TRUE, showWarnings = FALSE)
write.csv(table_df, output_path, row.names = FALSE, na = "")

cat("Saved matrix reservoir table to:", output_path, "\n")