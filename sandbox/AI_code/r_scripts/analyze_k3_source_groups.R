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

ensure_packages(c("cluster", "ggplot2"))

args <- parse_cli_args(list(
  dataset_dir = "Dataset",
  source_attribution_csv = "artifacts/k3_source_attribution_article_quality/province_source_attribution.csv",
  output_dir = "artifacts/k3_source_groups_article_quality_r",
  n_groups = 3
))

CLUSTER_FEATURES <- c("agricultural_combined_score", "industrial_combined_score", "hospital_municipal_combined_score", "contamination_rows")

build_bacteria_table <- function(dataset_dir) {
  carss <- read_csv_na(file.path(dataset_dir, "carss_cleaned.csv"))
  carss$resistant_pc <- suppressWarnings(as.numeric(carss$resistant_pc))
  stats::reshape(aggregate(resistant_pc ~ province + bacteria, data = carss, FUN = function(x) mean(x, na.rm = TRUE)), idvar = "province", timevar = "bacteria", direction = "wide")
}

choose_cluster_titles <- function(profiles) {
  titles <- character(nrow(profiles))
  for (index in seq_len(nrow(profiles))) {
    total <- profiles$agricultural_combined_score[index] + profiles$industrial_combined_score[index] + profiles$hospital_municipal_combined_score[index]
    if (total < 120) {
      titles[index] <- "Lower-intensity mixed source"
    } else if (profiles$industrial_combined_score[index] >= 0.45 * max(total, 1) || profiles$hospital_municipal_combined_score[index] >= 250) {
      titles[index] <- "Urban-industrial / hospital pressure"
    } else {
      titles[index] <- "Agriculture-dominant source pressure"
    }
  }
  titles
}

output_dir <- ensure_dir(args$output_dir)
source <- read_csv_na(args$source_attribution_csv)
bacteria <- build_bacteria_table(args$dataset_dir)
names(bacteria) <- c("province", sub("^resistant_pc\\.", "", names(bacteria)[-1]))
excluded <- source[is.na(source$contamination_rows) | source$contamination_rows <= 0, , drop = FALSE]
base <- source[!is.na(source$contamination_rows) & source$contamination_rows > 0, c("province", CLUSTER_FEATURES), drop = FALSE]
for (column in CLUSTER_FEATURES) base[[column]] <- log1p(pmax(suppressWarnings(as.numeric(base[[column]])), 0))
matrix <- median_impute_matrix(base[, CLUSTER_FEATURES, drop = FALSE])
scaled <- scale_matrix_safe(matrix)
pcs <- stats::prcomp(scaled, center = FALSE, scale. = FALSE)$x[, seq_len(min(3, ncol(scaled), nrow(scaled))), drop = FALSE]
cluster_fit <- stats::hclust(stats::dist(pcs), method = "ward.D2")
labels <- stats::cutree(cluster_fit, k = args$n_groups)
sil <- cluster::silhouette(labels, stats::dist(pcs))
silhouette <- mean(sil[, "sil_width"])
assignments <- data.frame(province = base$province, k3_source = labels, stringsAsFactors = FALSE)
province_groups <- merge(source, assignments, by = "province", all.y = TRUE)
province_groups <- merge(province_groups, bacteria, by = "province", all.x = TRUE)
profiles <- aggregate(province_groups[, c("agricultural_combined_score", "industrial_combined_score", "hospital_municipal_combined_score", "contamination_rows", "A. baumannii", "E. coli", "K. pneumoniae", "P. aeruginosa", "S. aureus"), drop = FALSE], by = list(k3_source = province_groups$k3_source), FUN = function(x) mean(suppressWarnings(as.numeric(x)), na.rm = TRUE))
titles <- choose_cluster_titles(profiles)
cluster_mix <- aggregate(province ~ k3_source + dominant_source, data = province_groups, FUN = length)
names(cluster_mix)[3] <- "n"
totals <- aggregate(n ~ k3_source, data = cluster_mix, FUN = sum)
cluster_mix <- merge(cluster_mix, totals, by = "k3_source", suffixes = c("", "_total"))
cluster_mix$share <- cluster_mix$n / cluster_mix$n_total

write_csv_safe(assignments, file.path(output_dir, "province_k3_source_groups.csv"))
write_csv_safe(province_groups, file.path(output_dir, "province_k3_source_profiles.csv"))
write_csv_safe(profiles, file.path(output_dir, "k3_source_cluster_profiles.csv"))
write_csv_safe(excluded, file.path(output_dir, "excluded_provinces_without_direct_source_evidence.csv"))
write_csv_safe(cluster_mix[, c("k3_source", "dominant_source", "share"), drop = FALSE], file.path(output_dir, "k3_source_dominant_source_mix.csv"))

lines <- c("K3-source summary", "", sprintf("Provinces clustered: %s", nrow(province_groups)), sprintf("Excluded provinces without direct source evidence rows: %s", nrow(excluded)), sprintf("Silhouette score: %.3f", silhouette), "")
for (index in seq_len(nrow(profiles))) {
  row <- profiles[index, , drop = FALSE]
  members <- sort(assignments$province[assignments$k3_source == row$k3_source])
  mix_row <- cluster_mix[cluster_mix$k3_source == row$k3_source, , drop = FALSE]
  source_parts <- paste(sprintf("%s (%.0f%%)", mix_row$dominant_source, 100 * mix_row$share), collapse = ", ")
  lines <- c(lines, sprintf("K3-source %s: %s", row$k3_source, titles[index]), sprintf("- Provinces: %s", paste(members, collapse = ", ")), sprintf("- Mean source scores: agricultural=%.1f, industrial=%.1f, hospital_municipal=%.1f", row$agricultural_combined_score, row$industrial_combined_score, row$hospital_municipal_combined_score), sprintf("- Mean resistance: A. baumannii=%.1f, K. pneumoniae=%.1f, P. aeruginosa=%.1f, S. aureus=%.1f", row$`A. baumannii`, row$`K. pneumoniae`, row$`P. aeruginosa`, row$`S. aureus`), sprintf("- Dominant-source composition: %s", source_parts), "")
  }
if (nrow(excluded) > 0) lines <- c(lines, "Excluded provinces without direct source evidence rows:", paste("-", paste(sort(excluded$province), collapse = ", ")))
writeLines(lines, file.path(output_dir, "summary.txt"))
cat(sprintf("Saved K3-source outputs to: %s\n", output_dir))