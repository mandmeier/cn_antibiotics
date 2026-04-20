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

ensure_packages(c("ggplot2"))

SOURCE_COLUMNS <- list(
  agricultural = c("Animal_husbandry_100_million_yuan", "Fishery_100_million_yuan", "Freshwater_Aquatic_Products_10000_tons", "Slaughtered_Hogs_10000_heads", "Output_of_Meat_10000_tons", "Agriculture_Use_100_million_cu_m"),
  industrial = c("Industry_Use_100_million_cu_m", "COD_ton", "Ammonia_Nitrogen_ton", "Total_Nitrogen_ton", "Total_Phosphorus_ton", "Treatment_of_Wastewater_10000_yuan"),
  hospital_municipal = c("Total_Health_Care_Institutions", "General_Hospitals", "Households_Service_Use_100_million_cu_m", "Centers_for_Disease_Control", "Water_Use_100_million_cu_m")
)

SOURCE_KEYWORDS <- list(
  agricultural = c("swine", "livestock", "veterinary", "manure", "mariculture", "aquaculture", "fishery", "hog", "agricultural", "farm"),
  industrial = c("production wastewater", "pharmaceutical", "tannery", "industrial", "manufacturing", "factory", "drinking water treatment plant"),
  hospital_municipal = c("municipal", "sewage treatment plant", "wastewater treatment plant", "wastewater", "urban aquatic", "hospital", "sewage")
)

ANTIBIOTIC_SOURCE_HINTS <- list(
  agricultural = c("Chlortetracycline", "Oxytetracycline", "Tetracycline", "Doxycycline", "Enrofloxacin", "Florfenicol", "Lincomycin", "Sulfadiazine", "Sulfamethazine", "Sulfamerazine", "Tylosin"),
  hospital_municipal = c("Ciprofloxacin", "Ofloxacin", "Levofloxacin", "Norfloxacin", "Azithromycin", "Clarithromycin", "Erythromycin", "Trimethoprim", "Amoxicillin", "Ampicillin", "Cefazolin", "Cefotaxime", "Ceftriaxone"),
  industrial = c("Oxytetracycline", "Chloramphenicol", "Ciprofloxacin", "Ofloxacin")
)

args <- parse_cli_args(list(
  dataset_dir = "Dataset",
  environment_csv = "artifacts/article_quality_table/article_quality_table.csv",
  output_dir = "artifacts/k3_source_attribution_r",
  top_antibiotics_per_province = 5
))

load_environment_table <- function(path) {
  frame <- read_csv_na(path)
  if ("full_reference_name" %in% names(frame) && !"pub_full" %in% names(frame)) names(frame)[names(frame) == "full_reference_name"] <- "pub_full"
  frame
}

build_yearbook_source_scores <- function(yearbook) {
  keep <- unique(c("province", unlist(SOURCE_COLUMNS, use.names = FALSE)))
  keep <- keep[keep %in% names(yearbook)]
  frame <- yearbook[, keep, drop = FALSE]
  province <- frame$province
  frame$province <- NULL
  for (column in names(frame)) frame[[column]] <- suppressWarnings(as.numeric(frame[[column]]))
  zscores <- zscore_df(frame)
  scores <- data.frame(province = province, stringsAsFactors = FALSE)
  for (source_name in names(SOURCE_COLUMNS)) {
    cols <- SOURCE_COLUMNS[[source_name]][SOURCE_COLUMNS[[source_name]] %in% names(zscores)]
    scores[[paste0(source_name, "_proxy_score")]] <- if (length(cols) > 0) rowMeans(zscores[, cols, drop = FALSE], na.rm = TRUE) else NA_real_
  }
  scores
}

classify_text_sources <- function(text) {
  lowered <- tolower(as.character(text))
  vapply(SOURCE_KEYWORDS, function(keywords) as.integer(any(vapply(keywords, function(keyword) grepl(keyword, lowered, fixed = TRUE), logical(1)))), integer(1))
}

build_text_source_scores <- function(env) {
  frame <- env[, c("province", "mean_concentration", "pub_full")]
  frame$mean_concentration <- suppressWarnings(as.numeric(frame$mean_concentration))
  frame$weight <- log1p(pmax(frame$mean_concentration, 0))
  for (source_name in names(SOURCE_KEYWORDS)) {
    frame[[paste0(source_name, "_text_flag")]] <- vapply(frame$pub_full, function(text) classify_text_sources(text)[[source_name]], integer(1))
    frame[[paste0(source_name, "_text_weight")]] <- frame[[paste0(source_name, "_text_flag")]] * frame$weight
  }
  aggregate(frame[, grep("_text_weight$", names(frame), value = TRUE), drop = FALSE], by = list(province = frame$province), FUN = sum, na.rm = TRUE)
}

build_contamination_source_table <- function(env) {
  frame <- env[, c("province", "antibiotic", "mean_concentration", "pub_full")]
  frame$mean_concentration <- suppressWarnings(as.numeric(frame$mean_concentration))
  frame$log_weight <- log1p(pmax(frame$mean_concentration, 0))
  for (source_name in names(SOURCE_KEYWORDS)) frame[[paste0(source_name, "_score")]] <- 0
  for (source_name in names(ANTIBIOTIC_SOURCE_HINTS)) {
    index <- frame$antibiotic %in% ANTIBIOTIC_SOURCE_HINTS[[source_name]]
    frame[[paste0(source_name, "_score")]][index] <- frame[[paste0(source_name, "_score")]][index] + 0.35 * frame$log_weight[index]
  }
  for (source_name in names(SOURCE_KEYWORDS)) {
    flags <- vapply(frame$pub_full, function(text) any(vapply(SOURCE_KEYWORDS[[source_name]], function(keyword) grepl(keyword, tolower(as.character(text)), fixed = TRUE), logical(1))), logical(1))
    frame[[paste0(source_name, "_score")]] <- frame[[paste0(source_name, "_score")]] + 0.65 * as.integer(flags) * frame$log_weight
  }
  score_cols <- grep("_score$", names(frame), value = TRUE)
  top_names <- apply(frame[, score_cols, drop = FALSE], 1, function(row) score_cols[which.max(row)])
  frame$dominant_source <- sub("_score$", "", top_names)
  top_sorted <- t(apply(frame[, score_cols, drop = FALSE], 1, sort, decreasing = TRUE))
  frame$source_margin <- top_sorted[, 1] - ifelse(ncol(top_sorted) >= 2, top_sorted[, 2], 0)
  frame$dominant_source[frame$source_margin < 0.25] <- "mixed"
  frame
}

infer_province_sources <- function(groups, yearbook_scores, text_scores, contamination_sources) {
  contamination_summary <- aggregate(contamination_sources[, c("agricultural_score", "industrial_score", "hospital_municipal_score"), drop = FALSE], by = list(province = contamination_sources$province), FUN = sum, na.rm = TRUE)
  counts <- aggregate(antibiotic ~ province, data = contamination_sources, FUN = length)
  names(counts)[2] <- "contamination_rows"
  names(contamination_summary)[2:4] <- c("agricultural_contamination_score", "industrial_contamination_score", "hospital_municipal_contamination_score")
  group_cols <- intersect(c("province", "k3_groups", "NBS_region", "qu_category"), names(groups))
  frame <- groups[, group_cols, drop = FALSE]
  if ("k3_groups" %in% names(frame)) names(frame)[names(frame) == "k3_groups"] <- "k3_cluster"
  frame <- merge(frame, yearbook_scores, by = "province", all.x = TRUE)
  frame <- merge(frame, text_scores, by = "province", all.x = TRUE)
  frame <- merge(frame, counts, by = "province", all.x = TRUE)
  frame <- merge(frame, contamination_summary, by = "province", all.x = TRUE)
  for (source_name in names(SOURCE_KEYWORDS)) {
    frame[[paste0(source_name, "_combined_score")]] <- rowSums(cbind(
      frame[[paste0(source_name, "_proxy_score")]] %||% 0,
      frame[[paste0(source_name, "_text_weight")]] %||% 0,
      frame[[paste0(source_name, "_contamination_score")]] %||% 0
    ), na.rm = TRUE)
  }
  score_cols <- paste0(names(SOURCE_KEYWORDS), "_combined_score")
  frame$dominant_source <- sub("_combined_score$", "", apply(frame[, score_cols, drop = FALSE], 1, function(row) score_cols[which.max(row)]))
  score_matrix <- as.matrix(frame[, score_cols, drop = FALSE])
  sorted_scores <- t(apply(score_matrix, 1, sort, decreasing = TRUE))
  frame$second_best_score <- if (ncol(sorted_scores) >= 2) sorted_scores[, 2] else 0
  frame$top_score <- sorted_scores[, 1]
  frame$source_margin <- frame$top_score - frame$second_best_score
  frame$dominant_source[frame$source_margin < 0.5] <- "mixed"
  frame[order(frame$k3_cluster, frame$dominant_source, frame$province), , drop = FALSE]
}

build_bacteria_source_links <- function(carss, province_sources) {
  carss$resistant_pc <- suppressWarnings(as.numeric(carss$resistant_pc))
  resistance_index <- aggregate(resistant_pc ~ province + bacteria, data = carss, FUN = function(x) mean(x, na.rm = TRUE))
  merged <- merge(resistance_index, province_sources, by = "province", all.x = TRUE)
  rows <- list()
  idx <- 1L
  for (bacteria in unique(merged$bacteria)) {
    sub <- merged[merged$bacteria == bacteria, , drop = FALSE]
    for (source_name in names(SOURCE_KEYWORDS)) {
      score <- suppressWarnings(as.numeric(sub[[paste0(source_name, "_combined_score")]]))
      resistance <- suppressWarnings(as.numeric(sub$resistant_pc))
      valid <- stats::complete.cases(score, resistance)
      if (sum(valid) < 8) next
      test <- suppressWarnings(stats::cor.test(score[valid], resistance[valid], method = "spearman", exact = FALSE))
      rows[[idx]] <- data.frame(bacteria = bacteria, candidate_source = source_name, spearman_rho = unname(test$estimate), p_value = test$p.value, n_provinces = sum(valid), stringsAsFactors = FALSE)
      idx <- idx + 1L
    }
  }
  table <- do.call(rbind, rows)
  if (is.null(table) || nrow(table) == 0) return(data.frame())
  table$positive_rho <- pmax(table$spearman_rho, 0)
  dominant <- do.call(rbind, lapply(split(table[order(table$bacteria, -table$positive_rho, table$p_value), , drop = FALSE], table$bacteria), function(frame) frame[1, , drop = FALSE]))
  names(dominant)[names(dominant) == "candidate_source"] <- "most_likely_source"
  dominant$source_confidence <- ifelse(dominant$positive_rho >= 0.3 & dominant$p_value <= 0.1, "supported", "uncertain")
  dominant$most_likely_source[dominant$source_confidence == "uncertain"] <- "uncertain"
  dominant[order(-dominant$positive_rho, dominant$p_value), , drop = FALSE]
}

build_province_top_antibiotics <- function(contamination_sources, top_n) {
  frame <- contamination_sources[order(contamination_sources$province, -contamination_sources$mean_concentration), , drop = FALSE]
  frame$rank_within_province <- ave(frame$province, frame$province, FUN = seq_along)
  frame <- frame[frame$rank_within_province <= top_n, c("province", "rank_within_province", "antibiotic", "mean_concentration", "dominant_source", "agricultural_score", "industrial_score", "hospital_municipal_score", "pub_full"), drop = FALSE]
  frame
}

build_cluster_source_summary <- function(province_sources) {
  counts <- aggregate(province ~ k3_cluster + dominant_source, data = province_sources, FUN = length)
  names(counts)[3] <- "province_count"
  totals <- aggregate(province_count ~ k3_cluster, data = counts, FUN = sum)
  counts <- merge(counts, totals, by = "k3_cluster", suffixes = c("", "_total"))
  counts$share_within_cluster <- counts$province_count / counts$province_count_total
  counts[order(counts$k3_cluster, -counts$province_count), c("k3_cluster", "dominant_source", "province_count", "share_within_cluster"), drop = FALSE]
}

write_summary <- function(output_dir, province_sources, bacteria_links, cluster_summary, province_top_antibiotics) {
  lines <- c("K3 source attribution summary", "", "Method:", "- Source attribution combines yearbook proxy scores, text cues from publication metadata, and antibiotic-specific source hints.", "- Candidate sources are agricultural, industrial, hospital_municipal, and mixed when no single source dominates.", "", "Cluster-level source pattern:")
  for (cluster in sort(unique(cluster_summary$k3_cluster))) {
    subset <- cluster_summary[cluster_summary$k3_cluster == cluster, , drop = FALSE]
    formatted <- paste(sprintf("%s (%s, %.0f%%)", subset$dominant_source, subset$province_count, 100 * subset$share_within_cluster), collapse = ", ")
    lines <- c(lines, sprintf("- Cluster %s: %s", cluster, formatted))
  }
  lines <- c(lines, "", "Bacteria most linked to each source profile:")
  for (index in seq_len(nrow(bacteria_links))) {
    row <- bacteria_links[index, , drop = FALSE]
    lines <- c(lines, sprintf("- %s: %s (Spearman rho=%.3f, p=%.4g, %s)", row$bacteria, row$most_likely_source, row$spearman_rho, row$p_value, row$source_confidence))
  }
  lines <- c(lines, "", "Examples of province-specific contamination origins:")
  for (province in head(unique(province_top_antibiotics$province), 8)) {
    subset <- province_top_antibiotics[province_top_antibiotics$province == province, , drop = FALSE]
    formatted <- paste(sprintf("%s -> %s (%.1f)", subset$antibiotic, subset$dominant_source, subset$mean_concentration), collapse = ", ")
    lines <- c(lines, sprintf("- %s: %s", province, formatted))
  }
  writeLines(lines, file.path(output_dir, "summary.txt"))
}

plot_cluster_source_composition <- function(cluster_summary, output_dir) {
  plot <- ggplot2::ggplot(cluster_summary, ggplot2::aes(x = factor(k3_cluster), y = share_within_cluster, fill = dominant_source)) + ggplot2::geom_col() + ggplot2::scale_fill_manual(values = c(agricultural = "#6A994E", industrial = "#BC4749", hospital_municipal = "#3A86FF", mixed = "#7A7A7A")) + ggplot2::labs(title = "Source composition within K3 clusters", x = NULL, y = "Share of provinces") + ggplot2::theme_minimal()
  ggplot2::ggsave(file.path(output_dir, "cluster_source_composition.png"), plot, width = 9, height = 5.5, dpi = 300)
}

plot_province_source_profile_map <- function(province_sources, output_dir) {
  score_cols <- c("agricultural_combined_score", "industrial_combined_score", "hospital_municipal_combined_score")
  available <- score_cols[score_cols %in% names(province_sources)]
  if (length(available) == 0) return()
  frame <- province_sources[order(province_sources$k3_cluster, province_sources$province), c("province", "k3_cluster", available), drop = FALSE]
  long <- reshape(frame, varying = available, v.names = "score", timevar = "source", times = available, direction = "long")
  plot <- ggplot2::ggplot(long, ggplot2::aes(x = source, y = stats::reorder(sprintf("%s (K%s)", province, k3_cluster), -k3_cluster), fill = score)) + ggplot2::geom_tile() + ggplot2::scale_fill_gradient(low = "#FFF5EB", high = "#D94801") + ggplot2::labs(title = "Province source profile map", x = NULL, y = NULL) + ggplot2::theme_minimal()
  ggplot2::ggsave(file.path(output_dir, "province_source_profile_map.png"), plot, width = 8.5, height = max(7, 0.33 * nrow(frame) + 1.5), dpi = 300)
}

plot_bacteria_source_links <- function(bacteria_links, output_dir) {
  if (nrow(bacteria_links) == 0) return()
  bacteria_links$label <- sprintf("%s (%s)", bacteria_links$bacteria, bacteria_links$most_likely_source)
  plot <- ggplot2::ggplot(bacteria_links, ggplot2::aes(x = positive_rho, y = stats::reorder(label, positive_rho), fill = most_likely_source)) + ggplot2::geom_col() + ggplot2::geom_text(ggplot2::aes(label = source_confidence), hjust = -0.1, size = 3) + ggplot2::coord_cartesian(xlim = c(0, max(bacteria_links$positive_rho, na.rm = TRUE) + 0.1)) + ggplot2::theme_minimal() + ggplot2::labs(title = "Bacteria-source associations", x = "Positive Spearman rho with source score", y = NULL)
  ggplot2::ggsave(file.path(output_dir, "bacteria_source_links.png"), plot, width = 9, height = 5.5, dpi = 300)
}

output_dir <- ensure_dir(args$output_dir)
groups <- read_csv_na(file.path(args$dataset_dir, "province_groups.csv"))
env <- load_environment_table(args$environment_csv)
yearbook <- read_csv_na(file.path(args$dataset_dir, "yearbook_cleaned.csv"))
carss <- read_csv_na(file.path(args$dataset_dir, "carss_cleaned.csv"))

yearbook_scores <- build_yearbook_source_scores(yearbook)
text_scores <- build_text_source_scores(env)
contamination_sources <- build_contamination_source_table(env)
province_sources <- infer_province_sources(groups, yearbook_scores, text_scores, contamination_sources)
bacteria_links <- build_bacteria_source_links(carss, province_sources)
province_top_antibiotics <- build_province_top_antibiotics(contamination_sources, args$top_antibiotics_per_province)
cluster_summary <- build_cluster_source_summary(province_sources)

write_csv_safe(province_sources, file.path(output_dir, "province_source_attribution.csv"))
write_csv_safe(bacteria_links, file.path(output_dir, "bacteria_source_links.csv"))
write_csv_safe(province_top_antibiotics, file.path(output_dir, "province_top_antibiotic_sources.csv"))
write_csv_safe(cluster_summary, file.path(output_dir, "cluster_source_summary.csv"))
write_csv_safe(yearbook_scores, file.path(output_dir, "yearbook_source_scores.csv"))
write_csv_safe(text_scores, file.path(output_dir, "text_source_scores.csv"))
write_summary(output_dir, province_sources, bacteria_links, cluster_summary, province_top_antibiotics)
plot_cluster_source_composition(cluster_summary, output_dir)
plot_province_source_profile_map(province_sources, output_dir)
plot_bacteria_source_links(bacteria_links, output_dir)

cat(sprintf("Saved source attribution outputs to: %s\n", output_dir))