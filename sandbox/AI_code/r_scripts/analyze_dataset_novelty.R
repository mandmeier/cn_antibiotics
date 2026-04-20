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

ANTIBIOTIC_CLASSES <- list(
  fluoroquinolone = c("Ciprofloxacin", "Danofloxacin", "Difloxacin", "Enoxacin", "Enrofloxacin", "Fleroxacin", "Flumequine", "Gatifloxacin", "Levofloxacin", "Lomefloxacin", "Marbofloxacin", "Moxifloxacin", "Nadifloxacin", "Nalidixic Acid", "Norfloxacin", "Ofloxacin", "Pefloxacin", "Pipemidic Acid", "Sarafloxacin", "Sparfloxacin"),
  macrolide = c("Azithromycin", "Clarithromycin", "Erythromycin", "Lincomycin", "Roxithromycin", "Spiramycin", "Tylosin"),
  sulfonamide = c("Sulfacetamide", "Sulfachinoxaline", "Sulfachloropyridazine", "Sulfadiazine", "Sulfadimethoxine", "Sulfamerazine", "Sulfameter", "Sulfamethazine", "Sulfamethoxazole", "Sulfamonomethoxine", "Sulfanilamide", "Sulfaphenazole", "Sulfapyridine", "Sulfaquinoxaline", "Sulfathiazole", "Sulfisoxazole"),
  tetracycline = c("Chlortetracycline", "Doxycycline", "Methacycline", "Minocycline", "Oxytetracycline", "Tetracycline")
)
YEARBOOK_FEATURES <- c("Agriculture_Use_100_million_cu_m", "Industry_Use_100_million_cu_m", "Households_Service_Use_100_million_cu_m", "Total_Health_Care_Institutions", "GRP_100_million_yuan", "COD_ton", "Ammonia_Nitrogen_ton", "Total_Nitrogen_ton", "Total_Phosphorus_ton", "Freshwater_Aquatic_Products_10000_tons", "Output_of_Meat_10000_tons", "Slaughtered_Hogs_10000_heads", "Animal_husbandry_100_million_yuan")

args <- parse_cli_args(list(dataset_dir = "Dataset", environment_csv = "artifacts/article_quality_table/article_quality_table.csv", output_dir = "artifacts/dataset_novelty_r", coverage_threshold = 0.6, max_groups = 6, top_findings = 12, n_permutations = 500, n_bootstrap = 500, random_state = 42))
set.seed(as.integer(args$random_state))

classify_antibiotic <- function(name) {
  for (class_name in names(ANTIBIOTIC_CLASSES)) if (name %in% ANTIBIOTIC_CLASSES[[class_name]]) return(class_name)
  "other"
}

load_environment_table <- function(path) {
  frame <- read_csv_na(path)
  frame$sample_type[frame$sample_type == "sediment"] <- "sediments"
  frame
}

build_resistance_features <- function(carss, provinces) {
  frame <- carss[carss$province %in% provinces, , drop = FALSE]
  frame$resistant_pc <- suppressWarnings(as.numeric(frame$resistant_pc))
  frame$antibiotic_class <- vapply(frame$antibiotic, classify_antibiotic, character(1))
  by_bacteria <- stats::reshape(aggregate(resistant_pc ~ province + bacteria, data = frame, FUN = function(x) mean(x, na.rm = TRUE)), idvar = "province", timevar = "bacteria", direction = "wide")
  by_class <- stats::reshape(aggregate(resistant_pc ~ province + antibiotic_class, data = frame, FUN = function(x) mean(x, na.rm = TRUE)), idvar = "province", timevar = "antibiotic_class", direction = "wide")
  names(by_bacteria) <- c("province", sub("^resistant_pc\\.", "", names(by_bacteria)[-1]))
  names(by_class) <- c("province", paste0("resclass_", sub("^resistant_pc\\.", "", names(by_class)[-1])))
  overall <- aggregate(resistant_pc ~ province, data = frame, FUN = function(x) mean(x, na.rm = TRUE))
  names(overall)[2] <- "overall_resistance"
  Reduce(function(left, right) merge(left, right, by = "province", all = TRUE), list(by_bacteria, by_class, overall))
}

build_environment_features <- function(env, provinces) {
  frame <- env[env$province %in% provinces, , drop = FALSE]
  frame$mean_concentration <- suppressWarnings(as.numeric(frame$mean_concentration))
  frame$log_concentration <- log1p(frame$mean_concentration)
  frame$antibiotic_class <- vapply(frame$antibiotic, classify_antibiotic, character(1))
  by_sample_class <- aggregate(log_concentration ~ province + sample_type + antibiotic_class, data = frame, FUN = function(x) stats::median(x, na.rm = TRUE))
  by_sample_class$key <- paste(by_sample_class$sample_type, by_sample_class$antibiotic_class, sep = "__")
  by_sample_class <- stats::reshape(by_sample_class[, c("province", "key", "log_concentration")], idvar = "province", timevar = "key", direction = "wide")
  by_sample <- stats::reshape(aggregate(log_concentration ~ province + sample_type, data = frame, FUN = function(x) stats::median(x, na.rm = TRUE)), idvar = "province", timevar = "sample_type", direction = "wide")
  names(by_sample) <- c("province", paste0("sample_", sub("^log_concentration\\.", "", names(by_sample)[-1])))
  by_class <- stats::reshape(aggregate(log_concentration ~ province + antibiotic_class, data = frame, FUN = function(x) stats::median(x, na.rm = TRUE)), idvar = "province", timevar = "antibiotic_class", direction = "wide")
  names(by_class) <- c("province", paste0("envclass_", sub("^log_concentration\\.", "", names(by_class)[-1])))
  burden <- aggregate(log_concentration ~ province, data = frame, FUN = function(x) c(mean = mean(x, na.rm = TRUE), max = max(x, na.rm = TRUE), size = length(x)))
  burden <- data.frame(province = burden$province, env_log_mean = burden$log_concentration[, "mean"], env_log_max = burden$log_concentration[, "max"], env_measurements = burden$log_concentration[, "size"], stringsAsFactors = FALSE)
  names(by_sample_class) <- c("province", sub("^log_concentration\\.", "", names(by_sample_class)[-1]))
  Reduce(function(left, right) merge(left, right, by = "province", all = TRUE), list(by_sample_class, by_sample, by_class, burden))
}

build_yearbook_features <- function(yearbook, provinces) {
  keep <- c("province", YEARBOOK_FEATURES[YEARBOOK_FEATURES %in% names(yearbook)])
  frame <- yearbook[yearbook$province %in% provinces, keep, drop = FALSE]
  for (column in setdiff(names(frame), "province")) frame[[column]] <- suppressWarnings(as.numeric(frame[[column]]))
  frame
}

build_integrated_table <- function(dataset_dir, environment_path, coverage_threshold) {
  carss <- read_csv_na(file.path(dataset_dir, "carss_cleaned.csv"))
  env <- load_environment_table(environment_path)
  yearbook <- read_csv_na(file.path(dataset_dir, "yearbook_cleaned.csv"))
  qu <- read_csv_na(file.path(dataset_dir, "qu_cleaned.csv"))
  provinces <- sort(setdiff(Reduce(intersect, list(unique(carss$province), unique(env$province), unique(yearbook$province), unique(qu$province))), "Hong Kong"))
  integrated <- Reduce(function(left, right) merge(left, right, by = "province", all = TRUE), list(build_yearbook_features(yearbook, provinces), build_resistance_features(carss, provinces), build_environment_features(env, provinces), qu))
  integrated <- integrated[, !duplicated(names(integrated)), drop = FALSE]
  numeric_cols <- names(integrated)[vapply(integrated, is.numeric, logical(1))]
  coverage <- data.frame(feature = numeric_cols, non_null_provinces = vapply(numeric_cols, function(column) sum(!is.na(integrated[[column]])), numeric(1)), stringsAsFactors = FALSE)
  coverage$coverage_share <- coverage$non_null_provinces / nrow(integrated)
  coverage$layer <- vapply(coverage$feature, feature_layer_novelty, character(1), yearbook_features = YEARBOOK_FEATURES)
  keep_meta <- c("province", "qu_category", "qu_subcategory", "NBS_region", "RR_ranking")
  keep_numeric <- coverage$feature[coverage$coverage_share >= coverage_threshold & !coverage$feature %in% keep_meta]
  filtered <- integrated[, c(intersect(keep_meta, names(integrated)), keep_numeric), drop = FALSE]
  list(filtered = filtered, coverage = coverage[order(-coverage$coverage_share, coverage$feature), , drop = FALSE], integrated = integrated)
}

prepare_numeric_matrix <- function(table) {
  numeric <- table[, names(table)[vapply(table, is.numeric, logical(1))], drop = FALSE]
  imputed <- median_impute_matrix(numeric)
  scaled <- scale_matrix_safe(imputed)
  list(numeric = data.frame(province = table$province, imputed, check.names = FALSE), scaled = scaled)
}

silhouette_mean <- function(pcs, labels) mean(cluster::silhouette(labels, stats::dist(pcs))[, "sil_width"])

choose_cluster_count <- function(pcs, max_groups) {
  upper <- min(max_groups, nrow(pcs) - 1)
  rows <- lapply(2:upper, function(k) {
    labels <- stats::cutree(stats::hclust(stats::dist(pcs), method = "ward.D2"), k = k)
    data.frame(n_groups = k, silhouette_score = silhouette_mean(pcs, labels), stringsAsFactors = FALSE)
  })
  selection <- do.call(rbind, rows)
  selection[order(-selection$silhouette_score, selection$n_groups), , drop = FALSE]
}

permutation_silhouette_test <- function(scaled_matrix, n_groups, observed_silhouette, n_permutations) {
  null_scores <- numeric(n_permutations)
  for (index in seq_len(n_permutations)) {
    permuted <- apply(scaled_matrix, 2, sample)
    pcs <- stats::prcomp(permuted)$x[, seq_len(min(3, ncol(permuted), nrow(permuted))), drop = FALSE]
    labels <- stats::cutree(stats::hclust(stats::dist(pcs), method = "ward.D2"), k = n_groups)
    null_scores[index] <- silhouette_mean(pcs, labels)
  }
  data.frame(n_groups = n_groups, observed_silhouette = observed_silhouette, null_mean_silhouette = mean(null_scores), null_std_silhouette = stats::sd(null_scores), permutation_p_value = (sum(null_scores >= observed_silhouette) + 1) / (n_permutations + 1), n_permutations = n_permutations, stringsAsFactors = FALSE)
}

compute_group_markers <- function(table, group_col, n_permutations) {
  numeric_cols <- setdiff(names(table)[vapply(table, is.numeric, logical(1))], c(group_col, "RR_ranking", "PC1", "PC2", "PC3"))
  rows <- list(); idx <- 1L
  for (feature in numeric_cols) {
    current <- table[, c(group_col, feature), drop = FALSE]
    current <- current[stats::complete.cases(current), , drop = FALSE]
    grouped <- split(current[[feature]], current[[group_col]])
    grouped <- grouped[vapply(grouped, length, integer(1)) >= 2]
    if (length(grouped) < 2) next
    test <- stats::kruskal.test(grouped)
    observed <- unname(test$statistic)
    perm_stats <- numeric(n_permutations)
    for (perm in seq_len(n_permutations)) {
      shuffled <- sample(current[[group_col]])
      perm_stats[perm] <- unname(stats::kruskal.test(split(current[[feature]], shuffled))$statistic)
    }
    means <- tapply(current[[feature]], current[[group_col]], mean)
    row <- data.frame(feature = feature, layer = feature_layer_novelty(feature, YEARBOOK_FEATURES), kruskal_h = observed, analytic_p_value = test$p.value, permutation_p_value = (sum(perm_stats >= observed) + 1) / (n_permutations + 1), dominant_group = as.integer(names(which.max(means))), stringsAsFactors = FALSE)
    for (group_id in sort(unique(current[[group_col]]))) row[[sprintf("mean_group_%s", group_id)]] <- means[as.character(group_id)] %||% NA_real_
    rows[[idx]] <- row; idx <- idx + 1L
  }
  out <- do.call(rbind, rows)
  out$fdr_bh_global <- stats::p.adjust(out$permutation_p_value, method = "BH")
  bh_by_group(out, "permutation_p_value", "layer", "fdr_bh_by_layer")[order(out$fdr_bh_global, out$permutation_p_value, -out$kruskal_h), , drop = FALSE]
}

compute_cross_layer_correlations <- function(table, n_permutations) {
  yearbook_features <- YEARBOOK_FEATURES[YEARBOOK_FEATURES %in% names(table)]
  outcome_features <- names(table)[vapply(names(table), function(column) feature_layer_novelty(column, YEARBOOK_FEATURES) %in% c("resistance", "environment"), logical(1))]
  rows <- list(); idx <- 1L
  for (left in yearbook_features) for (right in outcome_features) {
    if (left == right) next
    current <- table[, c(left, right), drop = FALSE]
    current <- current[stats::complete.cases(current), , drop = FALSE]
    if (nrow(current) < 18) next
    test <- suppressWarnings(stats::cor.test(current[[left]], current[[right]], method = "spearman", exact = FALSE))
    observed <- unname(test$estimate)
    perm_rho <- numeric(n_permutations)
    for (perm in seq_len(n_permutations)) perm_rho[perm] <- suppressWarnings(stats::cor(current[[left]], sample(current[[right]]), method = "spearman"))
    rows[[idx]] <- data.frame(yearbook_feature = left, response_feature = right, response_layer = feature_layer_novelty(right, YEARBOOK_FEATURES), n_provinces = nrow(current), spearman_rho = observed, analytic_p_value = test$p.value, permutation_p_value = (sum(abs(perm_rho) >= abs(observed)) + 1) / (n_permutations + 1), abs_rho = abs(observed), stringsAsFactors = FALSE)
    idx <- idx + 1L
  }
  out <- do.call(rbind, rows)
  out$fdr_bh_global <- stats::p.adjust(out$permutation_p_value, method = "BH")
  bh_by_group(out, "permutation_p_value", "response_layer", "fdr_bh_by_response_layer")[order(out$fdr_bh_by_response_layer, -out$abs_rho), , drop = FALSE]
}

bootstrap_group_stability <- function(numeric_table, pca_scores, n_bootstrap) {
  matrix <- as.matrix(numeric_table[, -1, drop = FALSE])
  provinces <- numeric_table$province
  original_groups <- pca_scores$novelty_group[match(provinces, pca_scores$province)]
  n_groups <- max(original_groups)
  n_features <- ncol(matrix)
  co_assign <- matrix(0, nrow = nrow(matrix), ncol = nrow(matrix))
  for (b in seq_len(n_bootstrap)) {
    sampled <- sample(seq_len(n_features), size = n_features, replace = TRUE)
    boot_values <- matrix[, sampled, drop = FALSE]
    pcs <- stats::prcomp(scale_matrix_safe(boot_values))$x[, seq_len(min(3, ncol(boot_values), nrow(boot_values))), drop = FALSE]
    labels <- stats::cutree(stats::hclust(stats::dist(pcs), method = "ward.D2"), k = n_groups)
    co_assign <- co_assign + outer(labels, labels, `==`)
  }
  co_assign <- co_assign / n_bootstrap
  province_rows <- do.call(rbind, lapply(seq_along(provinces), function(i) {
    same_group <- which(original_groups == original_groups[i] & seq_along(provinces) != i)
    data.frame(province = provinces[i], novelty_group = original_groups[i], bootstrap_membership_stability = if (length(same_group) == 0) NA_real_ else mean(co_assign[i, same_group]), stringsAsFactors = FALSE)
  }))
  group_rows <- do.call(rbind, lapply(sort(unique(original_groups)), function(group_id) {
    idx <- which(original_groups == group_id)
    block <- co_assign[idx, idx, drop = FALSE]
    data.frame(novelty_group = group_id, n_provinces = length(idx), bootstrap_group_stability = if (length(idx) < 2) NA_real_ else mean(block[row(block) != col(block)]), stringsAsFactors = FALSE)
  }))
  list(province = merge(province_rows, group_rows, by = "novelty_group", all.x = TRUE), matrix = data.frame(province = provinces, co_assign, check.names = FALSE))
}

build_group_profiles <- function(table, group_col, top_features) {
  zscores <- zscore_df(table[, top_features, drop = FALSE])
  zscores[[group_col]] <- table[[group_col]]
  aggregate(zscores[, top_features, drop = FALSE], by = list(novelty_group = zscores[[group_col]]), FUN = function(x) mean(x, na.rm = TRUE))
}

build_finding_table <- function(group_markers, correlations, group_members, top_findings) {
  findings <- list(); idx <- 1L
  for (i in seq_len(min(top_findings, nrow(correlations)))) {
    row <- correlations[i, , drop = FALSE]
    findings[[idx]] <- data.frame(finding_type = "cross_layer_correlation", headline = sprintf("%s tracks %s", row$yearbook_feature, row$response_feature), detail = sprintf("Spearman rho=%.3f, perm-p=%.4f, FDR-layer=%.4f, n=%s.", row$spearman_rho, row$permutation_p_value, row$fdr_bh_by_response_layer, row$n_provinces), province_groups = "all groups", primary_layer = paste0("yearbook->", row$response_layer), stringsAsFactors = FALSE)
    idx <- idx + 1L
  }
  members_lookup <- split(group_members$province, group_members$novelty_group)
  for (group_id in sort(unique(group_members$novelty_group))) {
    top_rows <- group_markers[group_markers$dominant_group == group_id, , drop = FALSE]
    top_rows <- head(top_rows, 3)
    if (nrow(top_rows) == 0) next
    findings[[idx]] <- data.frame(finding_type = "province_group_signature", headline = sprintf("Group %s has a distinct integrated signature", group_id), detail = sprintf("Top distinguishing features: %s.", paste(top_rows$feature, collapse = ", ")), province_groups = paste(members_lookup[[as.character(group_id)]], collapse = ", "), primary_layer = paste(unique(top_rows$layer), collapse = ", "), stringsAsFactors = FALSE)
    idx <- idx + 1L
  }
  head(do.call(rbind, findings), top_findings)
}

plot_pca_groups <- function(pca_scores, output_dir) {
  plot <- ggplot2::ggplot(pca_scores, ggplot2::aes(x = PC1, y = PC2, color = factor(novelty_group), label = province)) + ggplot2::geom_point(size = 3) + ggplot2::geom_text(size = 2.6, nudge_y = 0.05, check_overlap = TRUE) + ggplot2::theme_minimal() + ggplot2::labs(title = "De Novo Province Groups from Integrated Dataset", color = "Group")
  ggplot2::ggsave(file.path(output_dir, "province_groups_pca.png"), plot, width = 9, height = 7, dpi = 300)
}

plot_top_correlations <- function(correlations, output_dir, top_n = 12) {
  subset <- head(correlations, top_n)
  subset$label <- sprintf("%s -> %s", subset$yearbook_feature, subset$response_feature)
  plot <- ggplot2::ggplot(subset, ggplot2::aes(x = spearman_rho, y = stats::reorder(label, spearman_rho), fill = spearman_rho >= 0)) + ggplot2::geom_col() + ggplot2::theme_minimal() + ggplot2::scale_fill_manual(values = c("TRUE" = "#2A9D8F", "FALSE" = "#E76F51"), guide = "none") + ggplot2::labs(title = "Top Cross-Layer Correlations", x = "Spearman rho", y = NULL)
  ggplot2::ggsave(file.path(output_dir, "top_cross_layer_correlations.png"), plot, width = 10, height = 7, dpi = 300)
}

write_summary <- function(output_dir, integrated_table, cluster_selection, silhouette_test, pca_scores, province_stability, group_markers, correlations) {
  best_n_groups <- cluster_selection$n_groups[[1]]
  silhouette <- cluster_selection$silhouette_score[[1]]
  members <- split(pca_scores$province, pca_scores$novelty_group)
  sil_p <- silhouette_test$permutation_p_value[[1]]
  group_stability <- unique(province_stability[, c("novelty_group", "bootstrap_group_stability")])
  lines <- c("Integrated dataset novelty analysis summary", "", sprintf("Provinces with complete multi-layer coverage: %s", nrow(integrated_table)), sprintf("Selected de novo province groups: %s", best_n_groups), sprintf("Best silhouette score: %.3f", silhouette), sprintf("Silhouette permutation p-value: %.4f", sil_p), "", "Province groups:")
  for (group_id in sort(as.integer(names(members)))) lines <- c(lines, sprintf("- Group %s: %s", group_id, paste(members[[as.character(group_id)]], collapse = ", ")))
  lines <- c(lines, "", "Bootstrap group stability:")
  for (index in seq_len(nrow(group_stability))) {
    row <- group_stability[index, , drop = FALSE]
    lines <- c(lines, sprintf("- Group %s: mean within-group co-clustering=%.3f", row$novelty_group, row$bootstrap_group_stability))
  }
  lines <- c(lines, "", "Top cross-layer correlations:")
  for (index in seq_len(min(8, nrow(correlations)))) {
    row <- correlations[index, , drop = FALSE]
    lines <- c(lines, sprintf("- %s vs %s: rho=%.3f, perm-p=%.4f, FDR-layer=%.4f, n=%s", row$yearbook_feature, row$response_feature, row$spearman_rho, row$permutation_p_value, row$fdr_bh_by_response_layer, row$n_provinces))
  }
  lines <- c(lines, "", "Top province-group markers:")
  for (index in seq_len(min(12, nrow(group_markers)))) {
    row <- group_markers[index, , drop = FALSE]
    lines <- c(lines, sprintf("- %s (%s): dominant_group=%s, perm-p=%.4f, FDR=%.4f", row$feature, row$layer, row$dominant_group, row$permutation_p_value, row$fdr_bh_global))
  }
  lines <- c(lines, "", "Interpretation guide:", "- The groups are derived de novo from integrated resistance, environmental, and yearbook features rather than from the earlier K3 labels.", "- Cross-layer correlations are ranked using permutation p-values and FDR within resistance vs environment families.", "- Bootstrap stability values quantify how often provinces stay with their original group when the feature set is resampled.", "- Group markers highlight which provinces concentrate the most distinct signatures across the full dataset.")
  writeLines(lines, file.path(output_dir, "summary.txt"))
}

output_dir <- ensure_dir(args$output_dir)
bundle <- build_integrated_table(args$dataset_dir, args$environment_csv, args$coverage_threshold)
integrated_table <- bundle$filtered
coverage <- bundle$coverage
full_table <- bundle$integrated
prepared <- prepare_numeric_matrix(integrated_table)
numeric_table <- prepared$numeric
scaled_matrix <- prepared$scaled
pcs_full <- stats::prcomp(scaled_matrix)$x[, seq_len(min(3, ncol(scaled_matrix), nrow(scaled_matrix))), drop = FALSE]
cluster_selection <- choose_cluster_count(pcs_full, args$max_groups)
best_n_groups <- cluster_selection$n_groups[[1]]
labels <- stats::cutree(stats::hclust(stats::dist(pcs_full), method = "ward.D2"), k = best_n_groups)
pca_scores <- data.frame(province = integrated_table$province, PC1 = pcs_full[, 1], PC2 = pcs_full[, 2], PC3 = if (ncol(pcs_full) >= 3) pcs_full[, 3] else NA_real_, NBS_region = integrated_table$NBS_region %||% NA, qu_category = integrated_table$qu_category %||% NA, RR_ranking = integrated_table$RR_ranking %||% NA, novelty_group = labels, stringsAsFactors = FALSE)
loadings <- data.frame(feature = colnames(numeric_table)[-1], pcs_full = stats::prcomp(scaled_matrix)$rotation[, seq_len(min(3, ncol(scaled_matrix), nrow(scaled_matrix))), drop = FALSE], check.names = FALSE)
names(loadings)[2:ncol(loadings)] <- paste0("PC", seq_len(ncol(loadings) - 1))
loadings$layer <- vapply(loadings$feature, feature_layer_novelty, character(1), yearbook_features = YEARBOOK_FEATURES)
silhouette_test <- permutation_silhouette_test(scaled_matrix, best_n_groups, cluster_selection$silhouette_score[[1]], args$n_permutations)
integrated_with_groups <- merge(integrated_table, pca_scores[, c("province", "novelty_group", "PC1", "PC2", "PC3")], by = "province", all.x = TRUE)
group_markers <- compute_group_markers(integrated_with_groups, "novelty_group", args$n_permutations)
correlations <- compute_cross_layer_correlations(integrated_with_groups, args$n_permutations)
stability <- bootstrap_group_stability(numeric_table, pca_scores, args$n_bootstrap)
group_profiles <- build_group_profiles(integrated_with_groups, "novelty_group", unique(head(group_markers$feature, min(12, nrow(group_markers)))))
province_groups <- unique(pca_scores[, c("province", "novelty_group")])
finding_table <- build_finding_table(group_markers, correlations, province_groups, args$top_findings)

write_csv_safe(integrated_table, file.path(output_dir, "integrated_province_table.csv"))
write_csv_safe(coverage, file.path(output_dir, "feature_coverage.csv"))
write_csv_safe(loadings, file.path(output_dir, "pca_loadings.csv"))
write_csv_safe(pca_scores, file.path(output_dir, "province_novelty_groups.csv"))
write_csv_safe(cluster_selection, file.path(output_dir, "cluster_selection.csv"))
write_csv_safe(silhouette_test, file.path(output_dir, "silhouette_permutation_test.csv"))
write_csv_safe(group_markers, file.path(output_dir, "group_feature_markers.csv"))
write_csv_safe(correlations, file.path(output_dir, "cross_layer_correlations.csv"))
write_csv_safe(stability$province, file.path(output_dir, "group_bootstrap_stability.csv"))
write_csv_safe(stability$matrix, file.path(output_dir, "province_bootstrap_cocluster_matrix.csv"))
write_csv_safe(group_profiles, file.path(output_dir, "group_profiles_zscores.csv"))
write_csv_safe(finding_table, file.path(output_dir, "finding_table.csv"))
plot_pca_groups(pca_scores, output_dir)
plot_top_correlations(correlations, output_dir)
write_summary(output_dir, integrated_table, cluster_selection, silhouette_test, pca_scores, stability$province, group_markers, correlations)
cat(sprintf("Saved novelty outputs to: %s\n", output_dir))