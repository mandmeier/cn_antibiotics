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

ensure_packages(c("ggplot2", "nnet", "randomForest"))

YEARBOOK_FEATURES <- c(
  "Per_Capita_Water_Resources_cu_m_person",
  "Water_Use_100_million_cu_m",
  "Agriculture_Use_100_million_cu_m",
  "Industry_Use_100_million_cu_m",
  "Households_Service_Use_100_million_cu_m",
  "Total_Health_Care_Institutions",
  "Centers_for_Disease_Control",
  "COD_ton",
  "Ammonia_Nitrogen_ton",
  "Total_Nitrogen_ton",
  "Total_Phosphorus_ton",
  "GRP_100_million_yuan",
  "Freshwater_Aquatic_Products_10000_tons",
  "Output_of_Meat_10000_tons",
  "Slaughtered_Hogs_10000_heads",
  "Animal_husbandry_100_million_yuan"
)

SAMPLE_TYPES <- c("municipal sludge", "sediments", "soil")

args <- parse_cli_args(list(
  dataset_dir = "Dataset",
  output_dir = "artifacts/k3_relationships_r",
  environment_csv = "artifacts/article_quality_table/article_quality_table.csv",
  min_coverage = 10,
  model_feature_limit = 20
))

load_environment_table <- function(path) {
  frame <- read_csv_na(path)
  frame$sample_type[frame$sample_type == "sediment"] <- "sediments"
  frame
}

build_resistance_features <- function(carss) {
  carss$resistant_pc <- suppressWarnings(as.numeric(carss$resistant_pc))
  carss$feature_name <- paste(carss$bacteria, carss$antibiotic, "resistance", sep = "__")
  stats::reshape(
    aggregate(resistant_pc ~ province + feature_name, data = carss, FUN = function(x) mean(x, na.rm = TRUE)),
    idvar = "province",
    timevar = "feature_name",
    direction = "wide"
  )
}

build_concentration_features <- function(env, allowed_antibiotics) {
  frame <- env[env$sample_type %in% SAMPLE_TYPES & env$antibiotic %in% allowed_antibiotics, , drop = FALSE]
  frame$mean_concentration <- suppressWarnings(as.numeric(frame$mean_concentration))
  frame$log_concentration <- log1p(frame$mean_concentration)
  frame$feature_name <- paste(frame$sample_type, frame$antibiotic, "log_conc", sep = "__")
  stats::reshape(
    aggregate(log_concentration ~ province + feature_name, data = frame, FUN = function(x) stats::median(x, na.rm = TRUE)),
    idvar = "province",
    timevar = "feature_name",
    direction = "wide"
  )
}

build_yearbook_features <- function(yearbook) {
  keep <- c("province", YEARBOOK_FEATURES[YEARBOOK_FEATURES %in% names(yearbook)])
  frame <- yearbook[, keep, drop = FALSE]
  for (column in setdiff(names(frame), "province")) {
    frame[[column]] <- suppressWarnings(as.numeric(frame[[column]]))
  }
  frame
}

merge_wide <- function(groups, resistance_wide, concentration_wide, yearbook_wide) {
  frame <- merge(groups, resistance_wide, by = "province", all.x = TRUE)
  frame <- merge(frame, concentration_wide, by = "province", all.x = TRUE)
  merge(frame, yearbook_wide, by = "province", all.x = TRUE)
}

strip_prefixes <- function(names_vec) sub("^[^.]+\\.", "", names_vec)

build_feature_table <- function(dataset_dir, environment_path, min_coverage) {
  groups <- read_csv_na(file.path(dataset_dir, "province_groups.csv"))[, c("province", "k3_groups")]
  names(groups)[2] <- "k3_cluster"
  carss <- read_csv_na(file.path(dataset_dir, "carss_cleaned.csv"))
  env <- load_environment_table(environment_path)
  yearbook <- read_csv_na(file.path(dataset_dir, "yearbook_cleaned.csv"))

  common_antibiotics <- intersect(unique(carss$antibiotic), unique(env$antibiotic))
  resistance_wide <- build_resistance_features(carss)
  concentration_wide <- build_concentration_features(env, common_antibiotics)
  yearbook_wide <- build_yearbook_features(yearbook)

  names(resistance_wide) <- c("province", strip_prefixes(names(resistance_wide)[-1]))
  names(concentration_wide) <- c("province", strip_prefixes(names(concentration_wide)[-1]))

  merged <- merge_wide(groups, resistance_wide, concentration_wide, yearbook_wide)
  merged <- merged[order(merged$province), , drop = FALSE]
  feature_columns <- setdiff(names(merged), c("province", "k3_cluster"))
  coverage <- data.frame(
    feature = feature_columns,
    non_null_provinces = vapply(feature_columns, function(column) sum(!is.na(merged[[column]])), numeric(1)),
    stringsAsFactors = FALSE
  )
  coverage$coverage_share <- coverage$non_null_provinces / nrow(merged)
  keep_features <- coverage$feature[coverage$non_null_provinces >= min_coverage]
  list(
    feature_table = merged[, c("province", "k3_cluster", keep_features), drop = FALSE],
    coverage = coverage[order(-coverage$non_null_provinces, coverage$feature), , drop = FALSE]
  )
}

kruskal_feature_ranking <- function(table) {
  feature_columns <- setdiff(names(table), c("province", "k3_cluster"))
  rows <- lapply(feature_columns, function(column) {
    values <- suppressWarnings(as.numeric(table[[column]]))
    current <- data.frame(k3_cluster = table$k3_cluster, value = values)
    current <- current[stats::complete.cases(current), , drop = FALSE]
    split_values <- split(current$value, current$k3_cluster)
    split_values <- split_values[vapply(split_values, length, integer(1)) >= 2]
    if (length(split_values) < 2) {
      return(NULL)
    }
    test <- stats::kruskal.test(split_values)
    means <- tapply(current$value, current$k3_cluster, mean)
    row <- data.frame(
      feature = column,
      kruskal_h = unname(test$statistic),
      p_value = test$p.value,
      non_null_provinces = nrow(current),
      stringsAsFactors = FALSE
    )
    for (cluster in sort(unique(table$k3_cluster))) {
      row[[sprintf("mean_cluster_%s", cluster)]] <- means[as.character(cluster)] %||% NA_real_
    }
    row
  })
  ranking <- do.call(rbind, rows)
  if (is.null(ranking)) {
    return(data.frame())
  }
  ranking[order(ranking$p_value, -ranking$kruskal_h), , drop = FALSE]
}

loo_multinom <- function(feature_table, features) {
  X <- feature_table[, features, drop = FALSE]
  y <- factor(feature_table$k3_cluster)
  preds <- character(nrow(X))
  for (index in seq_len(nrow(X))) {
    train_x <- X[-index, , drop = FALSE]
    test_x <- X[index, , drop = FALSE]
    train_x <- as.data.frame(median_impute_matrix(train_x))
    names(train_x) <- features
    test_x <- as.data.frame(median_impute_matrix(rbind(test_x, train_x))[1, , drop = FALSE])
    names(test_x) <- features
    means <- vapply(train_x, mean, numeric(1))
    sds <- vapply(train_x, stats::sd, numeric(1))
    sds[sds == 0 | is.na(sds)] <- 1
    train_x <- as.data.frame(scale(train_x, center = means, scale = sds))
    test_x <- as.data.frame(scale(test_x, center = means, scale = sds))
    class_weights <- as.numeric(max(table(y[-index])) / table(y[-index])[y[-index]])
    fit <- nnet::multinom(y[-index] ~ ., data = cbind(train_x, y = y[-index]), weights = class_weights, trace = FALSE, MaxNWts = 10000)
    preds[index] <- as.character(stats::predict(fit, newdata = test_x, type = "class"))
  }

  full_x <- as.data.frame(median_impute_matrix(X))
  names(full_x) <- features
  full_x <- as.data.frame(scale(full_x))
  full_x[is.na(full_x)] <- 0
  class_weights <- as.numeric(max(table(y)) / table(y)[y])
  fit <- nnet::multinom(y ~ ., data = cbind(full_x, y = y), weights = class_weights, trace = FALSE, MaxNWts = 10000)
  coefs <- summary(fit)$coefficients
  if (is.null(dim(coefs))) {
    coefs <- matrix(coefs, nrow = 1)
    rownames(coefs) <- levels(y)[2]
  }
  coef_frame <- data.frame(feature = colnames(coefs), t(coefs), check.names = FALSE)
  names(coef_frame)[-1] <- paste0("cluster_", rownames(coefs))
  coef_numeric <- as.matrix(coef_frame[, -1, drop = FALSE])
  coef_frame$abs_max_coefficient <- apply(abs(coef_numeric), 1, max)
  coef_frame <- coef_frame[order(-coef_frame$abs_max_coefficient), , drop = FALSE]

  confusion <- table(actual = y, predicted = factor(preds, levels = levels(y)))
  list(
    coefficients = coef_frame,
    metrics = data.frame(
      loo_accuracy = mean(preds == as.character(y)),
      n_provinces = nrow(feature_table),
      n_model_features = length(features)
    ),
    confusion = as.data.frame.matrix(confusion)
  )
}

fit_random_forest <- function(feature_table, features) {
  X <- as.data.frame(median_impute_matrix(feature_table[, features, drop = FALSE]))
  names(X) <- features
  y <- factor(feature_table$k3_cluster)
  fit <- randomForest::randomForest(x = X, y = y, ntree = 1000, maxnodes = 31, importance = TRUE)
  importance <- randomForest::importance(fit, type = 2)
  data.frame(feature = rownames(importance), rf_importance = importance[, 1], row.names = NULL)[order(-importance[, 1]), , drop = FALSE]
}

build_cluster_profiles <- function(feature_table, features) {
  numeric <- feature_table[, features, drop = FALSE]
  numeric <- as.data.frame(lapply(numeric, function(column) suppressWarnings(as.numeric(column))))
  standardized <- zscore_df(numeric)
  standardized$k3_cluster <- feature_table$k3_cluster
  means <- aggregate(. ~ k3_cluster, data = standardized, FUN = function(x) mean(x, na.rm = TRUE))
  out <- data.frame(feature = features, stringsAsFactors = FALSE)
  for (cluster in means$k3_cluster) {
    row <- means[means$k3_cluster == cluster, , drop = FALSE]
    out[[sprintf("cluster_%s_z_mean", cluster)]] <- as.numeric(row[, features, drop = TRUE])
  }
  out
}

build_explanatory_feature_table <- function(ranking, coefficients, rf_importance, top_n = 15) {
  if (nrow(ranking) == 0) {
    return(data.frame())
  }
  selected <- ranking[seq_len(min(top_n, nrow(ranking))), , drop = FALSE]
  mean_cols <- grep("^mean_cluster_", names(selected), value = TRUE)
  selected$dominant_cluster <- apply(selected[, mean_cols, drop = FALSE], 1, function(row) {
    as.integer(sub("mean_cluster_", "", mean_cols[which.max(row)]))
  })
  selected$feature_type <- ifelse(grepl("__resistance", selected$feature, fixed = TRUE), "resistance", ifelse(grepl("__log_conc", selected$feature, fixed = TRUE), "concentration", "yearbook"))
  merged <- merge(selected, coefficients[, c("feature", "abs_max_coefficient")], by = "feature", all.x = TRUE)
  merged <- merge(merged, rf_importance[, c("feature", "rf_importance")], by = "feature", all.x = TRUE)
  merged[order(merged$p_value, -merged$kruskal_h), c("feature", "feature_type", "dominant_cluster", "p_value", "kruskal_h", "abs_max_coefficient", "rf_importance", mean_cols, "non_null_provinces"), drop = FALSE]
}

prettify_feature_name <- function(feature) {
  text <- gsub("__", " | ", feature, fixed = TRUE)
  text <- gsub("_", " ", text, fixed = TRUE)
  gsub("log conc", "log concentration", text, fixed = TRUE)
}

top_cluster_markers <- function(ranking, cluster, top_n = 3) {
  if (nrow(ranking) == 0) return(character())
  mean_cols <- grep("^mean_cluster_", names(ranking), value = TRUE)
  dominant <- apply(ranking[, mean_cols, drop = FALSE], 1, function(row) as.integer(sub("mean_cluster_", "", mean_cols[which.max(row)])))
  head(ranking$feature[dominant == cluster], top_n)
}

plot_cluster_profile_heatmap <- function(profiles, output_dir, top_n = 12) {
  if (nrow(profiles) == 0) return()
  value_cols <- grep("_z_mean$", names(profiles), value = TRUE)
  profiles$spread <- apply(abs(profiles[, value_cols, drop = FALSE]), 1, max, na.rm = TRUE)
  subset <- profiles[order(-profiles$spread), , drop = FALSE][seq_len(min(top_n, nrow(profiles))), , drop = FALSE]
  matrix <- as.matrix(subset[, value_cols, drop = FALSE])
  rownames(matrix) <- vapply(subset$feature, prettify_feature_name, character(1))
  save_png(file.path(output_dir, "cluster_profile_heatmap.png"), width = 9, height = max(5, 0.45 * nrow(matrix) + 1.5), {
    op <- par(mar = c(5, 12, 4, 2))
    on.exit(par(op), add = TRUE)
    image(t(matrix[nrow(matrix):1, , drop = FALSE]), axes = FALSE, col = grDevices::colorRampPalette(c("#2166AC", "#F7F7F7", "#B2182B"))(100), main = "Cluster profiles for the strongest differentiating features")
    axis(1, at = seq(0, 1, length.out = ncol(matrix)), labels = sub("_z_mean", "", value_cols))
    axis(2, at = seq(0, 1, length.out = nrow(matrix)), labels = rev(rownames(matrix)), las = 2)
  })
}

plot_feature_importance_summary <- function(explanatory_table, output_dir, top_n = 10) {
  if (nrow(explanatory_table) == 0) return()
  plot_frame <- explanatory_table[seq_len(min(top_n, nrow(explanatory_table))), , drop = FALSE]
  plot_frame$label <- vapply(plot_frame$feature, prettify_feature_name, character(1))
  ggplot2::ggsave(file.path(output_dir, "feature_importance_summary.png"), {
    left <- ggplot2::ggplot(plot_frame, ggplot2::aes(x = abs_max_coefficient, y = stats::reorder(label, abs_max_coefficient))) + ggplot2::geom_col(fill = "#355C7D") + ggplot2::labs(title = "Logistic model importance", x = "Absolute coefficient", y = NULL) + ggplot2::theme_minimal()
    right <- ggplot2::ggplot(plot_frame, ggplot2::aes(x = rf_importance, y = stats::reorder(label, rf_importance))) + ggplot2::geom_col(fill = "#F67280") + ggplot2::labs(title = "Random forest importance", x = "Feature importance", y = NULL) + ggplot2::theme_minimal()
    gridExtra::grid.arrange(left, right, ncol = 2)
  }, width = 14, height = 6, dpi = 300)
}

plot_cluster_feature_distributions <- function(feature_table, explanatory_table, output_dir, top_n = 6) {
  if (nrow(explanatory_table) == 0) return()
  ensure_packages(c("ggplot2"))
  features <- explanatory_table$feature[seq_len(min(top_n, nrow(explanatory_table)))]
  long <- do.call(rbind, lapply(features, function(feature) {
    data.frame(province = feature_table$province, k3_cluster = feature_table$k3_cluster, feature = prettify_feature_name(feature), value = suppressWarnings(as.numeric(feature_table[[feature]])), stringsAsFactors = FALSE)
  }))
  long <- long[stats::complete.cases(long), , drop = FALSE]
  plot <- ggplot2::ggplot(long, ggplot2::aes(x = factor(k3_cluster), y = value, color = factor(k3_cluster))) +
    ggplot2::geom_jitter(width = 0.12, size = 1.8, alpha = 0.8) +
    ggplot2::stat_summary(fun = mean, geom = "crossbar", width = 0.4, color = "black") +
    ggplot2::facet_wrap(~ feature, scales = "free_y") +
    ggplot2::scale_color_manual(values = c("1" = "#355C7D", "2" = "#C06C84", "3" = "#F67280")) +
    ggplot2::labs(title = "Distribution of top explanatory features across K3 clusters", x = "K3 cluster", y = "Feature value") +
    ggplot2::theme_minimal() + ggplot2::theme(legend.position = "none")
  ggplot2::ggsave(file.path(output_dir, "cluster_feature_distributions.png"), plot, width = 14, height = 8, dpi = 300)
}

write_summary <- function(output_dir, feature_table, ranking, coefficients, rf_importance, metrics) {
  top_conc <- head(coefficients$feature[grepl("__log_conc", coefficients$feature, fixed = TRUE)], 5)
  lines <- c(
    "K3 cluster explanation summary",
    "",
    sprintf("Provinces in analysis: %s", nrow(feature_table)),
    sprintf("Cross-validated leave-one-out accuracy: %.3f", metrics$loo_accuracy[[1]]),
    sprintf("Features passed into the models: %s", metrics$n_model_features[[1]]),
    "",
    "Top univariate signals by Kruskal-Wallis:"
  )
  for (index in seq_len(min(8, nrow(ranking)))) {
    row <- ranking[index, , drop = FALSE]
    lines <- c(lines, sprintf("- %s: p=%.4g, cluster_1=%.3f, cluster_2=%.3f, cluster_3=%.3f", row$feature, row$p_value, row$mean_cluster_1, row$mean_cluster_2, row$mean_cluster_3))
  }
  lines <- c(
    lines,
    "",
    "Cluster-level markers from the univariate ranking:",
    sprintf("- Cluster 1: %s", paste(top_cluster_markers(ranking, 1), collapse = ", ") %||% "none"),
    sprintf("- Cluster 2: %s", paste(top_cluster_markers(ranking, 2), collapse = ", ") %||% "none"),
    sprintf("- Cluster 3: %s", paste(top_cluster_markers(ranking, 3), collapse = ", ") %||% "none"),
    "",
    "Top logistic model features:"
  )
  for (index in seq_len(min(8, nrow(coefficients)))) {
    row <- coefficients[index, , drop = FALSE]
    lines <- c(lines, sprintf("- %s: abs_max_coefficient=%.4f", row$feature, row$abs_max_coefficient))
  }
  lines <- c(lines, "", "Top random forest features:")
  for (index in seq_len(min(8, nrow(rf_importance)))) {
    row <- rf_importance[index, , drop = FALSE]
    lines <- c(lines, sprintf("- %s: rf_importance=%.4f", row$feature, row$rf_importance))
  }
  lines <- c(lines, "", "Concentration features that still matter after combining all data layers:", sprintf("- %s", if (length(top_conc) == 0) "none among the top logistic features" else paste(top_conc, collapse = ", ")), "", "Interpretation guide:", "- Cluster 2 tends to be separated most by higher water-use, nutrient-load, and aquaculture indicators.", "- Cluster 3 tends to be separated most by higher K. pneumoniae and P. aeruginosa resistance signals.", "- Cluster 1 tends to be relatively distinct through S. aureus resistance and lower pollution-pressure covariates.")
  writeLines(lines, file.path(output_dir, "summary.txt"))
}

output_dir <- ensure_dir(args$output_dir)
bundle <- build_feature_table(args$dataset_dir, args$environment_csv, args$min_coverage)
feature_table <- bundle$feature_table
coverage <- bundle$coverage
ranking <- kruskal_feature_ranking(feature_table)
model_features <- head(ranking$feature, min(args$model_feature_limit, nrow(ranking)))
if (length(model_features) == 0) stop("No features survived the coverage and Kruskal filters.", call. = FALSE)
logistic <- loo_multinom(feature_table, model_features)
rf_importance <- fit_random_forest(feature_table, model_features)
profiles <- build_cluster_profiles(feature_table, model_features)
explanatory_table <- build_explanatory_feature_table(ranking, logistic$coefficients, rf_importance)

write_csv_safe(feature_table, file.path(output_dir, "feature_table.csv"))
write_csv_safe(coverage, file.path(output_dir, "feature_coverage.csv"))
write_csv_safe(ranking, file.path(output_dir, "kruskal_ranking.csv"))
write_csv_safe(logistic$coefficients, file.path(output_dir, "logistic_coefficients.csv"))
write_csv_safe(rf_importance, file.path(output_dir, "random_forest_importance.csv"))
write_csv_safe(profiles, file.path(output_dir, "cluster_profiles_zscores.csv"))
write_csv_safe(explanatory_table, file.path(output_dir, "explanatory_features_table.csv"))
write_csv_safe(logistic$metrics, file.path(output_dir, "model_metrics.csv"))
write_csv_safe(cbind(actual = rownames(logistic$confusion), logistic$confusion), file.path(output_dir, "loo_confusion_matrix.csv"))
write_summary(output_dir, feature_table, ranking, logistic$coefficients, rf_importance, logistic$metrics)
plot_cluster_profile_heatmap(profiles, output_dir)
plot_cluster_feature_distributions(feature_table, explanatory_table, output_dir)

cat(sprintf("Saved analysis outputs to: %s\n", output_dir))
cat(sprintf("Provinces analysed: %s\n", nrow(feature_table)))
cat(sprintf("Model features used: %s\n", length(model_features)))
cat(sprintf("Leave-one-out accuracy: %.3f\n", logistic$metrics$loo_accuracy[[1]]))