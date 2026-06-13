# Random forest and elastic-net cluster identity feature models.

build_cluster_feature_models <- function(yb, groups, yearbook_screen,
                                         min_province_coverage = 25L,
                                         n_bootstrap = 40L,
                                         seed = 42L) {
  y24 <- yb %>%
    dplyr::filter(.data$year == 2024, is.finite(.data$value)) %>%
    dplyr::inner_join(
      groups %>% dplyr::select("province", "cluster_id"),
      by = "province"
    )

  metric_cov <- y24 %>%
    dplyr::group_by(.data$metric) %>%
    dplyr::summarise(
      n_provinces = dplyr::n_distinct(.data$province),
      .groups = "drop"
    ) %>%
    dplyr::filter(.data$n_provinces >= min_province_coverage)

  if (nrow(yearbook_screen)) {
    screened <- yearbook_screen %>%
      dplyr::filter(is.finite(.data$p_value)) %>%
      dplyr::arrange(.data$p_value, .data$metric)
    metric_order <- unique(c(screened$metric, metric_cov$metric))
  } else {
    metric_order <- metric_cov$metric
  }

  metrics_use <- intersect(metric_order, metric_cov$metric)
  if (!length(metrics_use)) {
  return(list(
      performance = data.frame(),
      rf_importance = data.frame(),
      elastic_net = data.frame(),
      consensus = data.frame()
    ))
  }

  wide <- y24 %>%
    dplyr::filter(.data$metric %in% metrics_use) %>%
    dplyr::group_by(.data$province, .data$metric) %>%
    dplyr::summarise(value = stats::median(.data$value, na.rm = TRUE), .groups = "drop") %>%
    tidyr::pivot_wider(names_from = "metric", values_from = "value") %>%
    dplyr::inner_join(
      groups %>% dplyr::select("province", "cluster_id"),
      by = "province"
    )

  feature_cols <- setdiff(names(wide), c("province", "cluster_id"))
  x_raw <- as.data.frame(wide[, feature_cols, drop = FALSE])
  for (col in feature_cols) {
    med <- stats::median(x_raw[[col]], na.rm = TRUE)
    if (!is.finite(med)) med <- 0
    x_raw[[col]][!is.finite(x_raw[[col]])] <- med
  }
  x_mat <- scale(as.matrix(x_raw))
  y_fac <- factor(wide$cluster_id, levels = sort(unique(wide$cluster_id)))

  set.seed(seed)
  rf_fit <- ranger::ranger(
    x = x_mat,
    y = y_fac,
    num.trees = 500,
    importance = "permutation",
    probability = TRUE,
    oob.error = TRUE,
    seed = seed
  )

  oob_acc <- 1 - rf_fit$prediction.error
  loo_correct <- 0L
  for (i in seq_len(nrow(x_mat))) {
    train_idx <- setdiff(seq_len(nrow(x_mat)), i)
    rf_loo <- ranger::ranger(
      x = x_mat[train_idx, , drop = FALSE],
      y = y_fac[train_idx],
      num.trees = 300,
      importance = "none",
      probability = TRUE,
      seed = seed + i
    )
    pred <- predict(rf_loo, data = x_mat[i, , drop = FALSE])$predictions
    pred_class <- as.integer(colnames(pred)[which.max(pred)])
    if (pred_class == as.integer(y_fac[i])) loo_correct <- loo_correct + 1L
  }
  loo_acc <- loo_correct / nrow(x_mat)

  rf_importance <- data.frame(
    metric = colnames(x_mat),
    rf_permutation_importance = as.numeric(rf_fit$variable.importance),
    stringsAsFactors = FALSE
  ) %>%
    dplyr::arrange(dplyr::desc(.data$rf_permutation_importance)) %>%
    dplyr::mutate(rf_importance_rank = dplyr::row_number())

  set.seed(seed)
  cv_fit <- glmnet::cv.glmnet(
    x = x_mat,
    y = y_fac,
    family = "multinomial",
    alpha = 0.5,
    nfolds = min(5L, nrow(x_mat)),
    type.measure = "class"
  )

  enet_coefs <- coef(cv_fit, s = "lambda.min")
  selection_counts <- setNames(rep(0L, ncol(x_mat)), colnames(x_mat))
  for (b in seq_len(n_bootstrap)) {
    idx <- sample(seq_len(nrow(x_mat)), replace = TRUE)
    boot_fit <- tryCatch(
      glmnet::glmnet(
        x = x_mat[idx, , drop = FALSE],
        y = y_fac[idx],
        family = "multinomial",
        alpha = 0.5,
        lambda = cv_fit$lambda.min
      ),
      error = function(e) NULL
    )
    if (is.null(boot_fit)) next
    boot_coef <- coef(boot_fit, s = cv_fit$lambda.min)
    for (cls in names(boot_coef)) {
      mat <- as.matrix(boot_coef[[cls]])
      nz <- rownames(mat)[abs(mat[, 1]) > 1e-8]
      nz <- setdiff(nz, "(Intercept)")
      selection_counts[nz] <- selection_counts[nz] + 1L
    }
  }
  selection_freq <- selection_counts / n_bootstrap

  enet_rows <- lapply(names(enet_coefs), function(cls) {
    mat <- as.matrix(enet_coefs[[cls]])
    data.frame(
      metric = rownames(mat),
      cluster_id = as.integer(cls),
      elastic_net_coef = as.numeric(mat[, 1]),
      stringsAsFactors = FALSE
    )
  })
  elastic_net <- dplyr::bind_rows(enet_rows) %>%
    dplyr::filter(.data$metric != "(Intercept)") %>%
    dplyr::group_by(.data$metric) %>%
    dplyr::summarise(
      max_abs_coef = max(abs(.data$elastic_net_coef), na.rm = TRUE),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      elastic_net_selection_frequency = selection_freq[match(.data$metric, names(selection_freq))]
    ) %>%
    dplyr::arrange(dplyr::desc(.data$max_abs_coef))

  rf_rank <- stats::setNames(
    seq_along(rf_importance$metric),
    rf_importance$metric
  )
  enet_rank <- stats::setNames(
    seq_along(elastic_net$metric),
    elastic_net$metric
  )
  all_metrics <- union(rf_importance$metric, elastic_net$metric)
  consensus <- data.frame(metric = all_metrics, stringsAsFactors = FALSE) %>%
    dplyr::mutate(
      rf_rank = ifelse(.data$metric %in% names(rf_rank), rf_rank[.data$metric], max(rf_rank) + 1L),
      enet_rank = ifelse(.data$metric %in% names(enet_rank), enet_rank[.data$metric], max(enet_rank) + 1L),
      rf_permutation_importance = rf_importance$rf_permutation_importance[
        match(.data$metric, rf_importance$metric)
      ],
      elastic_net_selection_frequency = elastic_net$elastic_net_selection_frequency[
        match(.data$metric, elastic_net$metric)
      ],
      consensus_feature_score = (max(rf_rank) + 1 - .data$rf_rank) +
        (max(enet_rank) + 1 - .data$enet_rank) +
        ifelse(is.finite(.data$rf_permutation_importance), .data$rf_permutation_importance, 0) +
        ifelse(is.finite(.data$elastic_net_selection_frequency),
               .data$elastic_net_selection_frequency * 2, 0),
      available_in_2024_model = TRUE
    ) %>%
    dplyr::arrange(dplyr::desc(.data$consensus_feature_score))

  performance <- data.frame(
    model = c("random_forest", "elastic_net_multinomial"),
    oob_or_cv_accuracy = c(oob_acc, 1 - min(cv_fit$cvm)),
    loo_accuracy = c(loo_acc, NA_real_),
    n_features = ncol(x_mat),
    n_provinces = nrow(x_mat),
    stringsAsFactors = FALSE
  )

  list(
    performance = performance,
    rf_importance = rf_importance,
    elastic_net = elastic_net,
    consensus = consensus
  )
}
