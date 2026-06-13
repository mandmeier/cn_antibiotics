# Pathway scoring, discovery, multivariable support, and validation helpers.

source("R/utils/cluster_pathways_constants.R")
source("R/utils/cluster_pathways_stats.R")
source("R/utils/cluster_pathways_endpoints.R")

score_coverage <- function(n_provinces, env_records, n_clusters) {
  if (n_provinces >= 24 && env_records >= 100 && n_clusters == 3) return(5L)
  if (n_provinces >= 18 && env_records >= 50 && n_clusters >= 2) return(4L)
  if (n_provinces >= 12 && env_records >= 20 && n_clusters >= 2) return(3L)
  if (n_provinces >= 8) return(2L)
  if (n_provinces >= 5) return(1L)
  0L
}

score_interpretability <- function(env_class, endpoint, matrix) {
  antibiotic <- if (grepl(" \\| ", endpoint)) {
    strsplit(endpoint, " \\| ")[[1]][2]
  } else {
    endpoint
  }
  cls <- endpoint_class(antibiotic)
  ecls <- env_class_norm(env_class)
  if (identical(ecls, cls)) return(5L)
  if (grepl(tolower(as.character(antibiotic)), tolower(as.character(env_class)), fixed = TRUE)) {
    return(5L)
  }
  if (matrix %in% c("wastewater influent", "wastewater effluent", "sludge") &&
      cls %in% c("fluoroquinolones", "macrolides", "sulfonamides")) {
    return(4L)
  }
  if (matrix %in% c("soil", "sediment", "surface water")) return(3L)
  2L
}

score_cluster_contrast <- function(epsilon, p_value) {
  eps <- if (is.finite(epsilon)) epsilon else 0
  p <- if (is.finite(p_value)) p_value else 1
  if (p < 0.05 && eps >= 0.25) return(5L)
  if (p < 0.10 || eps >= 0.20) return(4L)
  if (eps >= 0.12) return(3L)
  if (eps >= 0.08) return(2L)
  if (eps >= 0.04) return(1L)
  0L
}

score_loo <- function(stability, influence) {
  if (!is.finite(stability)) return(0L)
  if (stability >= 0.90 && (!is.finite(influence) || influence <= 0.75)) return(5L)
  if (stability >= 0.85) return(4L)
  if (stability >= 0.75) return(3L)
  if (stability >= 0.60) return(2L)
  if (stability >= 0.50) return(1L)
  0L
}

score_coherence <- function(env_amr, driver_env, driver_amr) {
  vals <- c(env_amr, driver_env, driver_amr)
  if (!all(is.finite(vals))) return(list(score = 0L, coherent = FALSE))
  expected <- sign(driver_env) * sign(env_amr)
  observed <- sign(driver_amr)
  coherent <- isTRUE(expected == observed && observed != 0)
  strengths <- abs(vals)
  if (coherent && min(strengths) >= 0.30) return(list(score = 5L, coherent = TRUE))
  if (coherent && sum(strengths >= 0.25) >= 2) return(list(score = 4L, coherent = TRUE))
  if (coherent && sum(strengths >= 0.20) >= 1) return(list(score = 3L, coherent = TRUE))
  if (coherent) return(list(score = 2L, coherent = TRUE))
  if (abs(env_amr) >= 0.45 && abs(driver_env) >= 0.30 && abs(driver_amr) >= 0.30) {
    return(list(score = 1L, coherent = FALSE))
  }
  list(score = 0L, coherent = FALSE)
}

classify_pathway_row <- function(row) {
  scores <- c(
    row[["coverage_score"]],
    row[["interpretability_score"]],
    row[["cluster_contrast_score"]],
    row[["loo_sign_stability_score"]],
    row[["driver_env_amr_coherence_score"]],
    row[["matrix_consistency_score"]]
  )
  total <- row[["pathway_total_score"]]
  if (row[["coverage_score"]] < 3 || total < 14) return("Too sparse")
  if (!isTRUE(as.logical(row[["coherent_direction"]])) && abs(row[["env_amr_rho"]]) >= 0.35) {
    return("Contradictory")
  }
  if (total >= 22 && row[["coverage_score"]] >= 3 &&
      row[["interpretability_score"]] >= 4 && min(scores) >= 3) {
    return("Manuscript-worthy")
  }
  if (total >= 18 && total <= 21 && row[["coverage_score"]] >= 3 &&
      isTRUE(as.logical(row[["coherent_direction"]]))) {
    return("Strong lead")
  }
  if (total >= 14 && total <= 17) return("Faint lead")
  "Too sparse"
}

sort_pathways <- function(frame) {
  if (is.null(frame) || !nrow(frame)) return(frame)
  out <- frame
  out$pathway_class_rank <- unname(CP_PATHWAY_CLASS_RANK[as.character(out$pathway_class)])
  out$pathway_class_rank[is.na(out$pathway_class_rank)] <- 99L
  ord <- order(
    out$pathway_class_rank,
    -out$pathway_total_score,
    -out$n_provinces,
    out$env_amr_p,
    na.last = TRUE
  )
  out[ord, , drop = FALSE]
}

env_cluster_contrast <- function(env_feature) {
  split_vals <- split(env_feature$env_burden_log_median, env_feature$cluster_id)
  groups_keep <- split_vals[vapply(split_vals, function(x) sum(is.finite(x)) >= 2, logical(1))]
  if (length(groups_keep) < 2) return(list(h = NA_real_, p = NA_real_, eps = NA_real_))
  kw <- tryCatch(stats::kruskal.test(groups_keep), error = function(e) NULL)
  if (is.null(kw)) return(list(h = NA_real_, p = NA_real_, eps = NA_real_))
  h <- unname(as.numeric(kw$statistic))
  p <- kw$p.value
  n <- sum(vapply(groups_keep, function(x) sum(is.finite(x)), integer(1)))
  k <- length(groups_keep)
  eps <- if (n > k) max(0, (h - k + 1) / (n - k)) else NA_real_
  list(h = h, p = p, eps = eps)
}

loo_for_pair <- function(frame, x_col, y_col, full_rho) {
  provinces <- unique(frame$province[!is.na(frame$province)])
  vals <- c()
  influences <- c()
  omitted <- c()
  for (province in provinces) {
    sub <- frame[frame$province != province, , drop = FALSE]
    sp <- safe_spearman(sub[[x_col]], sub[[y_col]])
    rho <- sp$rho
    if (is.finite(rho)) {
      vals <- c(vals, rho)
      influences <- c(influences, abs(rho - full_rho) / max(abs(full_rho), 1e-9))
      omitted <- c(omitted, province)
    }
  }
  if (!length(vals) || !is.finite(full_rho)) {
    return(list(sign_share = NA_real_, influence = NA_real_, province = ""))
  }
  idx <- which.max(influences)
  list(
    sign_share = mean(sign(vals) == sign(full_rho)),
    influence = influences[idx],
    province = omitted[idx]
  )
}

matrix_consistency_scores <- function(pathways) {
  scores <- rep(NA_integer_, nrow(pathways))
  for (i in seq_len(nrow(pathways))) {
    row <- pathways[i, , drop = FALSE]
    peers <- pathways[
      pathways$env_class == row$env_class &
        pathways$endpoint == row$endpoint &
        pathways$scenario == row$scenario &
        is.finite(pathways$env_amr_rho),
      ,
      drop = FALSE
    ]
    if (nrow(peers) <= 1) {
      score <- if (abs(row$env_amr_rho) >= 0.35) 3L else 2L
    } else {
      same <- mean(sign(peers$env_amr_rho) == sign(row$env_amr_rho))
      score <- if (same >= 0.75 && abs(row$env_amr_rho) >= 0.35) {
        5L
      } else if (same >= 0.60) {
        4L
      } else if (same >= 0.50) {
        3L
      } else if (same >= 0.35) {
        2L
      } else {
        0L
      }
    }
    scores[i] <- score
  }
  pathways$matrix_consistency_score <- scores
  pathways
}

discover_pathways <- function(env_features, amr, driver_table, scenario,
                              preferred_driver_cols = NULL) {
  controls <- driver_table[, intersect(CP_DRIVER_COMPOSITES, names(driver_table)), drop = FALSE]
  rownames(controls) <- driver_table$province
  driver_cols <- names(driver_table)[names(driver_table) != "province"]
  driver_cols <- driver_cols[vapply(driver_table[driver_cols], is.numeric, logical(1))]
  driver_cols <- driver_cols[!grepl("^log1p__|^rankpct__", driver_cols)]
  if (!is.null(preferred_driver_cols)) {
    keep <- unique(c(CP_DRIVER_COMPOSITES, preferred_driver_cols[preferred_driver_cols %in% driver_cols]))
    driver_cols <- keep[keep %in% driver_cols]
  }
  driver_by_prov <- driver_table[, c("province", driver_cols), drop = FALSE]
  rownames(driver_by_prov) <- driver_by_prov$province
  endpoint_keep <- sort(table(amr$endpoint), decreasing = TRUE)
  endpoints <- names(endpoint_keep[endpoint_keep >= 20])
  rows <- list()

  split_env <- split(env_features, paste(env_features$matrix, env_features$env_class, sep = "||"))
  for (nm in names(split_env)) {
    env_g <- split_env[[nm]]
    matrix <- env_g$matrix[1]
    env_class <- env_g$env_class[1]
    env_s <- env_g$env_burden_log_median
    names(env_s) <- env_g$province
    env_records <- sum(env_g$env_records_total, na.rm = TRUE)
    env_refs <- sum(env_g$env_refs, na.rm = TRUE)
    n_units <- if (nrow(env_g)) max(env_g$n_units, na.rm = TRUE) else 0
    contrast <- env_cluster_contrast(env_g)

    for (endpoint in endpoints) {
      amr_g <- amr[amr$endpoint == endpoint, c("province", "amr_mean_2019_2024"), drop = FALSE]
      frame <- merge(
        data.frame(province = names(env_s), env = as.numeric(env_s), stringsAsFactors = FALSE),
        amr_g,
        by = "province",
        all = FALSE
      )
      names(frame)[names(frame) == "amr_mean_2019_2024"] <- "amr"
      frame <- frame[is.finite(frame$env) & is.finite(frame$amr), , drop = FALSE]
      if (nrow(frame) < 5) next
      sp <- safe_spearman(frame$env, frame$amr)
      rho <- sp$rho
      p <- sp$p
      if (!is.finite(rho)) next
      control_sub <- controls[frame$province, , drop = FALSE]
      ps <- partial_spearman(frame$env, frame$amr, control_sub)
      partial_rho <- ps$rho
      partial_p <- ps$p
      loo <- loo_for_pair(frame, "env", "amr", rho)
      coverage_score <- score_coverage(
        nrow(frame), env_records, length(unique(env_g$cluster_id))
      )
      minimal_signal <- abs(rho) >= 0.25 ||
        (is.finite(p) && p <= 0.25) ||
        (is.finite(partial_rho) && abs(partial_rho) >= 0.25) ||
        (is.finite(partial_p) && partial_p <= 0.25)
      candidate_drivers <- if (coverage_score >= 2 && minimal_signal) {
        driver_cols
      } else {
        CP_DRIVER_COMPOSITES
      }

      best <- list(
        driver_metric = "",
        driver_env_rho = NA_real_,
        driver_env_p = NA_real_,
        driver_amr_rho = NA_real_,
        driver_amr_p = NA_real_,
        coherence_score = 0L,
        coherent = FALSE,
        selection_value = -1
      )

      if (length(candidate_drivers)) {
        driver_sub <- driver_by_prov[frame$province, candidate_drivers, drop = FALSE]
        de <- fast_spearman_corrwith(driver_sub, frame$env)
        da <- fast_spearman_corrwith(driver_sub, frame$amr)
        score_rows <- lapply(candidate_drivers, function(driver) {
          de_rho <- unname(de[[driver]])
          da_rho <- unname(da[[driver]])
          coh <- score_coherence(rho, de_rho, da_rho)
          selection_value <- coh$score * 10 +
            abs(ifelse(is.finite(de_rho), de_rho, 0)) +
            abs(ifelse(is.finite(da_rho), da_rho, 0))
          list(
            selection_value = selection_value,
            driver = driver,
            de_rho = de_rho,
            da_rho = da_rho,
            coherence_score = coh$score,
            coherent = coh$coherent
          )
        })
        score_rows <- score_rows[order(
          vapply(score_rows, `[[`, numeric(1), "selection_value"),
          decreasing = TRUE
        )]
        if (length(score_rows)) {
          pick <- score_rows[[1]]
          d <- driver_by_prov[frame$province, pick$driver, drop = TRUE]
          de_p <- safe_spearman(d, frame$env)$p
          da_p <- safe_spearman(d, frame$amr)$p
          best <- list(
            driver_metric = pick$driver,
            driver_env_rho = pick$de_rho,
            driver_env_p = de_p,
            driver_amr_rho = pick$da_rho,
            driver_amr_p = da_p,
            coherence_score = pick$coherence_score,
            coherent = pick$coherent,
            selection_value = pick$selection_value
          )
        }
      }

      interpretability_score <- score_interpretability(env_class, endpoint, matrix)
      cluster_contrast_score <- score_cluster_contrast(contrast$eps, contrast$p)
      loo_score <- score_loo(loo$sign_share, loo$influence)
      rows[[length(rows) + 1]] <- data.frame(
        scenario = scenario,
        matrix = matrix,
        env_class = env_class,
        endpoint = endpoint,
        n_provinces = nrow(frame),
        n_clusters = length(unique(env_g$cluster_id)),
        env_records_total = env_records,
        env_refs_total = env_refs,
        n_units = n_units,
        env_year_min = suppressWarnings(min(as.numeric(env_g$env_year_min), na.rm = TRUE)),
        env_year_max = suppressWarnings(max(as.numeric(env_g$env_year_max), na.rm = TRUE)),
        env_amr_rho = rho,
        env_amr_p = p,
        partial_env_amr_rho = partial_rho,
        partial_env_amr_p = partial_p,
        cluster_contrast_p = contrast$p,
        cluster_contrast_epsilon = contrast$eps,
        driver_metric = best$driver_metric,
        driver_env_rho = best$driver_env_rho,
        driver_env_p = best$driver_env_p,
        driver_amr_rho = best$driver_amr_rho,
        driver_amr_p = best$driver_amr_p,
        coherent_direction = best$coherent,
        loo_sign_stability = loo$sign_share,
        loo_influence_ratio = loo$influence,
        loo_most_influential_province = loo$province,
        coverage_score = coverage_score,
        interpretability_score = interpretability_score,
        cluster_contrast_score = cluster_contrast_score,
        loo_sign_stability_score = loo_score,
        driver_env_amr_coherence_score = best$coherence_score,
        stringsAsFactors = FALSE
      )
    }
  }

  out <- if (length(rows)) do.call(rbind, rows) else data.frame()
  if (!nrow(out)) return(out)
  out$env_amr_q_fdr <- bh_fdr(out$env_amr_p)
  out$partial_env_amr_q_fdr <- bh_fdr(out$partial_env_amr_p)
  out <- matrix_consistency_scores(out)
  score_cols <- c(
    "coverage_score", "interpretability_score", "cluster_contrast_score",
    "loo_sign_stability_score", "driver_env_amr_coherence_score",
    "matrix_consistency_score"
  )
  out$pathway_total_score <- rowSums(out[, score_cols, drop = FALSE], na.rm = TRUE)
  out$pathway_class <- vapply(
    seq_len(nrow(out)),
    function(i) classify_pathway_row(out[i, , drop = FALSE]),
    character(1)
  )
  sort_pathways(out)
}

pathway_support_label <- function(row) {
  stable <- suppressWarnings(as.numeric(row[["bootstrap_sign_stability"]]))
  env_selected <- isTRUE(as.logical(row[["elastic_net_env_selected"]]))
  adjusted_same <- isTRUE(as.logical(row[["adjusted_env_direction_matches_screen"]]))
  if (is.finite(stable) && stable >= 0.80 && adjusted_same && env_selected) return("reinforced")
  if (is.finite(stable) && stable >= 0.65 && adjusted_same) return("partly reinforced")
  if (is.finite(stable) && stable < 0.50) return("unstable")
  "screen-only"
}

support_sort <- function(df) {
  if (is.null(df) || !nrow(df)) return(df)
  support_rank_map <- c(
    "reinforced" = 1L,
    "partly reinforced" = 2L,
    "screen-only" = 3L,
    "unstable" = 4L
  )
  df$support_rank <- unname(support_rank_map[as.character(df$multivariable_env_support)])
  df$support_rank[is.na(df$support_rank)] <- 99L
  ord <- order(
    df$support_rank,
    -df$pathway_total_score,
    -ifelse(is.finite(df$bootstrap_sign_stability), df$bootstrap_sign_stability, -Inf),
    -df$n_provinces_model,
    na.last = TRUE
  )
  df[ord, , drop = FALSE]
}

attach_pathway_support <- function(pathways, support) {
  out <- pathways
  support_cols <- c(
    "rf_driver_support", "elastic_net_driver_support",
    "multivariable_env_support", "bootstrap_sign_stability"
  )
  for (col in support_cols) {
    if (!(col %in% names(out))) out[[col]] <- ""
  }
  if (is.null(support) || !nrow(support)) return(out)
  key <- c("scenario", "matrix", "env_class", "endpoint", "driver_metric")
  keep <- unique(support[, c(key, support_cols), drop = FALSE])
  merged <- merge(out, keep, by = key, all.x = TRUE, suffixes = c("", "_model"))
  for (col in support_cols) {
    model_col <- paste0(col, "_model")
    if (model_col %in% names(merged)) {
      base_vals <- merged[[col]]
      model_vals <- merged[[model_col]]
      if (is.numeric(model_vals)) {
        merged[[col]] <- ifelse(is.finite(model_vals), model_vals, base_vals)
      } else {
        merged[[col]] <- ifelse(
          !is.na(model_vals) & nzchar(as.character(model_vals)),
          model_vals,
          base_vals
        )
      }
      merged[[model_col]] <- NULL
    }
  }
  merged
}

build_pathway_multivariable_support <- function(
    env_features,
    amr,
    driver_table,
    pathways,
    feature_consensus,
    cap_strong_leads = 12L) {
  primary <- sort_pathways(pathways[pathways$scenario == "sample_year_ge_2012", , drop = FALSE])
  selected <- primary[primary$pathway_class == "Manuscript-worthy", , drop = FALSE]
  strong <- primary[primary$pathway_class == "Strong lead", , drop = FALSE]
  if (nrow(strong) > cap_strong_leads) strong <- strong[seq_len(cap_strong_leads), , drop = FALSE]
  selected <- unique(rbind(selected, strong))
  selected <- selected[
    !duplicated(selected[, c("scenario", "matrix", "env_class", "endpoint", "driver_metric")]),
    ,
    drop = FALSE
  ]
  if (!nrow(selected)) {
    return(list(
      selected = selected,
      support = data.frame(),
      summary = "No primary-window pathways were eligible for multivariable support modeling."
    ))
  }

  drivers <- as.data.frame(driver_table)
  rownames(drivers) <- drivers$province
  feature_rank <- as.data.frame(feature_consensus)
  feature_rank$consensus_rank <- seq_len(nrow(feature_rank))
  rownames(feature_rank) <- feature_rank$metric
  cluster_lookup <- as.data.frame(unique(env_features[, c("province", "cluster_id"), drop = FALSE]))
  rownames(cluster_lookup) <- cluster_lookup$province
  lambdas <- 10^seq(-2, 2, length.out = 20)
  rng_seed <- 123L
  rows <- list()

  for (i in seq_len(nrow(selected))) {
    p <- selected[i, , drop = FALSE]
    env_s <- env_features[
      env_features$matrix == p$matrix & env_features$env_class == p$env_class,
      c("province", "env_burden_log_median"),
      drop = FALSE
    ]
    amr_s <- amr[amr$endpoint == p$endpoint, c("province", "amr_mean_2019_2024"), drop = FALSE]
    frame <- merge(env_s, amr_s, by = "province", all = FALSE)
    names(frame)[names(frame) == "env_burden_log_median"] <- "env"
    names(frame)[names(frame) == "amr_mean_2019_2024"] <- "amr"
    frame <- merge(frame, drivers, by = "province", all = FALSE)
    frame <- frame[is.finite(frame$env) & is.finite(frame$amr), , drop = FALSE]
    if (nrow(frame) < 10 || length(unique(frame$amr)) < 2) next

    driver_metric <- as.character(p$driver_metric)
    predictors <- c("env")
    if (nzchar(driver_metric) && driver_metric %in% names(frame) &&
        !(driver_metric %in% predictors)) {
      predictors <- c(predictors, driver_metric)
    }
    for (col in CP_DRIVER_COMPOSITES) {
      if (col %in% names(frame) && !(col %in% predictors)) predictors <- c(predictors, col)
    }
    frame$cluster_id <- cluster_lookup[frame$province, "cluster_id"]
    for (cid in c(2L, 3L)) {
      cname <- paste0("cluster_", cid)
      frame[[cname]] <- as.numeric(frame$cluster_id == cid)
      predictors <- c(predictors, cname)
    }

    x_raw <- frame[, predictors, drop = FALSE]
    for (col in names(x_raw)) {
      x_raw[[col]] <- suppressWarnings(as.numeric(x_raw[[col]]))
      med <- stats::median(x_raw[[col]], na.rm = TRUE)
      if (!is.finite(med)) med <- 0
      x_raw[[col]][!is.finite(x_raw[[col]])] <- med
    }
    x <- scale(x_raw)
    x <- as.matrix(x)
    colnames(x) <- names(x_raw)
    y <- suppressWarnings(as.numeric(frame$amr))
    if (length(unique(y[is.finite(y)])) < 2) next
    y_centered <- y - mean(y, na.rm = TRUE)

    ridge_lambda <- select_ridge_lambda(x, y_centered, lambdas)
    ridge_beta <- ridge_fit_one(x, y_centered, ridge_lambda)
    names(ridge_beta) <- colnames(x)

    loo_pred <- rep(NA_real_, nrow(x))
    for (k in seq_len(nrow(x))) {
      train_idx <- setdiff(seq_len(nrow(x)), k)
      lambda_k <- select_ridge_lambda(x[train_idx, , drop = FALSE], y_centered[train_idx], lambdas)
      beta_k <- ridge_fit_one(x[train_idx, , drop = FALSE], y_centered[train_idx], lambda_k)
      if (all(is.finite(beta_k))) loo_pred[k] <- sum(x[k, ] * beta_k)
    }
    loo_r2 <- if (length(unique(y_centered[is.finite(y_centered)])) > 1 && any(is.finite(loo_pred))) {
      sse <- sum((y_centered[is.finite(loo_pred)] - loo_pred[is.finite(loo_pred)])^2)
      sst <- sum((y_centered[is.finite(loo_pred)] - mean(y_centered[is.finite(loo_pred)]))^2)
      if (sst > 0) 1 - sse / sst else NA_real_
    } else {
      NA_real_
    }

    env_full <- unname(ridge_beta[["env"]])
    boot_env <- c()
    set.seed(rng_seed + i)
    for (b in seq_len(30)) {
      idx <- sample(seq_len(nrow(x)), size = nrow(x), replace = TRUE)
      yb <- y_centered[idx]
      if (length(unique(yb[is.finite(yb)])) < 2) next
      lambda_b <- select_ridge_lambda(x[idx, , drop = FALSE], yb, lambdas)
      beta_b <- ridge_fit_one(x[idx, , drop = FALSE], yb, lambda_b)
      if (length(beta_b) && all(is.finite(beta_b))) {
        boot_env <- c(boot_env, beta_b[match("env", colnames(x))])
      }
    }
    boot_env <- boot_env[is.finite(boot_env)]
    sign_stability <- ci_low <- ci_high <- NA_real_
    if (length(boot_env) >= 20 && is.finite(env_full)) {
      sign_stability <- mean(sign(boot_env) == sign(env_full))
      ci_low <- as.numeric(stats::quantile(boot_env, 0.025, na.rm = TRUE))
      ci_high <- as.numeric(stats::quantile(boot_env, 0.975, na.rm = TRUE))
    }
    adjusted_matches <- is.finite(env_full) && is.finite(p$env_amr_rho) &&
      sign(env_full) == sign(p$env_amr_rho)

    lm_df <- data.frame(amr = y_centered, x_raw, check.names = FALSE)
    elastic_env_selected <- FALSE
    elastic_driver_selected <- FALSE
    elastic_env_coef <- NA_real_
    if (ncol(x_raw) >= 1) {
      full_formula <- stats::as.formula(paste("amr ~", paste(colnames(x_raw), collapse = " + ")))
      lm_fit <- tryCatch(stats::lm(full_formula, data = lm_df), error = function(e) NULL)
      step_fit <- tryCatch(
        if (!is.null(lm_fit)) MASS::stepAIC(lm_fit, direction = "both", trace = FALSE) else NULL,
        error = function(e) NULL
      )
      coef_vec <- if (!is.null(step_fit)) stats::coef(step_fit) else numeric()
      elastic_env_selected <- "env" %in% names(coef_vec)
      elastic_driver_selected <- nzchar(driver_metric) && driver_metric %in% names(coef_vec)
      if (elastic_env_selected) elastic_env_coef <- unname(coef_vec[["env"]])
    }

    rf_support <- "not yearbook metric"
    enet_support <- "not yearbook metric"
    consensus_score <- NA_real_
    if (nzchar(driver_metric) && driver_metric %in% rownames(feature_rank)) {
      fr <- feature_rank[driver_metric, , drop = FALSE]
      consensus_score <- suppressWarnings(as.numeric(fr$consensus_feature_score[1]))
      rf_support <- if (suppressWarnings(as.integer(fr$consensus_rank[1])) <= 15) {
        "top15"
      } else {
        "not top15"
      }
      enet_freq <- suppressWarnings(as.numeric(fr$elastic_net_selection_frequency[1]))
      enet_support <- if (is.finite(enet_freq) && enet_freq >= 0.50) {
        "selected"
      } else {
        "not selected"
      }
    }

    row <- data.frame(
      scenario = p$scenario,
      matrix = p$matrix,
      env_class = p$env_class,
      endpoint = p$endpoint,
      driver_metric = driver_metric,
      pathway_class = p$pathway_class,
      pathway_total_score = p$pathway_total_score,
      n_provinces_model = nrow(frame),
      ridge_alpha = ridge_lambda,
      ridge_env_coef = env_full,
      ridge_driver_coef = if (nzchar(driver_metric) && driver_metric %in% names(ridge_beta)) {
        unname(ridge_beta[[driver_metric]])
      } else {
        NA_real_
      },
      elastic_net_env_coef = elastic_env_coef,
      elastic_net_env_selected = elastic_env_selected,
      elastic_net_driver_selected = elastic_driver_selected,
      loo_r2 = loo_r2,
      bootstrap_env_coef_low = ci_low,
      bootstrap_env_coef_high = ci_high,
      bootstrap_sign_stability = sign_stability,
      adjusted_env_direction_matches_screen = adjusted_matches,
      rf_driver_support = rf_support,
      elastic_net_driver_support = enet_support,
      driver_consensus_feature_score = consensus_score,
      stringsAsFactors = FALSE
    )
    row$multivariable_env_support <- pathway_support_label(row)
    rows[[length(rows) + 1]] <- row
  }

  support <- if (length(rows)) do.call(rbind, rows) else data.frame()
  if (!nrow(support)) {
    return(list(
      selected = selected,
      support = support,
      summary = paste0(
        "No selected primary-window pathways had enough complete province ",
        "coverage for multivariable support modeling."
      )
    ))
  }
  support <- support_sort(support)
  counts <- sort(table(support$multivariable_env_support), decreasing = TRUE)
  summary <- paste0(
    "# Module 1.2 Pathway Modeling Summary\n\n",
    "Multivariable models are used as descriptive support checks, not causal tests. ",
    nrow(support), " primary-window pathways had enough province coverage for support modeling. ",
    paste(sprintf("%s: %s", names(counts), as.integer(counts)), collapse = "; "),
    "."
  )
  list(selected = selected, support = support, summary = summary)
}

build_matrix_trends <- function(env_features, pathways) {
  pathway_pool <- pathways
  pathway_pool$class_concordant <- mapply(
    endpoint_matches_env_class,
    pathway_pool$endpoint,
    pathway_pool$env_class
  )
  pathway_pool$core_driver <- pathway_pool$driver_metric %in% CP_CORE_MANUSCRIPT_DRIVERS
  preferred_pool <- pathway_pool[
    pathway_pool$class_concordant &
      pathway_pool$core_driver &
      pathway_pool$multivariable_env_support %in% c("reinforced", "partly reinforced"),
    ,
    drop = FALSE
  ]
  rows <- list()
  split_grp <- split(
    env_features,
    paste(env_features$cluster_id, env_features$matrix, env_features$env_class, sep = "||")
  )
  for (nm in names(split_grp)) {
    g <- split_grp[[nm]]
    cluster_id <- g$cluster_id[1]
    matrix <- g$matrix[1]
    env_class <- g$env_class[1]
    related <- preferred_pool[
      preferred_pool$matrix == matrix & preferred_pool$env_class == env_class,
      ,
      drop = FALSE
    ]
    if (!nrow(related)) {
      related <- pathway_pool[
        pathway_pool$matrix == matrix &
          pathway_pool$env_class == env_class &
          pathway_pool$class_concordant &
          pathway_pool$core_driver,
        ,
        drop = FALSE
      ]
    }
    if (nrow(related)) {
      related <- related[order(-related$pathway_total_score, -related$n_provinces), , drop = FALSE]
    }
    top <- if (nrow(related)) related[1, , drop = FALSE] else NULL
    rows[[length(rows) + 1]] <- data.frame(
      cluster_id = as.integer(cluster_id),
      cluster_name = unname(CP_CLUSTER_NAMES[as.character(cluster_id)]),
      matrix = matrix,
      env_class = env_class,
      n_provinces = length(unique(g$province)),
      env_records_total = sum(g$env_records_total, na.rm = TRUE),
      median_env_burden_log = stats::median(g$env_burden_log_median, na.rm = TRUE),
      dominant_endpoint_from_pathway_screen = if (!is.null(top)) top$endpoint else "",
      top_pathway_score = if (!is.null(top)) top$pathway_total_score else NA_real_,
      top_pathway_class = if (!is.null(top)) top$pathway_class else "Too sparse",
      sparse_or_too_little_data = length(unique(g$province)) < 5 ||
        sum(g$env_records_total, na.rm = TRUE) < 10,
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, rows)
  out[order(out$cluster_id, out$matrix, -out$top_pathway_score), , drop = FALSE]
}

build_reservoir_table <- function(matrix_trends, pathways) {
  top_pathways <- pathways[pathways$scenario == "sample_year_ge_2012", , drop = FALSE]
  top_pathways$class_concordant <- mapply(
    endpoint_matches_env_class,
    top_pathways$endpoint,
    top_pathways$env_class
  )
  top_pathways$core_driver <- top_pathways$driver_metric %in% CP_CORE_MANUSCRIPT_DRIVERS
  preferred <- top_pathways[
    top_pathways$class_concordant &
      top_pathways$core_driver &
      top_pathways$multivariable_env_support %in% c("reinforced", "partly reinforced"),
    ,
    drop = FALSE
  ]
  fallback <- top_pathways[
    top_pathways$class_concordant & top_pathways$core_driver,
    ,
    drop = FALSE
  ]
  rows <- list()
  for (i in seq_len(nrow(matrix_trends))) {
    trend <- matrix_trends[i, , drop = FALSE]
    sub <- preferred[preferred$matrix == trend$matrix & preferred$env_class == trend$env_class, , drop = FALSE]
    if (!nrow(sub)) {
      sub <- fallback[fallback$matrix == trend$matrix & fallback$env_class == trend$env_class, , drop = FALSE]
    }
    if (nrow(sub)) sub <- sub[order(-sub$pathway_total_score, -sub$n_provinces), , drop = FALSE]
    best <- if (nrow(sub)) sub[1, , drop = FALSE] else NULL
    sparse <- isTRUE(trend$sparse_or_too_little_data)
    score <- if (!is.null(best)) best$pathway_total_score else NA_real_
    pclass <- if (!is.null(best)) as.character(best$pathway_class) else "Too sparse"
    interpretation <- if (sparse) {
      "sparse / too little data"
    } else if (pclass == "Manuscript-worthy") {
      "candidate descriptive reservoir"
    } else if (pclass == "Strong lead") {
      "strong reservoir lead"
    } else if (pclass == "Faint lead") {
      "faint reservoir lead"
    } else if (pclass == "Contradictory") {
      "contradictory reservoir evidence"
    } else {
      "not supported as reservoir"
    }
    rows[[length(rows) + 1]] <- data.frame(
      cluster_id = as.integer(trend$cluster_id),
      cluster_name = as.character(trend$cluster_name),
      matrix = as.character(trend$matrix),
      antibiotic_or_class = as.character(trend$env_class),
      amr_endpoint_or_index = if (!is.null(best)) as.character(best$endpoint) else "",
      burden_signal = as.numeric(trend$median_env_burden_log),
      pathway_score = score,
      pathway_class = pclass,
      reservoir_interpretation = interpretation,
      sparse_or_too_little_data = sparse,
      notes = paste(
        "Reservoir language is descriptive and requires coverage, coherent direction,",
        "LOO stability, and matrix evidence."
      ),
      stringsAsFactors = FALSE
    )
  }
  out <- do.call(rbind, rows)
  out[order(-out$pathway_score, -out$burden_signal), , drop = FALSE]
}

build_exception_table <- function(env_features, pathways) {
  selected <- sort_pathways(
    pathways[pathways$pathway_class %in% c("Manuscript-worthy", "Strong lead", "Faint lead"), , drop = FALSE]
  )
  if (nrow(selected) > 60) selected <- selected[1:60, , drop = FALSE]
  rows <- list()
  for (i in seq_len(nrow(selected))) {
    p <- selected[i, , drop = FALSE]
    feat <- env_features[
      env_features$matrix == p$matrix & env_features$env_class == p$env_class,
      ,
      drop = FALSE
    ]
    if (!nrow(feat)) next
    q90 <- as.numeric(stats::quantile(feat$env_burden_log_median, 0.90, na.rm = TRUE))
    q10 <- as.numeric(stats::quantile(feat$env_burden_log_median, 0.10, na.rm = TRUE))
    for (j in seq_len(nrow(feat))) {
      r <- feat[j, , drop = FALSE]
      labels <- c()
      evidence <- c()
      if (is.finite(r$env_burden_log_median) && r$env_burden_log_median >= q90) {
        labels <- c(labels, "matrix hotspot")
        evidence <- c(evidence, sprintf("%s %s burden >= p90", p$matrix, p$env_class))
      }
      if (is.finite(r$env_burden_log_median) && r$env_burden_log_median <= q10 && p$pathway_total_score >= 18) {
        labels <- c(labels, "signal reverser")
        evidence <- c(evidence, sprintf(
          "low %s %s burden within a high-scoring pathway", p$matrix, p$env_class
        ))
      }
      q90_records <- as.numeric(stats::quantile(feat$env_records_total, 0.90, na.rm = TRUE))
      if (is.finite(r$env_records_total) && r$env_records_total >= q90_records) {
        labels <- c(labels, "coverage outlier")
        evidence <- c(evidence, sprintf(
          "environmental record count high for %s %s", p$matrix, p$env_class
        ))
      }
      if (nzchar(as.character(p$loo_most_influential_province)) &&
          identical(as.character(r$province), as.character(p$loo_most_influential_province))) {
        labels <- c(labels, "signal amplifier")
        evidence <- c(evidence, "most influential province in LOO check")
      }
      if (length(labels)) {
        rows[[length(rows) + 1]] <- data.frame(
          province = as.character(r$province),
          cluster_id = as.integer(r$cluster_id),
          cluster_name = as.character(r$cluster_name),
          exception_labels = paste(sort(unique(labels)), collapse = "; "),
          pathway_evidence = sprintf("%s | %s | %s", p$matrix, p$env_class, p$endpoint),
          pathway_class = as.character(p$pathway_class),
          pathway_score = as.numeric(p$pathway_total_score),
          specific_evidence = paste(evidence, collapse = "; "),
          stringsAsFactors = FALSE
        )
      }
    }
  }
  if (!length(rows)) {
    return(data.frame(
      province = character(),
      cluster_id = integer(),
      cluster_name = character(),
      exception_labels = character(),
      pathway_evidence = character(),
      pathway_class = character(),
      pathway_score = numeric(),
      specific_evidence = character(),
      stringsAsFactors = FALSE
    ))
  }
  out <- unique(do.call(rbind, rows))
  out[order(-out$pathway_score, out$province), , drop = FALSE]
}

load_current_maintext_signals <- function(pathways) {
  primary <- pathways[
    pathways$scenario == "sample_year_ge_2012" &
      !(pathways$env_class %in% c("other", "multiple classes")),
    ,
    drop = FALSE
  ]
  primary <- primary[
    order(primary$pathway_class_rank, -primary$pathway_total_score, -primary$n_provinces, na.last = TRUE),
    ,
    drop = FALSE
  ]
  if (nrow(primary) > 8) primary <- primary[seq_len(8), , drop = FALSE]
  primary$thread_id <- sprintf("T%d", seq_len(nrow(primary)))
  primary$cluster_short <- "Current shortlist"
  primary$current_signal_source <- "pathway_shortlist_fallback"
  primary$support_label <- ifelse(
    is.na(primary$multivariable_env_support) | primary$multivariable_env_support == "",
    "screen-only",
    as.character(primary$multivariable_env_support)
  )
  primary$signal_tier <- as.character(primary$pathway_class)
  primary$curation_note <- "Shortlist derived from Module 1.2 pathway scoring output."
  primary
}

build_interpretation_validation <- function(current_signals, pathways, exceptions) {
  primary <- pathways[pathways$scenario == "sample_year_ge_2012", , drop = FALSE]
  all_years <- pathways[pathways$scenario == "all_years", , drop = FALSE]
  primary <- primary[
    order(primary$pathway_class_rank, -primary$pathway_total_score, -primary$n_provinces, na.last = TRUE),
    ,
    drop = FALSE
  ]
  all_years <- all_years[
    order(all_years$pathway_class_rank, -all_years$pathway_total_score, -all_years$n_provinces, na.last = TRUE),
    ,
    drop = FALSE
  ]
  primary$primary_rank <- seq_len(nrow(primary))
  all_years$all_years_rank <- seq_len(nrow(all_years))

  rows <- list()
  for (i in seq_len(nrow(current_signals))) {
    signal <- current_signals[i, , drop = FALSE]
    key_mask_p <- primary$matrix == signal$matrix &
      primary$env_class == signal$env_class &
      primary$endpoint == signal$endpoint
    key_mask_a <- all_years$matrix == signal$matrix &
      all_years$env_class == signal$env_class &
      all_years$endpoint == signal$endpoint
    p <- if (any(key_mask_p, na.rm = TRUE)) primary[which(key_mask_p)[1], , drop = FALSE] else NULL
    a <- if (any(key_mask_a, na.rm = TRUE)) all_years[which(key_mask_a)[1], , drop = FALSE] else NULL

    evidence_key <- sprintf("%s | %s | %s", signal$matrix, signal$env_class, signal$endpoint)
    related_ex <- exceptions[exceptions$pathway_evidence == evidence_key, , drop = FALSE]
    exception_labels <- if (nrow(related_ex)) {
      paste(sort(unique(as.character(related_ex$exception_labels))), collapse = "; ")
    } else {
      ""
    }
    exception_provinces <- if (nrow(related_ex)) {
      paste(sort(unique(as.character(related_ex$province))), collapse = "; ")
    } else {
      ""
    }

    if (is.null(p)) {
      input_status <- "FAIL"
      coverage_adequate <- FALSE
      time_window_status <- "window-sensitive"
      model_support <- "missing primary pathway row"
      matrix_audit <- "missing primary pathway row"
      interpretation_status <- "fragile"
      final_label <- "removed from main-text shortlist"
      note <- "Current signal is not present in the primary Module 1.2 pathway screen."
      support_label <- signal[["multivariable_env_support"]] %||% signal[["support_label"]] %||% ""
      primary_rank <- NA_real_
      all_rank <- NA_real_
      loo <- NA_real_
      influence <- NA_real_
      n_prov <- NA_real_
      sparse_flag <- isTRUE(signal[["sparse_or_too_little_data"]])
      driver_metric <- ""
      all_years_pathway_class <- ""
    } else {
      coverage_adequate <- isTRUE(p$coverage_score >= 3 & p$n_provinces >= 12)
      input_status <- "PASS"
      if (!coverage_adequate) {
        input_status <- "FAIL"
      } else if ((is.finite(p$n_units) && p$n_units > 1) || p$n_provinces < 15) {
        input_status <- "PASS WITH NOTE"
      }

      sign_primary <- if (is.finite(p$env_amr_rho)) sign(p$env_amr_rho) else 0
      sign_all <- if (!is.null(a) && is.finite(a$env_amr_rho)) sign(a$env_amr_rho) else 0
      if (is.null(a) || sign_all == 0 || sign_primary == 0 || sign_primary != sign_all) {
        time_window_status <- "window-sensitive"
      } else {
        similar_strength <- abs(abs(as.numeric(p$env_amr_rho)) - abs(as.numeric(a$env_amr_rho))) <= 0.10
        strong_counterpart <- as.character(a$pathway_class) %in% c("Manuscript-worthy", "Strong lead")
        if (strong_counterpart && similar_strength &&
            abs(as.integer(p$primary_rank) - as.integer(a$all_years_rank)) <= 6) {
          time_window_status <- "stable across windows"
        } else {
          time_window_status <- "direction retained, rank shifted"
        }
      }

      support_label <- as.character(
        p[["multivariable_env_support"]] %||%
          signal[["multivariable_env_support"]] %||%
          signal[["support_label"]] %||%
          ""
      )
      if (is.na(support_label) || !nzchar(trimws(support_label))) support_label <- "screen-only"
      driver_metric <- as.character(p[["driver_metric"]] %||% "")
      mc <- as.integer(p[["matrix_consistency_score"]] %||% 0)
      loo <- as.numeric(p[["loo_sign_stability"]] %||% NA_real_)
      influence <- as.numeric(p[["loo_influence_ratio"]] %||% NA_real_)
      n_prov <- as.numeric(p[["n_provinces"]] %||% NA_real_)
      has_feature_model <- identical(as.character(p[["rf_driver_support"]] %||% ""), "top15") ||
        identical(as.character(p[["elastic_net_driver_support"]] %||% ""), "selected")
      if (support_label %in% c("reinforced", "partly reinforced") && has_feature_model) {
        model_support <- "cross-method support"
      } else if (support_label %in% c("reinforced", "partly reinforced") &&
                 driver_metric %in% CP_DRIVER_COMPOSITES) {
        model_support <- "composite-driver + multivariable support"
      } else if (support_label %in% c("reinforced", "partly reinforced")) {
        model_support <- "multivariable-only support"
      } else if (has_feature_model) {
        model_support <- "feature-model-only support"
      } else {
        model_support <- "limited cross-check support"
      }

      matrix_audit <- if (mc >= 4) {
        "repeated / matrix-consistent"
      } else if (mc == 3) {
        "matrix-specific but acceptable"
      } else {
        "weak matrix consistency"
      }

      sparse_flag <- isTRUE(signal[["sparse_or_too_little_data"]])
      interpretation_status <- if (sparse_flag) {
        "sparse"
      } else if (identical(support_label, "unstable") ||
                 identical(time_window_status, "window-sensitive") ||
                 (!is.na(loo) && loo < 0.85) || mc <= 2) {
        "fragile"
      } else if (identical(support_label, "screen-only")) {
        "fragile"
      } else if (identical(support_label, "partly reinforced") ||
                 identical(time_window_status, "direction retained, rank shifted") ||
                 (!is.na(influence) && influence > 0.35)) {
        "conditionally stable"
      } else {
        "stable"
      }

      final_label <- if (identical(input_status, "FAIL")) {
        "removed from main-text shortlist"
      } else if (sparse_flag) {
        "supplement-only"
      } else if (identical(interpretation_status, "fragile")) {
        "removed from main-text shortlist"
      } else if (identical(interpretation_status, "conditionally stable")) {
        "conditional lead"
      } else {
        "validated lead"
      }

      notes <- character()
      if (identical(time_window_status, "direction retained, rank shifted")) {
        notes <- c(notes, "signal direction retained but strength/order changed in the all-years sensitivity.")
      }
      if (identical(time_window_status, "window-sensitive")) {
        notes <- c(notes, "all-years comparison weakens or destabilizes the signal.")
      }
      if (sparse_flag) {
        notes <- c(notes, "cluster-specific reservoir row is sparse and should not anchor strong claims.")
      }
      if (!is.na(influence) && influence > 0.35) {
        notes <- c(notes, sprintf("LOO influence is moderate (ratio %.2f).", influence))
      }
      if (nrow(related_ex)) {
        notes <- c(notes, sprintf("Exception provinces: %s.", exception_provinces))
      }
      note <- paste(notes, collapse = " ")
      primary_rank <- as.integer(p$primary_rank)
      all_rank <- if (!is.null(a)) as.integer(a$all_years_rank) else NA_real_
      all_years_pathway_class <- if (!is.null(a)) as.character(a$pathway_class) else ""
    }

    rows[[length(rows) + 1]] <- data.frame(
      thread_id = as.character(signal[["thread_id"]] %||% ""),
      current_signal_source = as.character(signal[["current_signal_source"]] %||% ""),
      cluster_name = as.character(signal[["cluster_name"]] %||% ""),
      cluster_short = as.character(signal[["cluster_short"]] %||% ""),
      matrix = as.character(signal$matrix),
      env_class = as.character(signal$env_class),
      endpoint = as.character(signal$endpoint),
      driver_metric = driver_metric,
      input_validation_status = input_status,
      coverage_adequate = coverage_adequate,
      n_provinces_primary = n_prov,
      loo_sign_stability = loo,
      loo_influence_ratio = influence,
      primary_rank = primary_rank,
      all_years_rank = all_rank,
      all_years_pathway_class = all_years_pathway_class,
      time_window_validation = time_window_status,
      model_support_triangulation = model_support,
      matrix_consistency_audit = matrix_audit,
      exception_provinces = exception_provinces,
      exception_labels = exception_labels,
      sparse_or_too_little_data = sparse_flag,
      multivariable_env_support = support_label,
      interpretation_validation_status = interpretation_status,
      final_label = final_label,
      validation_note = note,
      stringsAsFactors = FALSE
    )
  }
  if (!length(rows)) return(data.frame())
  do.call(rbind, rows)
}

build_signal_promotion_audit <- function(interpretation) {
  out <- interpretation
  out$affected_manuscript_assets <- ifelse(
    out$final_label %in% c("supplement-only", "removed from main-text shortlist"),
    "data/analysis_ready/cluster_pathways/12_signal_validation/",
    ""
  )
  out
}
