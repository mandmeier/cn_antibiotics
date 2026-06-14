# Pathway discovery and filtering (no composite scoring).

source("R/utils/cluster_pathways_constants.R")
source("R/utils/cluster_pathways_stats.R")
source("R/utils/cluster_pathways_endpoints.R")

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

pick_best_driver <- function(frame, driver_by_prov, driver_cols, env_amr_rho) {
  best <- list(
    driver_metric = "",
    driver_env_rho = NA_real_,
    driver_env_p = NA_real_,
    driver_amr_rho = NA_real_,
    driver_amr_p = NA_real_,
    coherent_direction = FALSE
  )
  if (!length(driver_cols)) return(best)

  driver_sub <- driver_by_prov[frame$province, driver_cols, drop = FALSE]
  de <- fast_spearman_corrwith(driver_sub, frame$env)
  da <- fast_spearman_corrwith(driver_sub, frame$amr)
  score_rows <- lapply(driver_cols, function(driver) {
    de_rho <- unname(de[[driver]])
    da_rho <- unname(da[[driver]])
    list(
      selection_value = abs(ifelse(is.finite(de_rho), de_rho, 0)) +
        abs(ifelse(is.finite(da_rho), da_rho, 0)),
      driver = driver,
      de_rho = de_rho,
      da_rho = da_rho,
      coherent = isTRUE(sign(de_rho) * sign(env_amr_rho) == sign(da_rho) && sign(da_rho) != 0)
    )
  })
  score_rows <- score_rows[order(
    vapply(score_rows, `[[`, numeric(1), "selection_value"),
    decreasing = TRUE
  )]
  if (!length(score_rows)) return(best)

  pick <- score_rows[[1]]
  d <- driver_by_prov[frame$province, pick$driver, drop = TRUE]
  list(
    driver_metric = pick$driver,
    driver_env_rho = pick$de_rho,
    driver_env_p = safe_spearman(d, frame$env)$p,
    driver_amr_rho = pick$da_rho,
    driver_amr_p = safe_spearman(d, frame$amr)$p,
    coherent_direction = pick$coherent
  )
}

sort_discovered_pathways <- function(frame) {
  if (is.null(frame) || !nrow(frame)) return(frame)
  ord <- order(
    -abs(frame$env_amr_rho),
    -frame$n_provinces,
    frame$env_amr_p,
    na.last = TRUE
  )
  frame[ord, , drop = FALSE]
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
      candidate_drivers <- if (nrow(frame) >= 8) driver_cols else CP_DRIVER_COMPOSITES
      best <- pick_best_driver(frame, driver_by_prov, candidate_drivers, rho)

      rows[[length(rows) + 1]] <- data.frame(
        scenario = scenario,
        matrix = matrix,
        env_class = env_class,
        endpoint = endpoint,
        driver_metric = best$driver_metric,
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
        driver_env_rho = best$driver_env_rho,
        driver_env_p = best$driver_env_p,
        driver_amr_rho = best$driver_amr_rho,
        driver_amr_p = best$driver_amr_p,
        coherent_direction = best$coherent_direction,
        loo_sign_stability = loo$sign_share,
        loo_influence_ratio = loo$influence,
        loo_most_influential_province = loo$province,
        stringsAsFactors = FALSE
      )
    }
  }

  out <- if (length(rows)) do.call(rbind, rows) else data.frame()
  if (!nrow(out)) return(out)
  out$env_amr_q_fdr <- bh_fdr(out$env_amr_p)
  out$partial_env_amr_q_fdr <- bh_fdr(out$partial_env_amr_p)
  sort_discovered_pathways(out)
}

filter_class_concordant <- function(pathways) {
  if (is.null(pathways) || !nrow(pathways)) return(pathways)
  keep <- vapply(
    seq_len(nrow(pathways)),
    function(i) endpoint_matches_env_class(pathways$endpoint[i], pathways$env_class[i]),
    logical(1)
  )
  pathways[keep, , drop = FALSE]
}

filter_specific_env_classes <- function(pathways,
                                        excluded_classes = CP_EXCLUDED_PATHWAY_ENV_CLASSES) {
  if (is.null(pathways) || !nrow(pathways)) return(pathways)
  excluded <- tolower(excluded_classes)
  keep <- !tolower(env_class_norm(pathways$env_class)) %in% excluded
  pathways[keep, , drop = FALSE]
}

filter_pathway_matrices <- function(pathways,
                                    keep_matrices = CP_PATHWAY_FILTER_MATRICES) {
  if (is.null(pathways) || !nrow(pathways)) return(pathways)
  pathways[pathways$matrix %in% keep_matrices, , drop = FALSE]
}

pathway_loo_label <- function(loo_sign_stability,
                              stable_threshold = CP_MIN_LOO_SIGN_STABILITY) {
  vapply(loo_sign_stability, function(x) {
    if (!is.finite(x)) return("missing")
    if (x >= stable_threshold) "stable" else "unstable"
  }, character(1), USE.NAMES = FALSE)
}

pathway_loo_rank <- function(loo_label) {
  order <- c("stable" = 0L, "unstable" = 1L, "missing" = 2L)
  unname(order[loo_label])
}

rank_pathways_simple_figure3 <- function(pathways) {
  if (is.null(pathways) || !nrow(pathways)) return(pathways)
  pathways %>%
    dplyr::mutate(
      abs_env_amr_rho = abs(.data$env_amr_rho),
      abs_partial_env_amr_rho = abs(.data$partial_env_amr_rho),
      loo_label = pathway_loo_label(.data$loo_sign_stability),
      loo_rank = pathway_loo_rank(.data$loo_label)
    ) %>%
    dplyr::arrange(
      .data$matrix,
      dplyr::desc(.data$n_provinces),
      dplyr::desc(.data$abs_env_amr_rho),
      dplyr::desc(.data$abs_partial_env_amr_rho),
      .data$loo_rank,
      .data$env_amr_p
    )
}

apply_pathway_quality_floors <- function(pathways,
                                         min_provinces = CP_MIN_PROVINCES) {
  if (is.null(pathways) || !nrow(pathways)) return(pathways)
  dplyr::filter(pathways, .data$n_provinces >= min_provinces)
}

select_top_pathways_per_matrix <- function(pathways,
                                           n = CP_SIMPLE_FIG3_ROWS_PER_MATRIX) {
  if (is.null(pathways) || !nrow(pathways)) return(pathways)
  rank_pathways_simple_figure3(pathways) %>%
    dplyr::group_by(.data$matrix) %>%
    dplyr::slice_head(n = n) %>%
    dplyr::ungroup() %>%
    dplyr::mutate(
      matrix_rank = dplyr::row_number(),
      candidate_id = paste0(.data$matrix, "::", .data$matrix_rank),
      .by = "matrix"
    )
}

apply_pathway_base_filters <- function(pathways) {
  if (is.null(pathways) || !nrow(pathways)) return(pathways)
  pathways %>%
    filter_specific_env_classes() %>%
    filter_pathway_matrices() %>%
    filter_class_concordant()
}

build_filter_pool <- function(pathways) {
  if (is.null(pathways) || !nrow(pathways)) return(pathways)
  apply_pathway_base_filters(pathways) %>%
    apply_pathway_quality_floors()
}

apply_pathway_filters <- function(pathways) {
  if (is.null(pathways) || !nrow(pathways)) return(pathways)
  pool <- build_filter_pool(pathways)
  # Final shortlist: top N per matrix (disabled — show full filter pool in figure).
  # pool %>%
  #   select_top_pathways_per_matrix()
  rank_pathways_simple_figure3(pool)
}

build_filter_summary <- function(pathways) {
  if (is.null(pathways) || !nrow(pathways)) {
    return(data.frame(
      matrix = character(),
      stage = character(),
      n_surviving = integer(),
      stringsAsFactors = FALSE
    ))
  }

  stages <- list(
    all_candidates = function(df) df,
    exclude_other_multiple_classes = function(df) filter_specific_env_classes(df),
    figure_matrices_only = function(df) {
      filter_pathway_matrices(filter_specific_env_classes(df))
    },
    class_concordant_only = function(df) apply_pathway_base_filters(df),
    min_provinces = function(df) build_filter_pool(df),
    top_n_per_matrix = function(df) apply_pathway_filters(df)
  )

  rows <- list()
  for (matrix_name in sort(unique(pathways$matrix))) {
    sub <- pathways[pathways$matrix == matrix_name, , drop = FALSE]
    for (stage_name in names(stages)) {
      survivors <- stages[[stage_name]](sub)
      rows[[length(rows) + 1]] <- data.frame(
        matrix = matrix_name,
        stage = stage_name,
        n_surviving = nrow(survivors),
        stringsAsFactors = FALSE
      )
    }
  }
  do.call(rbind, rows)
}
