# Step 02: Kruskal-Wallis screening of 2024 yearbook metrics by cluster.

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tidyr)
})

source("R/utils/cluster_pathways_constants.R")
source("R/utils/cluster_pathways_stats.R")

out_dir <- cp_step_dir("02_yearbook_cluster_screen")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

groups <- cp_read_csv(file.path(cp_step_dir("01_cluster_provenance"), "province_groups_with_clusters.csv"))
yb <- cp_read_csv("data/cleaned/yearbook_province_year_panel.csv") %>%
  filter(.data$province %in% CP_PROVINCES) %>%
  mutate(value = suppressWarnings(as.numeric(.data$value)))

y24 <- yb %>%
  filter(.data$year == 2024) %>%
  inner_join(groups %>% select("province", "cluster_id", "cluster_name"), by = "province")

metrics <- sort(unique(y24$metric[!is.na(y24$metric)]))

screen_rows <- list()
pair_rows <- list()

for (metric in metrics) {
  sub <- y24 %>% filter(.data$metric == !!metric, is.finite(.data$value))
  cluster_ids <- sort(unique(sub$cluster_id))
  cluster_values <- lapply(cluster_ids, function(cid) sub$value[sub$cluster_id == cid])
  cluster_values_test <- cluster_values[vapply(cluster_values, length, integer(1)) >= 2]
  h <- p <- eps <- NA_real_
  if (length(cluster_values_test) >= 2 &&
      sum(vapply(cluster_values_test, length, integer(1))) > length(cluster_values_test)) {
    kw <- tryCatch(stats::kruskal.test(cluster_values_test), error = function(e) NULL)
    if (!is.null(kw)) {
      h <- unname(as.numeric(kw$statistic))
      p <- kw$p.value
      n <- sum(vapply(cluster_values_test, length, integer(1)))
      k <- length(cluster_values_test)
      eps <- if (n > k) max(0, (h - k + 1) / (n - k)) else NA_real_
    }
  }

  med <- tapply(sub$value, sub$cluster_id, stats::median, na.rm = TRUE)
  iqrs <- tapply(sub$value, sub$cluster_id, iqr_num)
  top_cluster <- if (length(med)) as.integer(names(med)[which.max(med)]) else NA_integer_
  bottom_cluster <- if (length(med)) as.integer(names(med)[which.min(med)]) else NA_integer_
  unit_txt <- if ("unit" %in% names(sub)) {
    paste(sort(unique(sub$unit[!is.na(sub$unit) & nzchar(as.character(sub$unit))])), collapse = "; ")
  } else {
    ""
  }

  screen_rows[[length(screen_rows) + 1]] <- data.frame(
    metric = metric,
    unit = unit_txt,
    n_provinces = length(unique(sub$province)),
    n_clusters_observed = length(unique(sub$cluster_id)),
    kruskal_h = h,
    p_value = p,
    epsilon_squared = eps,
    top_cluster_id = top_cluster,
    top_cluster_name = if (!is.na(top_cluster)) unname(CP_CLUSTER_NAMES[as.character(top_cluster)]) else "",
    bottom_cluster_id = bottom_cluster,
    bottom_cluster_name = if (!is.na(bottom_cluster)) unname(CP_CLUSTER_NAMES[as.character(bottom_cluster)]) else "",
    median_cluster_1 = if ("1" %in% names(med)) med[["1"]] else NA_real_,
    median_cluster_2 = if ("2" %in% names(med)) med[["2"]] else NA_real_,
    median_cluster_3 = if ("3" %in% names(med)) med[["3"]] else NA_real_,
    iqr_cluster_1 = if ("1" %in% names(iqrs)) iqrs[["1"]] else NA_real_,
    iqr_cluster_2 = if ("2" %in% names(iqrs)) iqrs[["2"]] else NA_real_,
    iqr_cluster_3 = if ("3" %in% names(iqrs)) iqrs[["3"]] else NA_real_,
    coverage_missing_provinces = 31L - length(unique(sub$province)),
    stringsAsFactors = FALSE
  )

  if (length(cluster_ids) >= 2) {
    cmb <- utils::combn(cluster_ids, 2, simplify = FALSE)
    pair_p <- rep(NA_real_, length(cmb))
    pair_meta <- vector("list", length(cmb))
    for (i in seq_along(cmb)) {
      left <- cmb[[i]][1]
      right <- cmb[[i]][2]
      lv <- sub$value[sub$cluster_id == left]
      rv <- sub$value[sub$cluster_id == right]
      u <- pv <- NA_real_
      if (length(lv) >= 2 && length(rv) >= 2) {
        wt <- tryCatch(
          stats::wilcox.test(lv, rv, alternative = "two.sided", exact = FALSE),
          error = function(e) NULL
        )
        if (!is.null(wt)) {
          u <- unname(as.numeric(wt$statistic))
          pv <- wt$p.value
        }
      }
      pair_p[i] <- pv
      pair_meta[[i]] <- list(
        left = left, right = right, u = u, pv = pv,
        lmed = if (length(lv)) stats::median(lv, na.rm = TRUE) else NA_real_,
        rmed = if (length(rv)) stats::median(rv, na.rm = TRUE) else NA_real_
      )
    }
    adj <- holm_adjust(pair_p)
    for (i in seq_along(pair_meta)) {
      meta <- pair_meta[[i]]
      higher_cluster <- if (is.finite(meta$lmed) && is.finite(meta$rmed)) {
        if (meta$lmed >= meta$rmed) meta$left else meta$right
      } else {
        NA_integer_
      }
      pair_rows[[length(pair_rows) + 1]] <- data.frame(
        metric = metric,
        cluster_left_id = as.integer(meta$left),
        cluster_right_id = as.integer(meta$right),
        cluster_left_name = unname(CP_CLUSTER_NAMES[as.character(meta$left)]),
        cluster_right_name = unname(CP_CLUSTER_NAMES[as.character(meta$right)]),
        u_stat = meta$u,
        p_value = meta$pv,
        holm_p_value = adj[i],
        median_left = meta$lmed,
        median_right = meta$rmed,
        higher_median_cluster_id = higher_cluster,
        stringsAsFactors = FALSE
      )
    }
  }
}

screen <- if (length(screen_rows)) bind_rows(screen_rows) else tibble()
pairwise <- if (length(pair_rows)) bind_rows(pair_rows) else tibble()

if (nrow(screen)) {
  screen <- screen %>%
    mutate(
      fdr_bh_q_value = bh_fdr(.data$p_value),
      significant_q_0_10 = is.finite(.data$fdr_bh_q_value) & .data$fdr_bh_q_value < 0.10
    ) %>%
    arrange(
      ifelse(is.na(.data$fdr_bh_q_value), Inf, .data$fdr_bh_q_value),
      ifelse(is.na(.data$p_value), Inf, .data$p_value),
      .data$metric
    )
}

cp_write_csv(screen, file.path(out_dir, "yearbook_2024_cluster_discriminators.csv"))
cp_write_csv(pairwise, file.path(out_dir, "yearbook_2024_pairwise_holm.csv"))

message("Wrote yearbook cluster screen to ", out_dir)
