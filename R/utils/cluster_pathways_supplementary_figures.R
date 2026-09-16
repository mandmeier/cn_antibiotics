# Supplementary sensitivity bar charts for pathway filter calibration.

source("R/utils/cluster_pathways_constants.R")
source("R/utils/cluster_pathways_discovery.R")

count_rho_survivors <- function(sub, min_rho, min_loo = CP_MIN_LOO_SIGN_STABILITY) {
  if (length(min_rho) > 1L && length(min_loo) == 1L) {
    min_loo <- rep(min_loo, length(min_rho))
  }
  vapply(
    seq_along(min_rho),
    function(i) {
      sum(
        is.finite(sub$n_provinces) & sub$n_provinces >= CP_MIN_PROVINCES &
          is.finite(sub$env_amr_rho) & abs(sub$env_amr_rho) >= min_rho[[i]] &
          is.finite(sub$loo_sign_stability) & sub$loo_sign_stability >= min_loo[[i]],
        na.rm = TRUE
      )
    },
    integer(1)
  )
}

count_p_survivors <- function(sub, max_p, min_rho = 0) {
  vapply(
    max_p,
    function(threshold) {
      sum(
        is.finite(sub$n_provinces) & sub$n_provinces >= CP_MIN_PROVINCES &
          is.finite(sub$env_amr_rho) & abs(sub$env_amr_rho) >= min_rho &
          is.finite(sub$loo_sign_stability) & sub$loo_sign_stability >= CP_MIN_LOO_SIGN_STABILITY &
          is.finite(sub$env_amr_p) & sub$env_amr_p <= threshold,
        na.rm = TRUE
      )
    },
    integer(1)
  )
}

save_supplementary_plot <- function(path, plot, width, height) {
  if (grepl("\\.svg$", path, ignore.case = TRUE)) {
    if (!requireNamespace("svglite", quietly = TRUE)) {
      message("Skipping SVG export for ", basename(path), ": install the svglite package")
      return(invisible(NULL))
    }
    ggplot2::ggsave(
      path,
      plot,
      width = width,
      height = height,
      device = svglite::svglite,
      bg = "white"
    )
    return(invisible(NULL))
  }
  ggplot2::ggsave(
    path,
    plot,
    width = width,
    height = height,
    dpi = 320,
    bg = "white"
  )
}

write_pathway_supplementary_figures <- function(pathways, out_dir) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

  scored <- build_filter_pool(pathways) %>%
    dplyr::mutate(abs_env_amr_rho = abs(.data$env_amr_rho))
  scored_12 <- scored

  min_abs_rho_grid <- seq(0.00, 0.80, by = 0.05)
  matrix_pool_sizes <- scored_12 %>%
    dplyr::count(.data$matrix, name = "n_pool") %>%
    dplyr::arrange(dplyr::desc(.data$n_pool))
  matrix_levels <- matrix_pool_sizes$matrix

  rho_sweep_by_matrix <- tidyr::expand_grid(
    matrix = matrix_levels,
    min_abs_env_amr_rho = min_abs_rho_grid
  ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      min_loo_sign_stability = CP_MIN_LOO_SIGN_STABILITY,
      n_surviving = count_rho_survivors(
        scored_12[scored_12$matrix == .data$matrix, , drop = FALSE],
        min_abs_env_amr_rho,
        CP_MIN_LOO_SIGN_STABILITY
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(matrix_pool_sizes, by = "matrix") %>%
    dplyr::mutate(
      matrix = factor(.data$matrix, levels = matrix_levels),
      min_abs_env_amr_rho = factor(
        .data$min_abs_env_amr_rho,
        levels = min_abs_rho_grid,
        labels = sprintf("%.2f", min_abs_rho_grid)
      )
    )

  cp_write_csv(
    rho_sweep_by_matrix %>%
      dplyr::mutate(min_abs_env_amr_rho = as.numeric(as.character(.data$min_abs_env_amr_rho))),
    file.path(out_dir, "loo_fixed_rho_sweep_by_matrix.csv")
  )

  matrix_labeller <- setNames(
    paste0(matrix_pool_sizes$matrix, "\n(n = ", matrix_pool_sizes$n_pool, ")"),
    matrix_pool_sizes$matrix
  )

  p_rho_bars <- ggplot2::ggplot(
    rho_sweep_by_matrix,
    ggplot2::aes(x = .data$min_abs_env_amr_rho, y = .data$n_surviving)
  ) +
    ggplot2::geom_col(fill = "#440154", width = 0.85) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$n_surviving),
      vjust = -0.35,
      size = 2.6,
      colour = "grey20"
    ) +
    ggplot2::facet_wrap(
      ~ matrix,
      ncol = 1,
      labeller = ggplot2::as_labeller(matrix_labeller)
    ) +
    ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 45)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(
      title = paste0(
        "Pathways surviving |rho| thresholds by matrix (LOO >= ",
        CP_MIN_LOO_SIGN_STABILITY, ", ", CP_MIN_PROVINCES, "+ provinces)"
      ),
      subtitle = "Fixed LOO sign-stability cutoff; bars show count with |env–AMR rho| at or above each threshold",
      x = "Minimum |Spearman rho| (env–AMR)",
      y = "Surviving pathways"
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = 10),
      strip.text = ggplot2::element_text(face = "bold")
    )

  save_supplementary_plot(
    file.path(out_dir, "loo_fixed_rho_sweep_by_matrix.png"),
    p_rho_bars,
    width = 10,
    height = 18
  )
  save_supplementary_plot(
    file.path(out_dir, "loo_fixed_rho_sweep_by_matrix.svg"),
    p_rho_bars,
    width = 10,
    height = 18
  )

  scored_rho_loo <- scored_12 %>%
    dplyr::filter(
      is.finite(.data$loo_sign_stability),
      .data$loo_sign_stability >= CP_MIN_LOO_SIGN_STABILITY
    )

  max_p_grid <- c(0.25, 0.2, 0.15, 0.1, 0.05, 0.01, 0.001)
  max_p_labels <- c("0.25", "0.2", "0.15", "0.1", "0.05", "0.01", "0.001")

  matrix_pool_sizes_rho <- scored_rho_loo %>%
    dplyr::count(.data$matrix, name = "n_pool") %>%
    dplyr::arrange(dplyr::desc(.data$n_pool))
  matrix_levels_rho <- matrix_pool_sizes_rho$matrix

  p_sweep_by_matrix <- tidyr::expand_grid(
    matrix = matrix_levels_rho,
    max_env_amr_p = max_p_grid
  ) %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      n_surviving = count_p_survivors(
        scored_rho_loo[scored_rho_loo$matrix == .data$matrix, , drop = FALSE],
        max_env_amr_p
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::left_join(matrix_pool_sizes_rho, by = "matrix") %>%
    dplyr::mutate(
      matrix = factor(.data$matrix, levels = matrix_levels_rho),
      max_env_amr_p = factor(
        .data$max_env_amr_p,
        levels = max_p_grid,
        labels = max_p_labels
      )
    )

  cp_write_csv(
    p_sweep_by_matrix %>%
      dplyr::mutate(max_env_amr_p = as.numeric(as.character(.data$max_env_amr_p))),
    file.path(out_dir, "rho_fixed_p_sweep_by_matrix.csv")
  )

  matrix_labeller_rho <- setNames(
    paste0(matrix_pool_sizes_rho$matrix, "\n(n = ", matrix_pool_sizes_rho$n_pool, ")"),
    matrix_pool_sizes_rho$matrix
  )

  p_p_bars <- ggplot2::ggplot(
    p_sweep_by_matrix,
    ggplot2::aes(x = .data$max_env_amr_p, y = .data$n_surviving)
  ) +
    ggplot2::geom_col(fill = "#31688e", width = 0.85) +
    ggplot2::geom_text(
      ggplot2::aes(label = .data$n_surviving),
      vjust = -0.35,
      size = 2.6,
      colour = "grey20"
    ) +
    ggplot2::facet_wrap(
      ~ matrix,
      ncol = 1,
      labeller = ggplot2::as_labeller(matrix_labeller_rho)
    ) +
    ggplot2::scale_x_discrete(guide = ggplot2::guide_axis(angle = 0)) +
    ggplot2::scale_y_continuous(expand = ggplot2::expansion(mult = c(0, 0.12))) +
    ggplot2::labs(
      title = paste0(
        "Pathways surviving p-value thresholds by matrix (LOO >= ",
        CP_MIN_LOO_SIGN_STABILITY, ", ",
        CP_MIN_PROVINCES, "+ provinces)"
      ),
      subtitle = "Filter pool after class concordance and matrix eligibility; bars show count with env–AMR p at or below each threshold",
      x = "Maximum env–AMR p-value",
      y = "Surviving pathways"
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      plot.title = ggplot2::element_text(face = "bold", size = 10),
      strip.text = ggplot2::element_text(face = "bold")
    )

  save_supplementary_plot(
    file.path(out_dir, "rho_fixed_p_sweep_by_matrix.png"),
    p_p_bars,
    width = 10,
    height = 18
  )
  save_supplementary_plot(
    file.path(out_dir, "rho_fixed_p_sweep_by_matrix.svg"),
    p_p_bars,
    width = 10,
    height = 18
  )

  invisible(out_dir)
}
