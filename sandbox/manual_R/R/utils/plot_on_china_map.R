plot_on_china_map <- function(
    plot_data,
    plot_variable,
    legend_title = NULL,
    na_value = NA,
    border_col = "grey90",
    color_pallette = "D",
    breaks = NULL,
    labels = NULL,
    china_geometry_shapefile_path = "data/raw/cn_shp/cn.shp"
) {

  # append geometry data to plot data



  ## add province geometries from shapefile
  china_prov <- sf::st_read(china_geometry_shapefile_path)

  china_prov_names <- china_prov %>%
    mutate(name = sub(" .*", "", name)) %>%
    rename(province = name) %>%
    select(province, geometry) %>%
    mutate(province = ifelse(province == "Inner", "Inner Mongolia", province)) %>%
    mutate(province = ifelse(province == "Hong", "Hong Kong", province))

  plot_data <- plot_data %>%
    full_join(china_prov_names, by = "province")


  na_col <- "grey90"
  is_numeric <- is.numeric(plot_data[[plot_variable]])

  # -------------------------
  # DEFAULT LEGEND TITLE
  # -------------------------
  if (is.null(legend_title)) {
    legend_title <- plot_variable
  }

  # -------------------------
  # HANDLE CATEGORICAL NA
  # -------------------------
  if (!is_numeric) {

    plot_data[[plot_variable]] <- as.factor(plot_data[[plot_variable]])

    if (!is.na(na_value)) {

      plot_data[[plot_variable]] <- addNA(plot_data[[plot_variable]])

      levels(plot_data[[plot_variable]])[
        is.na(levels(plot_data[[plot_variable]]))
      ] <- na_value

      # move NA label to end
      lvls <- levels(plot_data[[plot_variable]])
      plot_data[[plot_variable]] <- factor(
        plot_data[[plot_variable]],
        levels = c(setdiff(lvls, na_value), na_value)
      )

      # ensure NA appears in breaks if provided
      if (!is.null(breaks) && !(na_value %in% breaks)) {
        breaks <- c(breaks, na_value)
      }
    }
  }

  # -------------------------
  # BASE MAP
  # -------------------------
  p <- ggplot(plot_data) +
    geom_sf(
      aes(fill = .data[[plot_variable]], geometry = geometry),
      color = border_col,
      size = 0.2
    ) +
    theme_minimal() +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      panel.grid = element_blank(),
      legend.position = "bottom"
    )

  # -------------------------
  # DISCRETE
  # -------------------------
  if (!is_numeric) {

    # ---- viridis ----
    if (is.character(color_pallette) && length(color_pallette) == 1) {

      lvls <- levels(plot_data[[plot_variable]])
      cols <- viridisLite::viridis(length(lvls), option = color_pallette)
      names(cols) <- lvls

      if (!is.na(na_value) && na_value %in% lvls) {
        cols[na_value] <- na_col
      }

      scale_args <- list(
        values = cols,
        name = legend_title,
        drop = FALSE
      )

      if (!is.null(breaks)) scale_args$breaks <- breaks
      if (!is.null(labels)) scale_args$labels <- labels

      p <- p + do.call(scale_fill_manual, scale_args)

      # ---- custom ----
    } else if (is.vector(color_pallette) && !is.null(names(color_pallette))) {

      if (!is.na(na_value) && !(na_value %in% names(color_pallette))) {
        color_pallette[na_value] <- na_col
      }

      scale_args <- list(
        values = color_pallette,
        name = legend_title,
        drop = FALSE
      )

      if (!is.null(breaks)) scale_args$breaks <- breaks
      if (!is.null(labels)) scale_args$labels <- labels

      p <- p + do.call(scale_fill_manual, scale_args)

    } else {
      stop("For categorical data, color_pallette must be viridis option or named vector.")
    }

    # -------------------------
    # CONTINUOUS
    # -------------------------
  } else {

    # ---- viridis ----
    if (is.character(color_pallette) && length(color_pallette) == 1) {

      scale_args <- list(
        na.value = na_col,
        option = color_pallette,
        name = legend_title
      )

      if (!is.null(breaks)) scale_args$breaks <- breaks
      if (!is.null(labels)) scale_args$labels <- labels

      p <- p + do.call(scale_fill_viridis_c, scale_args)

      # ---- gradient ----
    } else if (is.vector(color_pallette)) {

      scale_args <- list(
        colours = color_pallette,
        na.value = na_col,
        name = legend_title
      )

      if (!is.null(breaks)) scale_args$breaks <- breaks
      if (!is.null(labels)) scale_args$labels <- labels

      p <- p + do.call(scale_fill_gradientn, scale_args)

    } else {
      stop("For numeric data, color_pallette must be viridis option or a vector of colors.")
    }
  }

  return(p)
}
