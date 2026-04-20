ensure_packages <- function(packages) {
  missing <- packages[!vapply(packages, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop(
      sprintf(
        "Missing R packages: %s\nInstall them before running these scripts.",
        paste(missing, collapse = ", ")
      ),
      call. = FALSE
    )
  }
}

parse_cli_args <- function(defaults) {
  args <- commandArgs(trailingOnly = TRUE)
  result <- defaults
  if (length(args) == 0) {
    return(result)
  }

  index <- 1L
  while (index <= length(args)) {
    key <- args[[index]]
    if (!startsWith(key, "--")) {
      stop(sprintf("Unexpected argument: %s", key), call. = FALSE)
    }
    name <- substring(key, 3L)
    if (!name %in% names(defaults)) {
      stop(sprintf("Unknown option: %s", key), call. = FALSE)
    }
    if (index == length(args)) {
      stop(sprintf("Missing value for option: %s", key), call. = FALSE)
    }
    value <- args[[index + 1L]]
    template <- defaults[[name]]
    if (is.numeric(template)) {
      result[[name]] <- as.numeric(value)
    } else if (is.integer(template)) {
      result[[name]] <- as.integer(value)
    } else if (is.logical(template)) {
      result[[name]] <- tolower(value) %in% c("1", "true", "yes")
    } else {
      result[[name]] <- value
    }
    index <- index + 2L
  }
  result
}

ensure_dir <- function(path) {
  dir.create(path, recursive = TRUE, showWarnings = FALSE)
  normalizePath(path, winslash = "/", mustWork = FALSE)
}

read_csv_na <- function(path) {
  utils::read.csv(path, stringsAsFactors = FALSE, check.names = FALSE, na.strings = c("", "NA"))
}

write_csv_safe <- function(frame, path, row.names = FALSE) {
  utils::write.csv(frame, path, row.names = row.names, na = "")
}

median_impute_matrix <- function(frame) {
  matrix <- as.matrix(frame)
  for (column in seq_len(ncol(matrix))) {
    values <- matrix[, column]
    med <- stats::median(values, na.rm = TRUE)
    if (is.na(med)) {
      med <- 0
    }
    values[is.na(values)] <- med
    matrix[, column] <- values
  }
  matrix
}

scale_matrix_safe <- function(matrix) {
  scaled <- scale(matrix)
  scaled[, apply(scaled, 2, function(column) all(is.na(column)))] <- 0
  scaled[is.na(scaled)] <- 0
  scaled
}

zscore_df <- function(frame) {
  out <- frame
  for (column in names(out)) {
    values <- suppressWarnings(as.numeric(out[[column]]))
    if (all(is.na(values))) {
      out[[column]] <- values
      next
    }
    sd_value <- stats::sd(values, na.rm = TRUE)
    if (is.na(sd_value) || sd_value == 0) {
      out[[column]] <- values - mean(values, na.rm = TRUE)
    } else {
      out[[column]] <- (values - mean(values, na.rm = TRUE)) / sd_value
    }
  }
  out
}

bh_by_group <- function(frame, p_col, group_col, out_col) {
  frame[[out_col]] <- NA_real_
  groups <- unique(frame[[group_col]])
  for (group in groups) {
    index <- which(frame[[group_col]] == group)
    frame[[out_col]][index] <- stats::p.adjust(frame[[p_col]][index], method = "BH")
  }
  frame
}

feature_layer_novelty <- function(feature, yearbook_features) {
  if (feature %in% yearbook_features) {
    return("yearbook")
  }
  if (feature %in% c("overall_resistance")) {
    return("resistance")
  }
  if (startsWith(feature, "resclass_")) {
    return("resistance")
  }
  if (feature %in% c("A. baumannii", "E. coli", "K. pneumoniae", "P. aeruginosa", "S. aureus")) {
    return("resistance")
  }
  if (startsWith(feature, "envclass_") || startsWith(feature, "sample_") || startsWith(feature, "env_log_")) {
    return("environment")
  }
  if (grepl("__", feature, fixed = TRUE)) {
    return("environment")
  }
  "other"
}

save_png <- function(path, width = 9, height = 6, expr) {
  grDevices::png(path, width = width, height = height, units = "in", res = 300)
  on.exit(grDevices::dev.off(), add = TRUE)
  force(expr)
}