# Statistical helpers for cluster-pathways analysis.

bh_fdr <- function(values) {
  p <- suppressWarnings(as.numeric(values))
  out <- rep(NA_real_, length(p))
  valid_idx <- which(is.finite(p))
  if (!length(valid_idx)) return(out)
  out[valid_idx] <- stats::p.adjust(p[valid_idx], method = "BH")
  out
}

holm_adjust <- function(p_values) {
  p <- suppressWarnings(as.numeric(p_values))
  out <- rep(NA_real_, length(p))
  valid_idx <- which(is.finite(p))
  if (!length(valid_idx)) return(out)
  out[valid_idx] <- stats::p.adjust(p[valid_idx], method = "holm")
  out
}

iqr_num <- function(values) {
  vals <- suppressWarnings(as.numeric(values))
  vals <- vals[is.finite(vals)]
  if (!length(vals)) return(NA_real_)
  stats::IQR(vals)
}

rank_standardize <- function(frame) {
  ranked <- as.data.frame(lapply(frame, function(col) {
    rank(col, na.last = "keep", ties.method = "average") / sum(!is.na(col))
  }))
  filled <- ranked
  for (nm in names(filled)) {
    med <- stats::median(filled[[nm]], na.rm = TRUE)
    if (!is.finite(med)) med <- 0.5
    filled[[nm]][is.na(filled[[nm]])] <- med
  }
  as.data.frame(scale(filled))
}

build_composite_index <- function(wide_df, component_metrics, index_name) {
  available <- intersect(component_metrics, names(wide_df))
  if (!length(available)) {
    wide_df[[index_name]] <- NA_real_
    return(wide_df)
  }
  comp <- wide_df[, available, drop = FALSE]
  z <- rank_standardize(comp)
  wide_df[[index_name]] <- rowMeans(z, na.rm = TRUE)
  wide_df
}

safe_spearman <- function(x, y) {
  frame <- data.frame(
    x = suppressWarnings(as.numeric(x)),
    y = suppressWarnings(as.numeric(y))
  )
  frame <- frame[is.finite(frame$x) & is.finite(frame$y), , drop = FALSE]
  if (nrow(frame) < 5 ||
      length(unique(frame$x)) < 2 ||
      length(unique(frame$y)) < 2) {
    return(list(rho = NA_real_, p = NA_real_))
  }
  test <- suppressWarnings(
    stats::cor.test(frame$x, frame$y, method = "spearman", exact = FALSE)
  )
  list(rho = unname(test$estimate), p = test$p.value)
}

partial_spearman <- function(x, y, controls) {
  frame <- cbind(
    data.frame(
      x = suppressWarnings(as.numeric(x)),
      y = suppressWarnings(as.numeric(y))
    ),
    as.data.frame(controls)
  )
  keep <- apply(frame, 1, function(r) all(is.finite(suppressWarnings(as.numeric(r)))))
  frame <- frame[keep, , drop = FALSE]
  if (nrow(frame) < 8 ||
      length(unique(frame$x)) < 2 ||
      length(unique(frame$y)) < 2) {
    return(list(rho = NA_real_, p = NA_real_))
  }
  ranked <- as.data.frame(lapply(frame, function(col) {
    rank(as.numeric(col), ties.method = "average")
  }))
  z <- as.matrix(cbind(1, ranked[, setdiff(names(ranked), c("x", "y")), drop = FALSE]))
  xr <- ranked$x
  yr <- ranked$y
  x_fit <- stats::lm.fit(z, xr)
  y_fit <- stats::lm.fit(z, yr)
  x_res <- xr - z %*% x_fit$coefficients
  y_res <- yr - z %*% y_fit$coefficients
  test <- suppressWarnings(
    stats::cor.test(as.numeric(x_res), as.numeric(y_res), method = "spearman", exact = FALSE)
  )
  list(rho = unname(test$estimate), p = test$p.value)
}

fast_spearman_corrwith <- function(frame, target) {
  out <- rep(NA_real_, ncol(frame))
  names(out) <- colnames(frame)
  y <- suppressWarnings(as.numeric(target))
  y_rank <- rank(y, na.last = "keep", ties.method = "average")
  for (col in colnames(frame)) {
    x <- suppressWarnings(as.numeric(frame[[col]]))
    mask <- is.finite(x) & is.finite(y_rank)
    if (sum(mask) < 5 ||
        length(unique(x[mask])) < 2 ||
        length(unique(y[mask])) < 2) next
    xr <- rank(x[mask], ties.method = "average")
    yr <- y_rank[mask]
    xr <- xr - mean(xr)
    yr <- yr - mean(yr)
    denom <- sqrt(sum(xr * xr) * sum(yr * yr))
    out[[col]] <- if (denom > 0) sum(xr * yr) / denom else NA_real_
  }
  out
}

ridge_fit_one <- function(x, y, lambda) {
  xtx <- crossprod(x)
  beta <- tryCatch(
    solve(xtx + diag(lambda, ncol(x)), crossprod(x, y)),
    error = function(e) rep(NA_real_, ncol(x))
  )
  as.numeric(beta)
}

select_ridge_lambda <- function(x, y, lambdas) {
  best_lambda <- lambdas[1]
  best_mse <- Inf
  n <- nrow(x)
  for (lambda in lambdas) {
    preds <- rep(NA_real_, n)
    for (i in seq_len(n)) {
      train_idx <- setdiff(seq_len(n), i)
      beta <- ridge_fit_one(x[train_idx, , drop = FALSE], y[train_idx], lambda)
      if (all(is.finite(beta))) preds[i] <- sum(x[i, ] * beta)
    }
    ok <- is.finite(preds) & is.finite(y)
    if (!any(ok)) next
    mse <- mean((y[ok] - preds[ok])^2)
    if (is.finite(mse) && mse < best_mse) {
      best_mse <- mse
      best_lambda <- lambda
    }
  }
  best_lambda
}
