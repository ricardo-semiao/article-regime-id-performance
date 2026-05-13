
# Setup ------------------------------------------------------------------------

box::use(
  src/utils[...],
  ../metrics[series_sd]
)

# * Common docs for all models in in './__init__.R'"



# Creator ----------------------------------------------------------------------

#' Model: K-means
#' @export
km <- function(
  n_r, n_l = 1, n_l_r = 1, w_size = 5, w_metrics = NULL
) {
  if (n_l != 1) cli_abort("Only n_l = 1 is currently supported.")
  # TODO: generalize to n_l > 1

  w_metrics <- w_metrics %||% list(avg = mean, acf = acor, sd = stats::sd)
  hyperparameters <- as.list(current_env())

  f <- function(data, n_t, n_b, n_h, rn_par) {
    data <- cbind(data, lapply(w_metrics, \(x) rep(NA_real_, n_t)))

    for (t in 1:n_t) {
      w_data <- data[max((t - w_size), 1):t, "y_l1"]
      for (metric in names(w_metrics)) {
        data[t, metric] <- w_metrics[[metric]](w_data)
      }
    }

    w_nas <- names(w_metrics) |>
      vapply(\(col) which(is.finite(data[, col]))[1] - 1L, integer(1)) |>
      max()
    n_cut <- max(w_nas, n_b, n_l_r)

    mod <- stats::kmeans(
      # Data:
      data[(n_cut + 1):(n_t - n_h), -1],
      # Hyperparameters:
      centers = n_r,
      # Optimization: iter.max, nstart, algorithm
    )
    get_results(data, mod, n_t, n_b, n_h, n_r, n_l, rn_par, n_cut = n_cut)
  }

  fn_env(f) <- new_environment(
    c(hyperparameters, get_results = get_results),
    pkg_env("base")
  )

  f
}


get_results <- function(data, mod, n_t, n_b, n_h, n_r, n_l, rn_par, n_cut) {
  dims <- list(
    rows = paste0("R", 1:n_r),
    cols = c("mu", paste0("rho", 1:n_l), "sigma")
  )

  # Regimes:
  r <- c(rep(NA_integer_, n_cut), mod$cluster, rep(NA_integer_, n_h))
  for (i in (n_h - 1):0) {
    w_idx <- n_t - i
    r[n_t - i] <- which.min(apply(mod$centers, 1, \(c) sum((c - data[w_idx, ])^2)))
  }

  # Series:
  idx_fit <- (n_cut + 1):(n_t - n_h)
  idx_pred <- (n_t - n_h + 1):n_t

  ar <- stats::lm(
    y ~ 0 + r + y_l1:r - 1,
    data = cbind(data[idx_fit, ], r = factor(r[idx_fit], 1:n_r))
  )
  y <- c(
    rep(NA_real_, n_cut),
    stats::fitted(ar),
    stats::predict(ar, cbind(data[idx_pred, ], r = factor(r[idx_pred], 1:n_r)))
  ) # TODO: reimplement natively

  # Meta information:
  coefs <- c(
    ar$coefficients,
    series_sd(data[idx_fit, "y"] - y[idx_fit], r[idx_fit], n_r)
  ) |>
    matrix(n_r, 3)
  ord <- regimes_order(coefs, rn_par, dims)

  meta <- list(
    coefs = `dimnames<-`(coefs[ord, ], dims),
    switches = mod$centers
  )

  list(y = unname(y), r = ord[r], meta = meta)
}
fn_env(get_results) <- new_environment(
  list(series_sd = series_sd, regimes_order = regimes_order),
  pkg_env("base")
)
