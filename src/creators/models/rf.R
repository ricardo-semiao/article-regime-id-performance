
# Setup ------------------------------------------------------------------------

box::use(
  src/utils[...],
  ../metrics[series_sd],
  src/parameters[n_t, n_b, n_h]
)

# * Common docs for all models in in './__init__.R'



# Creator ----------------------------------------------------------------------

#' Creator - Model: Random forest
#'
#' @param n_r [`integer(1)`] Number of regimes.
#' @param n_l [`integer(1)`] Number of lags.
#' @param n_l_r [`integer(1)`] Number of lagged regimes.
#' @param w_size [`integer(1)`] Window size for metrics.
#' @param w_metrics [`list()`] List of window metrics.
#' @param mtry [`integer(1)`] Number of variables randomly sampled as candidates.
#' @param nodesize [`integer(1)`] Minimum size of terminal nodes.
#' @param maxnodes [`integer(1)`] Maximum number of terminal nodes.
#' @param sampsize [`integer(1)`] Size of sample for training.
#' @param ntree [`integer(1)`] Number of trees to grow.
#'
#' @returns [`function(data, n_t, n_b, n_h, rn_par)`] Function to fit the model.
#' @export
rf <- function(
  n_r = 1, n_l = 1, n_l_r = 1, w_size = 5, w_metrics = NULL,
  mtry = NULL, nodesize = 5, maxnodes = NULL, sampsize = NULL, ntree = 500
) {
  mtry <- mtry %||% max(floor((n_l_r + length(w_metrics)) / 3), 1)
  w_metrics <- w_metrics %||% list(avg = mean, acf = acor, sd = bare_sd)
  hyperparameters <- as.list(current_env())

  f <- function(data, n_t, n_b, n_h, rn_par) {
    model.frame <- stats::model.frame # Bug correction
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
    sampsize <- sampsize %||% (n_t - n_h - n_cut)
    idx_fit <- (n_cut + 1):(n_t - n_h)

    mod <- randomForest::randomForest(
      # Data:
      y = data[idx_fit, "y"], x = data[idx_fit, -1],
      # Hyperparameters: replace
      mtry = mtry, nodesize = nodesize, maxnodes = maxnodes, sampsize = sampsize,
      # Optimization: corr.bias
      ntree = ntree
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
    rows = "R1",
    cols = c("mu", paste0("rho", 1:n_l), "sigma")
  )

  y <- c(
    rep(NA_real_, n_cut),
    mod$predicted,
    stats::predict(mod, newdata = as.data.frame(data[(n_t - n_h + 1):n_t, ]))
  ) # TODO: reimplement natively
  r <- c(rep(NA_integer_, n_cut), rep(1, n_t - n_cut))

  meta <- list(
    coefs = `dimnames<-`(matrix(NA_real_, 1, 2 + n_l), dims),
    importance = mod$importance
  )

  list(y = unname(y), r = r, meta = meta)
}
fn_env(get_results) <- new_environment(
  list(series_sd = series_sd, regimes_order = regimes_order),
  pkg_env("base")
)
