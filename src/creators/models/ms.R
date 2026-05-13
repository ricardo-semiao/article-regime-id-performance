
# Setup ------------------------------------------------------------------------

box::use(
  src/utils[...],
  ../metrics[series_sd]
)

# * Common docs for all models in in './__init__.R'



# Creator ----------------------------------------------------------------------

#' Model: Markov switching
#'
#' Comments on parameters:
#' - All coefficients switch between regimes, but not sigma
#'
#' @export
ms <- function(
  n_r = 2, n_l = 1, gamma = NULL,
  min_r_size = 0.1,
  tol = 1e-5, max_iter = 10
) {
  hyperparameters <- as.list(current_env())

  f <- function(data, n_t, n_b, n_h, rn_par) {
    mod <- MSwM::msmFit(
      # Data:
      y ~ 1, k = n_r, p = n_l, data = data[(n_b + 1):(n_t - n_h), ],
      # Optimization:
      control = list(maxiter = max_iter, tol = tol, parallelization = FALSE),
      # Others:
      sw = c(rep(TRUE, n_l + 1), FALSE)
    )
    get_results(data, mod, n_t, n_b, n_h, n_r, n_l, rn_par)
  }

  fn_env(f) <- new_environment(
    c(hyperparameters, get_results = get_results),
    pkg_env("base")
  )

  f
}

get_results <- function(data, mod, n_t, n_b, n_h, n_r, n_l, rn_par) {
  dims <- list(
    rows = paste0("R", 1:n_r),
    cols = c("mu", paste0("rho", 1:n_l), "sigma")
  )
  cols <- paste0("y_l", 1:n_l)

  coefs <- as.matrix(mod@Coef)

  # Regimes:
  r <- matrix(NA_real_, n_t, n_r)
  r[(n_b + 1 + n_l):(n_t - n_h), ] <- mod@Fit@filtProb
  for (i in (n_h - 1):0) {
    r[n_t - i, ] <- mod@transMat %*% r[n_t - i - 1, ]
  }
  r_cat <- max.col(r, ties.method = "first")

  # Series:
  y <- c(rep(NA_real_, n_b + n_l), mod@model$fitted.values, rep(NA_real_, n_h))
  for (i in (n_h - 1):0) {
    y[n_t - i] <- sum(coefs %*% c(1, data[n_t - i, cols]) * r[n_t - i, ])
  }
  # TODO: do via simulation

  # Meta information:
  idx_fit <- (n_b + 1):(n_t - n_h)
  coefs <- cbind(coefs, series_sd(data[idx_fit, "y"] - y[idx_fit], r_cat, n_r, na.rm = TRUE))
  ord <- regimes_order(coefs, rn_par, dims)

  meta <- list(
    coefs = `dimnames<-`(coefs[ord, ], dims),
    switches = mod@transMat
  )

  list(y = unname(y), r = ord[r_cat], meta = meta)
}
fn_env(get_results) <- new_environment(
  list(series_sd = series_sd, regimes_order = regimes_order),
  pkg_env("base")
)
