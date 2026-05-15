
# Setup ------------------------------------------------------------------------

box::use(
  src/utils[...],
  ../metrics[series_sd]
)

# * Common docs for all models in in './__init__.R'



# Creator ----------------------------------------------------------------------

#' Creator - Model: Threshold
#'
#' - m, ML, MM, MH given by mL etc.; th missing (will be estimated)
#' - mTh, thDelay missing, given by thVar
#'
#' @param n_r [`integer(1)`] Number of regimes.
#' @param n_l [`integer(1)`] Number of lags.
#' @param g [`function(y)`] Transition function. Must be a closure (non-primitive).
#' @param min_r_size [`double(1)`] Minimum regime size as a proportion.
#' @param tol [`double(1)`] Convergence tolerance.
#' @param max_iter [`integer(1)`] Maximum number of iterations.
#'
#' @returns [`function(data, n_t, n_b, n_h, rn_par)`] Function to fit the model.
#' @export
set <- function(
  n_r, n_l = 1, g = \(y) y,
  min_r_size = 0.1,
  tol = 1e-5, max_iter = 10
) {
  fn_env(g) <- pkg_env("base")
  hyperparameters <- as.list(current_env())

  f <- function(data, n_t, n_b, n_h, rn_par) {
    idx_fit <- (n_b + 1):(n_t - n_h)

    mod <- tsDyn::setar(
      # Data:
      data[idx_fit, "y"], mL = n_l, mM = n_l, mH = n_l,
      thVar = g(data[idx_fit, "y_l1"]),
      # Hyperparameters:
      nthresh = n_r - 1,
      # Optimization:
      trim = min_r_size,
      # Others:
      d = 1, steps = 1,
      include = "const", common = "none", model = "TAR", type = "level",
      restriction = "none", trace = FALSE
    )
    get_results(data, mod, n_t, n_b, n_h, n_r, n_l, rn_par, g = g)
  }

  fn_env(f) <- new_environment(
    c(hyperparameters, get_results = get_results),
    pkg_env("base")
  )

  f
}

get_results <- function(data, mod, n_t, n_b, n_h, n_r, n_l, rn_par, g) {
  dims <- list(
    rows = paste0("R", 1:n_r),
    cols = c("mu", paste0("rho", 1:n_l), "sigma")
  )
  cols <- paste0("y_l", 1:n_l)

  coefs_raw <- mod$coefficients
  thresholds <- coefs_raw[grep("^th", names(coefs_raw))]
  coefs <- matrix(
    coefs_raw[grep("^[^th]", names(coefs_raw))],
    n_r, n_l + 1, byrow = TRUE
  )

  # Regimes:
  r <- c(rep(NA_integer_, n_b), mod$model.specific$regime, rep(NA_integer_, n_h))
  for (i in (n_h - 1):0) {
    r[n_t - i] <- sum(thresholds < g(data[, "y_l1"])[n_t - i]) + 1
  }

  # Series:
  y <- c(rep(NA_real_, n_b + n_l), mod$fitted.values, rep(NA_real_, n_h))
  for (i in (n_h - 1):0) {
    y[n_t - i] <- sum(coefs[r[n_t - i], ] * c(1, data[n_t - i, cols]))
  }

  # Meta information:
  idx_fit <- (n_b + 1):(n_t - n_h)
  coefs <- cbind(coefs, series_sd(data[idx_fit, "y"] - y[idx_fit], r, n_r, na.rm = TRUE))
  ord <- regimes_order(coefs, rn_par, dims)

  meta <- list(
    coefs = `dimnames<-`(coefs[ord, ], dims),
    switches = thresholds
  )

  list(y = y, r = ord[r], meta = meta)
}
fn_env(get_results) <- new_environment(
  list(series_sd = series_sd, regimes_order = regimes_order),
  pkg_env("base")
)
