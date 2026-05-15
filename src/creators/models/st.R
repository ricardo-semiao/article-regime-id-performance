
# Setup ------------------------------------------------------------------------

box::use(
  src/utils[...],
  ../metrics[series_sd]
)

# * Common docs for all models in in './__init__.R'


# Creator ----------------------------------------------------------------------

#' Creator - Model: Smooth transition
#'
#' Only works for 2 regimes.
#' - m, ML, MM, MH given by mL etc.; th missing (will be estimated)
#' - mTh, thDelay missing, given by thVar
#'
#' @param n_r [`integer(1)`] Number of regimes. Only `2` is supported.
#' @param n_l [`integer(1)`] Number of lags.
#' @param gamma [`double()`] Transition smoothness parameter.
#' @param min_r_size [`double(1)`] Minimum regime size as a proportion.
#' @param tol [`double(1)`] Convergence tolerance.
#' @param max_iter [`integer(1)`] Maximum number of iterations.
#'
#' @returns [`function(data, n_t, n_b, n_h, rn_par)`] Function to fit the model.
#' @export
st <- function(
  n_r = 2, n_l = 1, gamma = NULL,
  min_r_size = 0.1,
  tol = 1e-5, max_iter = 10
) {
  gamma <- gamma %||% quote(expr = )
  hyperparameters <- as.list(current_env())

  f <- function(data, n_t, n_b, n_h, rn_par) {
    mod <- tsDyn::lstar(
      # Data:
      data[(n_b + 1):(n_t - n_h), "y"], mL = n_l, mH = n_l, thDelay = n_l,
      # Hyperparameters:
      gamma = gamma,
      # Optimization: starting.control
      control = list(maxit = max_iter, abstol = tol),
      # Others:
      d = 1, steps = 1, include = "const", trace = FALSE
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

  threshold <- mod$coefficients["th"]
  gamma <- mod$coefficients["gamma"]
  coefs <- matrix(
    mod$coefficients[grep("const|phi", names(mod$coefficients))],
    2, n_l + 1, byrow = TRUE
  )

  # Regimes:
  r <- c(
    rep(NA_real_, n_b + n_l + 1),
    1 / (1 + exp(- (data[(n_b + n_l + 2):n_t, "y_l1"] - threshold) / gamma))
  )
  r_cat <- (r <= 0.5) + 1

  # Series:
  y <- c(rep(NA_real_, n_b + n_l + 1), mod$fitted.values, rep(NA_real_, n_h))
  for (i in (n_h - 1):0) {
    r_i <- r[n_t - i]
    y[n_t - i] <- sum(
      (coefs[1, ] * r_i + coefs[2, ] * (1 - r_i)) * c(1, data[n_t - i, cols])
    )
  }

  # Meta information:
  idx_fit <- (n_b + 1):(n_t - n_h)
  coefs <- cbind(coefs, series_sd(data[idx_fit, "y"] - y[idx_fit], r_cat, n_r, na.rm = TRUE))
  ord <- regimes_order(coefs, rn_par, dims)

  meta <- list(
    coefs = `dimnames<-`(coefs[ord, ], dims),
    switches = threshold,
    gamma = gamma
  )

  list(y = y, r = ord[r_cat], meta = meta)
}
fn_env(get_results) <- new_environment(
  list(series_sd = series_sd, regimes_order = regimes_order),
  pkg_env("base")
)
