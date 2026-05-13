
# Setup ------------------------------------------------------------------------

box::use(
  src/utils[...],
  ../metrics[series_sd]
)

# * Common docs for all models in in './__init__.R'



# Creator ----------------------------------------------------------------------

#' Structural breaks
#'
#' Might only work for n_l = 1.
#'
#' Comments on parameters:
#' - h set by eps1; model with intercept; no error treatments
#'
#' @export
sb <- function(
  n_r, n_l = 1,
  min_r_size = 0.25,
  tol = 1e-5, max_iter = 10
) {
  hyperparameters <- as.list(current_env())

  f <- function(data, n_t, n_b, n_h, rn_par) {
    z_name <- paste0("y_l", 1:n_l)
    mod <- mbreaks::dofix(
      # Data:
      "y", z_name, x_name = NULL, data = data[(n_b + 1):(n_t - n_h), ],
      # Hyperparameters:
      fixn = n_r - 1,
      # Optimization:
      eps = tol, eps1 = min_r_size, maxi = max_iter, fixb = 0, betaini = NULL,
      # Others:
      prewhit = 0, robust = 0, hetdat = 0, hetvar = 0, hetq = 0, hetomega = 0,
      h = NULL, const = 1
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

  coefs <- matrix(mod$beta, n_r, n_l + 1)

  # Regimes:
  date1 <- n_b + c(1, mod$date)
  date2 <- n_b + c(mod$date - 1, n_t - n_b)

  r <- rep(NA_integer_, n_t)
  for (s in 1:n_r) {
    r[date1[s]:date2[s]] <- s
  }

  # Series:
  y <- c(rep(NA_real_, n_b), mod$fitted.values, rep(NA_real_, n_h))
  for (i in (n_h - 1):0) {
    y[n_t - i] <- sum(coefs[n_r, ] * c(1, data[n_t - i, cols]))
  }

  # Meta information:
  idx_fit <- (n_b + 1):(n_t - n_h)
  coefs <- cbind(
    coefs,
    series_sd(data[idx_fit, "y"] - y[idx_fit], r, n_r, na.rm = TRUE)
  )
  ord <- regimes_order(coefs, rn_par, dims)

  meta <- list(
    coefs = `dimnames<-`(coefs[ord, ], dims),
    switches = c(mod$date)
  )

  list(y = y, r = ord[r], meta = meta)
}
fn_env(get_results) <- new_environment(
  list(series_sd = series_sd, regimes_order = regimes_order),
  pkg_env("base")
)
