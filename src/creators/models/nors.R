
# Setup ------------------------------------------------------------------------

box::use(
  src/utils[...],
  ../metrics[series_sd]
)

# * Common docs for all models in in './__init__.R'



# Creator ----------------------------------------------------------------------

#' Creator - Model: no RS
#'
#' @param n_r [`integer(1)`] Number of regimes. Only `1` is supported.
#' @param n_l [`integer(1)`] Number of lags. Only `1` is supported.
#'
#' @returns [`function(data, n_t, n_b, n_h, rn_par)`] Function to fit the model.
#' @export
nors <- function(
  n_r = 1, n_l = 1
) {
  if (n_l != 1) cli_abort("Only n_l = 1 is currently supported for ar().")
  if (n_r != 1) cli_abort("Only n_r = 1 is supported for ar().")

  hyperparameters <- as.list(current_env())

  f <- function(data, n_t, n_b, n_h, rn_par) {
    mod <- stats::lm(
      data[(n_b + 1):(n_t - n_h), "y"] ~ data[(n_b + 1):(n_t - n_h), "y_l1"]
    )
    get_results(data, mod, n_t, n_b, n_h, n_l)
  }

  fn_env(f) <- new_environment(
    c(hyperparameters, get_results = get_results),
    pkg_env("base")
  )

  f
}

get_results <- function(data, mod, n_t, n_b, n_h, n_l) {
  dims <- list(
    rows = paste0("R", 1),
    cols = c("mu", paste0("rho", 1:n_l), "sigma")
  )
  cols <- paste0("y_l", 1:n_l)

  # Regimes:
  r <- c(rep(NA_integer_, n_b), rep(1, n_t - n_b))

  # Series:
  y <- c(rep(NA_real_, n_b), mod$fitted.values, rep(NA_real_, n_h))
  for (i in 0:(n_h - 1)) {
    y[n_t - i] <- sum(mod$coefficients * c(1, data[n_t - i, cols]))
  }

  # Meta information:
  idx_fit <- (n_b + 1):(n_t - n_h)
  coefs <- cbind(
    matrix(mod$coefficients, 1, n_l + 1, byrow = TRUE),
    series_sd(data[idx_fit, "y"] - y[idx_fit], r, 1, na.rm = TRUE)
  )

  meta <- list(
    coefs = `dimnames<-`(coefs, dims)
  )

  list(y = unname(y), r = r, meta = meta)
}
fn_env(get_results) <- new_environment(
  list(series_sd = series_sd, regimes_order = regimes_order),
  pkg_env("base")
)
