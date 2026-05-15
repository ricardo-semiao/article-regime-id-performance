# Setup ------------------------------------------------------------------------

# Loading dependencies:
box::use(
  src/utils[...]
)

bare_stats <- new_environment(
  list(cov = bare_cov, cor = bare_cor, sd = bare_sd, lag = lag, acor = acor),
  pkg_env("base")
)


# Temporary example:
if (FALSE) {
  y <- c(rnorm(30, 4), rnorm(40, 0), rnorm(30, 2))
  r <- c(rep(3, 30), rep(1, 40), rep(2, 30)) # Note the mu-based order
  r <- sample(3, 100, replace = TRUE)
  n_r <- 3
}



# Dispersion Metrics -----------------------------------------------------------

# All receive a vector of metrics x

#' Metrics - dispersion: Mean pairwise distance
#'
#' @param x [`double()`] Input vector.
#' @param ... Additional arguments passed to [base::dist()].
#'
#' @returns [`double(1)`] Mean pairwise distance.
#' @export
disp_mpe <- function(x, k, n = length(x), ...) {
  if (n == 1) {
    0
  } else {
    mean(vapply(1:n, \(i) abs(x[i] - x[-i]), double(n - 1)), ...)
  }
}
fn_env(disp_mpe) <- pkg_env("base")

#' Metrics - dispersion: Difference raised to power
#'
#' @param x [`double()`] Input vector of length 2.
#' @param k [`integer(1)`] Power to raise the difference.
#'
#' @returns [`double(1)`] Difference raised to the power `k`.
#' @export
diff_k_2 <- function(x, k = 1) {
  abs(x[1] - x[2])^k
}
fn_env(diff_k_2) <- pkg_env("base")

# Others: simply `sd()`



# Performance Metrics ----------------------------------------------------------

# All receive y, y_true, and additional hyperparameters if needed,
# and return a single numeric value

#' Metrics - performance: R squared
#'
#' @param y_est [`double()`] Estimated values.
#' @param y_true [`double()`] True values.
#' @param ... Additional arguments passed to [base::mean()].
#'
#' @returns [`double(1)`] R squared value.
#' @export
performance_r2 <- function(y_est, y_true, ...) {
  y_bar <- mean(y_true, ...)
  ss_reg <- sum((y_est - y_bar)^2, ...)
  ss_tot <- sum((y_true - y_bar)^2, ...)
  ss_reg / ss_tot
}
fn_env(performance_r2) <- pkg_env("base")

#' Metrics - performance: RMSE
#'
#' @param y_err [`double()`] Error values.
#' @param ... Additional arguments passed to [base::mean()].
#'
#' @returns [`double(1)`] Root mean squared error.
#' @export
performance_rmse <- function(y_err, ...) {
  sqrt(mean(y_err^2, ...))
}
fn_env(performance_rmse) <- pkg_env("base")

#' Metrics - performance: MAPE
#'
#' @param y_err [`double()`] Error values.
#' @param y_true [`double()`] True values.
#' @param ... Additional arguments passed to [base::mean()].
#'
#' @returns [`double(1)`] Mean absolute percentage error.
#' @export
performance_mape <- function(y_err, y_true, ...) {
  mean(abs(y_err) / abs(y_true), ...)
}
fn_env(performance_mape) <- pkg_env("base")

#' Metrics - performance: Binary ME
#'
#' @param r_err [`double()`] Binary error values.
#' @param ... Additional arguments passed to [base::mean()].
#'
#' @returns [`double(1)`] Binary mean error.
#' @export
performance_bme <- function(r_err, ...) {
  mean(r_err, ...)
}
fn_env(performance_bme) <- pkg_env("base")



# Series Metrics ---------------------------------------------------------------

# All receive y, r, n_r, and additional hyperparameters if needed, and return a
# vector of length n_r

#' Metrics - series: Conditional average
#'
#' @param y [`double()`] Input series.
#' @param r [`integer()`] Regime identifiers.
#' @param n_r [`integer(1)`] Number of regimes.
#' @param ... Additional arguments passed to [base::mean()].
#'
#' @returns [`double()`] Conditional averages for each regime.
#' @export
series_avg <- function(y, r, n_r = max(r, na.rm = TRUE), ...) {
  vapply(1:n_r, FUN.VALUE = double(1), FUN = \(s) {
    mean(y[r == s], ...)
  })
}
fn_env(series_avg) <- pkg_env("base")

#' Metrics - series: Conditional ACF
#'
#' @param y [`double()`] Input series.
#' @param r [`integer()`] Regime identifiers.
#' @param n_r [`integer(1)`] Number of regimes.
#' @param n [`integer(1)`] Lag order.
#' @param ... Additional arguments passed to [base::mean()].
#'
#' @returns [`double()`] Conditional autocorrelations for each regime.
#' @export
series_acf <- function(y, r, n_r = max(r, na.rm = TRUE), n = 1, ...) {
  t <- 1:length(y)

  vapply(1:n_r, FUN.VALUE = double(1), FUN = \(s) {
    instances <- split(y[r == s], cumsum(c(1, diff(t[r == s]) != 1)))

    cors <- vapply(instances, FUN.VALUE = double(1), \(yi) {
      if (length(yi) <= n + 1) return(0)
      acor(yi, p = n, ...)
    })

    weigths <- vapply(seq_along(instances), FUN.VALUE = double(1), \(i) {
      if (is.na(cors[i]) || cors[i] == 0) 0 else length(instances[[i]]) - n
    })

    sum(cors * weigths, na.rm = TRUE) / sum(weigths)
  })
}
fn_env(series_acf) <- bare_stats

#' Metrics - series: Conditional sign proportion
#'
#' @param y [`double()`] Input series.
#' @param r [`integer()`] Regime identifiers.
#' @param n_r [`integer(1)`] Number of regimes.
#' @param ... Additional arguments passed to [base::mean()].
#'
#' @returns [`double()`] Proportion of positive differences for each regime.
#' @export
series_sign_prop <- function(y, r, n_r = max(r, na.rm = TRUE), ...) {
  vapply(1:n_r, FUN.VALUE = double(1), FUN = \(s) {
    mean(diff(y[r == s]) >= 0, ...)
  })
}
fn_env(series_sign_prop) <- bare_stats

#' Metrics - series: Conditional SD
#'
#' @param y [`double()`] Input series.
#' @param r [`integer()`] Regime identifiers.
#' @param n_r [`integer(1)`] Number of regimes.
#' @param ... Additional arguments passed to [stats::sd()].
#'
#' @returns [`double()`] Conditional standard deviations for each regime.
#' @export
series_sd <- function(y, r, n_r = max(r, na.rm = TRUE), ...) {
  vapply(1:n_r, FUN.VALUE = double(1), FUN = \(s) {
    sd(y[r == s], ...)
  })
}
fn_env(series_sd) <- bare_stats



# Analytical Metrics -----------------------------------------------------------

# All receive the model's parameters, n_r, and return a vector of length n_r

#' Metrics - analytical: Conditional average
#'
#' @param coefs [`matrix()`] Model coefficients.
#' @param n_r [`integer(1)`] Number of regimes.
#'
#' @returns [`double()`] Analytical averages for each regime.
#' @export
analytical_avg <- function(coefs, n_r = max(r, na.rm = TRUE)) {
  apply(coefs, 1, \(coefs_s) {
    coefs_s["mu"] / (1 - coefs_s["rho1"])
  })
}
fn_env(analytical_avg) <- pkg_env("base")

#' Metrics - analytical: Conditional ACF
#'
#' @param coefs [`matrix()`] Model coefficients.
#' @param n_r [`integer(1)`] Number of regimes.
#' @param lag [`integer(1)`] Lag order.
#'
#' @returns [`double()`] Analytical autocorrelations for each regime.
#' @export
analytical_acf <- function(coefs, n_r = max(r, na.rm = TRUE), lag = 1) {
  apply(coefs, 1, \(coefs_s) {
    coefs_s["rho1"]^lag
  })
}
fn_env(analytical_acf) <- pkg_env("base")

#' Metrics - analytical: Conditional SD
#'
#' @param coefs [`matrix()`] Model coefficients.
#' @param n_r [`integer(1)`] Number of regimes.
#'
#' @returns [`double()`] Analytical standard deviations for each regime.
#' @export
analytical_sd <- function(coefs, n_r = max(r, na.rm = TRUE)) {
  apply(coefs, 1, \(coefs_s) {
    sqrt(coefs_s["sigma"]^2 / (1 - coefs_s["rho1"]^2))
  })
}
fn_env(analytical_sd) <- pkg_env("base")



# Regimes Metrics --------------------------------------------------------------

# All receive y, r, n_r, and additional hyperparameters if needed, and most
# return a vector of length n_r, unless otherwise specified

#' Metrics - regimes: Number of instances
#'
#' @param y [`double()`] Input series.
#' @param r [`integer()`] Regime identifiers.
#' @param n_r [`integer(1)`] Number of regimes.
#' @param ... Additional arguments passed to [base::sum()].
#'
#' @returns [`double()`] Number of instances for each regime.
#' @export
regimes_instances <- function(y, r, n_r = max(r, na.rm = TRUE), ...) {
  vapply(1:n_r, FUN.VALUE = double(1), FUN = \(s) {
    sum((c(1, diff(r)) != 0)[r == s], ...)
  })
}
fn_env(regimes_instances) <- pkg_env("base")

#' Metrics - regimes: Average duration
#'
#' The cumulative sum of absolute differences generates a unique id for each
#'  instance (across all regimes). `r == s` subsets the ones for a specific
#'  regime, and table counts how many observations each instance had.
#'
#' @param y [`double()`] Input series.
#' @param r [`integer()`] Regime identifiers.
#' @param n_r [`integer(1)`] Number of regimes.
#' @param ... Additional arguments passed to [base::mean()].
#'
#' @returns [`double()`] Average duration of instances for each regime.
#' @export
regimes_duration <- function(y, r, n_r = max(r, na.rm = TRUE), ...) {
  vapply(1:n_r, FUN.VALUE = double(1), FUN = \(s) {
    idx <- r == s
    if (!any(idx)) return(0)
    mean(table(cumsum(abs(c(0, diff(r))))[idx]), ...)
  })
}
fn_env(regimes_duration) <- pkg_env("base")

#' Metrics - regimes: Transition matrix
#'
#' Creates a factor whose levels are all possible combinations of
#'  "$r_{t-1}$_$r_t$", then tabulates it and reshapes into a matrix.
#'
#' @param y [`double()`] Input series.
#' @param r [`integer()`] Regime identifiers.
#' @param n_r [`integer(1)`] Number of regimes.
#' @param prop [`logical(1)`] Whether to return probabilities.
#' @param ... Additional arguments passed to [base::table()].
#'
#' @returns [`matrix(, n_r, n_r)`] Transition matrix of counts or probabilities.
#' @export
regimes_transmat <- function(y, r, n_r = max(r, na.rm = TRUE), prop = TRUE, ...) {
  if (n_r < 2) {
    n_r <- 2
    cli_warn("{.arg n_r} must be atleast 2, assuming {.code n_r = 2}.")
  }

  r_lead <- r[-1]
  x <- paste0(r_lead - diff(r), "_", r_lead)
  levels <- paste0(rep(1:n_r, each = n_r), "_", rep(1:n_r, times = n_r))

  counts <- matrix(table(factor(x, levels)), n_r, n_r, byrow = TRUE)

  if (prop) counts / rowSums(counts) else counts
}
#fn_env(regimes_transmat) <- pkg_env("base")

#' Metrics - regimes: Average switches
#'
#' @param y [`double()`] Input series.
#' @param r [`integer()`] Regime identifiers.
#' @param n_r [`integer(1)`] Number of regimes.
#' @param ... Additional arguments passed to [base::sum()].
#'
#' @returns [`double(1)`] Average number of switches per observation.
#' @export
average_switches <- function(y, r, n_r = max(r, na.rm = TRUE), ...) {
  sum(diff(r) != 0) / length(y)
}
fn_env(average_switches) <- pkg_env("base")



# Inconditional Series Metrics -------------------------------------------------


#' Metrics - series: Skewness
#'
#' @param y [`double()`] Input series.
#' @param ... Additional arguments passed to [base::mean()].
#'
#' @returns [`double(1)`] Skewness of the series.
#' @export
inconditional_skewness <- function(y, ...) {
  sdev <- sd(y, ...)
  if (sdev == 0) return(0)
  mean((y - mean(y, ...))^3, ...) / (sdev^3)
}
fn_env(inconditional_skewness) <- bare_stats

#' Metrics - series: Kurtosis
#'
#' @param y [`double()`] Input series.
#' @param ... Additional arguments passed to [base::mean()].
#'
#' @returns [`double(1)`] Kurtosis of the series.
#' @export
inconditional_kurtosis <- function(y, ...) {
  sdev <- sd(y, ...)
  if (sdev == 0) return(0)
  mean((y - mean(y, ...))^4, ...) / (sdev^4)
}
fn_env(inconditional_kurtosis) <- bare_stats
