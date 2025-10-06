
# Setup ------------------------------------------------------------------------

# Loading dependencies:
box::use(
  src/utils[...]
)



# Helper -----------------------------------------------------------------------

#' Helper: Compute metric based on SGP
#' @export
sgp_metric <- function(sgp, y, r, n_r = length(unique(r)), ...) {
  force(n_r)

  switch(str_replace(sgp, "^r[0-9]_ar[0-9]_([a-z]+)[0-9]$", "\\1"),
    "mu"   = series_average(y, r, n_r, ...) |>
      set_names(glue("Mean(y | R{1:n_r})")),
    "rho"  = series_autocorr(y, r, n_r, ...) |>
      set_names(glue("ACF1(y | R{1:n_r})")),
    "sign" = series_sign_prop(y, r, n_r, ...) |>
      set_names(glue("Sign%(y | R{1:n_r})")),
    "vol"  = series_volatility(y, r, n_r, ...) |>
      set_names(glue("SD(y | R{1:n_r})")),
    c("NA" = NA) # Not considering new lag
  )
}

#' Helper: Compute metric based on RGP
#' @export
rgp_metric <- function(rgp, y, r, n_r = length(unique(r)), ...) {
  force(n_r)

  transmat_diag_named <- function(y, r, n_r, ...) {
    names <- glue("Prob({1:n_r} | {1:n_r})")
    set_names(diag(regimes_transmat(y, r, n_r, ...)), names)
  }
  thresholds_named <- function(y, r, n_r, ...) {
    names <- if (n_r == 1) {
      c("Threshold(1-2)")
    } else {
      glue("Threshold({1:(n_r - 1)}-{2:n_r})")
    }
    set_names(regimes_thresholds(y, r, n_r, ...), names)
  }

  switch(str_replace(rgp, "^r[0-9]_([a-z]+)_[a-z0-9_]+$", "\\1"),
    "threshold"   = thresholds_named(y, r, n_r, ...),
    "stransition" = thresholds_named(y, r, n_r, ...),
    "markov"      = transmat_diag_named(y, r, n_r, ...),
    c("NA" = NA) # Not considering multinomial nor sbreaks
  )
}



# Dispersion Metrics -----------------------------------------------------------

# All receive a vector of metrics x

#' Metrics - dispersion: Mean pairwise distance
#' @param x [`numeric()`]
#' @returns [`double(1)`]
#' @export
mean_pairwise_dist <- function(x) {
  mean(dist(x))
}

# Others: simply `sd()`. Todo: what else?

# Temporary example:
if (FALSE) {
  y <- c(rnorm(30, 4), rnorm(40, 0), rnorm(30, 2))
  r <- c(rep(3, 30), rep(1, 40), rep(2, 30)) # Note the mu-based order
  r <- sample(3, 100, replace = TRUE)
  n_r <- 3
}



# Performance Metrics ----------------------------------------------------------

# All receive y, y_true, n_h, n_t, and additional hyperparameters if needed,
# and return a single numeric value

#' Metrics - performance: RMSE
#' @export
performance_rmse <- function(y, y_true, n_h, n_t) {
  error <- y[(n_t - n_h + 1):n_t] - y_true[(n_t - n_h + 1):n_t]
  sqrt(sum(error^2) / length(n_h))
}

#' Metrics - performance: MAPE
#' @export
performance_mape <- function(y, y_true, n_h, n_t) {
  error <- y[(n_t - n_h + 1):n_t] - y_true[(n_t - n_h + 1):n_t]
  mean(abs(error) / abs(y_true[(n_t - n_h + 1):n_t]))
}



# Series Metrics ---------------------------------------------------------------

# All receive y, r, n_r, and additional hyperparameters if needed, and return a
# vector of length n_r

#' Metrics - series: Conditional average
#' @export
series_average <- function(y, r, n_r = length(unique(r)), ...) {
  vapply(1:n_r, FUN.VALUE = numeric(1), FUN = \(s) {
    mean(y[r == s], ...)
  })
}

#' Metrics - series: Conditional ACF
#' @param n [`integer(1)`] Lag order.
#' @export
series_autocorr <- function(y, r, n_r = length(unique(r)), n = 1, ...) {
  vapply(1:n_r, FUN.VALUE = numeric(1), FUN = \(s) {
    cor(y[r == s][-(1:n)], lag(y[r == s], n)[-(1:n)], ...)
  })
}

#' Metrics - series: Conditional ACF
#' @export
series_sign_prop <- function(y, r, n_r = length(unique(r)), ...) {
  vapply(1:n_r, FUN.VALUE = numeric(1), FUN = \(s) {
    mean(diff(y[r == s]) >= 0, ...)
  })
}

#' Metrics - series: Conditional SD
#' @export
series_volatility <- function(y, r, n_r = length(unique(r)), ...) {
  vapply(1:n_r, FUN.VALUE = numeric(1), FUN = \(s) {
    sd(y[r == s], ...)
  })
}



# Regimes Metrics --------------------------------------------------------------

# All receive y, r, n_r, and additional hyperparameters if needed, and most
# return a vector of length n_r, unless otherwise specified

#' Metrics - regimes: Number of regimes' instances
#' For each regime's observations, counts how many had a different previous
#'  value
#' @export
regimes_instances <- function(y, r, n_r = length(unique(r)), ...) {
  vapply(1:n_r, FUN.VALUE = numeric(1), FUN = \(s) {
    sum((c(1, diff(r)) != 0)[r == s], ...)
  })
}

#' Metrics - regimes: Average duration of regimes' instances
#' The cumulative sum of absolute differences generates a unique id for each
#'  instance (across all regimes). `r == s` subsets the ones for a specific
#'  regime, and table counts how many observations each instance had.
#' @export
regimes_duration <- function(y, r, n_r = length(unique(r)), ...) {
  vapply(1:n_r, FUN.VALUE = numeric(1), FUN = \(s) {
    mean(table(cumsum(abs(c(0, diff(r))))[r == s]), ...)
  })
}

#' Metrics - regimes: Estimated transition counts/probabilities matrix
#' Creates a factor whose levels are all possible combinations of
#'  "$r_{t-1}$_$r_t$", then tabulates it and reshapes into a matrix.
#' @param prop [`logical(1)`] Whether to return transition probabilities
#' @returns [`matrix(, n_r, n_r)`]
#' @export
regimes_transmat <- function(y, r, n_r = length(unique(r)), prop = TRUE, ...) {
  if (n_r < 2) cli_abort("At least 2 regimes are required.")

  r_lead <- r[-1]
  x <- paste0(r_lead - diff(r), "_", r_lead)
  levels <- paste0(rep(1:n_r, each = n_r), "_", rep(1:n_r, times = n_r))

  counts <- matrix(table(factor(x, levels)), n_r, n_r, byrow = TRUE)

  if (prop) counts / rowSums(counts) else counts
}

#' Metrics - regimes: Average duration of regimes' instances
#' Assuming regimes are in ascending order or their mean value, returns the
#'  midpoint between min. of upper regime and max. of lower regime, even if they
#'  overlap. Calculated in a pairwise fashion.
#' @export
regimes_thresholds <- function(y, r, n_r = length(unique(r)), ...) {
  pairs <- if (n_r == 1) {
    tibble(r1 = 1, r2 = 2)
  } else {
    tibble(r1 = 1:(n_r - 1), r2 = 2:n_r)
  }

  pmap_dbl(pairs, \(r1, r2) {
    min_top_r <- min(y[r == r2], ...)
    max_bot_r <- max(y[r == r1], ...)
    dist <- min_top_r - max_bot_r
    if (dist > 0) min_top_r + dist / 2 else max_bot_r - dist / 2
  })
}
# Todo: consider `... else NA`
