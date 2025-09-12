
# Helpers ----------------------------------------------------------------------

#' Transition matrix: diagonal
#'
#' @param p [`double(n_r)`] Probability of remaining in the same state
#' (diagonal elements).
#' @param n_r [`integer(1)`] Number of states (size of the
#' square matrix).
#'
#' @return [`matrix(, n_r, n_r)`] Probability matrix.
transmat_diag <- function(p, n_r) {
  mat <- matrix(0, n_r, n_r)

  if (length(p) == 1) {
    p <- rep(p, n_r)
  }

  for (i in seq_len(n_r)) {
    mat[i, i] <- p[i]
    mat[i, -i] <- (1 - p[i]) / (n_r - 1)
  }

  mat
}


#' Transition matrix: one column with different probability
#'
#' @param p [`double(n_r)`] Probability of `col` variable.
#' @param n_r [`integer(1)`] Number of regimes (matrix dimension).
#' @param col [`integer(1)`] Column with different probability.
#'
#' @return [`matrix(, n_r, n_r)`] Probability matrix.
transmat_main_col <- function(p, n_r, col = 1) {
  mat <- matrix(0, n_r, n_r)

  if (length(p) == 1) {
    p <- rep(p, n_r)
  }

  for (i in seq_len(n_r)) {
    mat[i, col] <- p[i]
    mat[i, -col] <- (1 - p[i]) / (n_r - 1)
  }

  mat
}


#' Logistic CDF
#'
#' @param x [`numeric()`] input values.
#' @param location [`numeric(1)`] location parameter.
#' @param scale [`numeric(1)`] scale parameter.
#'
#' @return [`numeric()`] Logistic CDF evaluated at `x`.
logistic_cdf <- function(x, location = 0, scale = 1) {
  1 / (1 + exp(- (x - location) / scale))
}


#' Exponential CDF
#'
#' @param x [`numeric()`] Input values.
#' @param location [`numeric(1)`] Location parameter.
#' @param scale [`numeric(1)`] Scale parameter.
#'
#' @return [`numeric()`] Exponential CDF evaluated at `x`.
exponential_cdf <- function(x, location = 0, scale = 1) {
  1 - exp(- (x - location)^2 / scale)
}



# Options ----------------------------------------------------------------------

options_rgp <- list()


# 2 regimes, multinomial (equal probs)
options_rgp$multinomial_equal <- list3(
  n_r = 2,
  fun = create_rgp$markov(transmat_diag(1 / 2, n_r))
)

# 2 regimes, multinomial (more likely to be in regime 1)
options_rgp$multinomial_reg1 <- list3(
  n_r = 2,
  fun = create_rgp$markov(transmat_main_col(0.7, n_r)),
)


# 2 regimes, markov, symmetric, high persistence
options_rgp$markov_high <- list3(
  n_r = 2,
  fun = create_rgp$markov(transmat_diag(0.9, n_r))
)

# 2 regimes, markov, symmetric, low persistence
options_rgp$markov_low <- list3(
  n_r = 2,
  fun = create_rgp$markov(transmat_diag(0.6, n_r))
)


# 2 regimes, sbreak, at the middle
options_rgp$sbreak_mid <- list3(
  n_r = 2,
  fun = create_rgp$sbreak(c(as.integer(n_t / 2)))
)

# 2 regimes, sbreak, at 2/3 of the way
options_rgp$sbreak_end <- list3(
  n_r = 2,
  fun = create_rgp$sbreak(c(as.integer(n_t * 2 / 3)))
)


# 2 regimes, threshold at 0
options_rgp$threshold_0 <- list3(
  n_r = 2,
  fun = create_rgp$threshold(c(0))
)

# 2 regimes, threshold at 0.5
options_rgp$threshold_05 <- list3(
  n_r = 2,
  fun = create_rgp$threshold(c(0.5))
)


# 2 regimes, threshold, g = abs, at 0.5
options_rgp$threshold_abs_0 <- list3(
  n_r = 2,
  fun = create_rgp$threshold(c(0.5), g = abs)
)

# 2 regimes, threshold, g = abs, at 2
options_rgp$threshold_abs_05 <- list3(
  n_r = 2,
  fun = create_rgp$threshold(c(2), g = abs)
)


# 2 regimes, star, lstar, at 0
options_rgp$lstar_0 <- list3(
  n_r = 2,
  fun = create_rgp$threshold(c(0), g = logistic_cdf)
)

# 2 regimes, star, estar, at 0.5
options_rgp$lstar_05 <- list3(
  n_r = 2,
  fun = create_rgp$threshold(c(0.5), g = logistic_cdf)
)


# 2 regimes, star, lstar, at 0
options_rgp$estar_0 <- list3(
  n_r = 2,
  fun = create_rgp$threshold(c(0), g = exponential_cdf)
)

# 2 regimes, star, estar, at 0.5
options_rgp$estar_05 <- list3(
  n_r = 2,
  fun = create_rgp$threshold(c(0.5), g = exponential_cdf)
)
