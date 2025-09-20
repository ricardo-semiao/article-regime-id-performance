
# Setup ------------------------------------------------------------------------

# Loading dependencies
box::use(
  ../utils[...],
  create_rgp = ../creators/rgps
)



# Helpers ----------------------------------------------------------------------

#' Transition matrix: diagonal
#'
#' @param p [`double(n_r)`] Probability of remaining in the same state
#' (diagonal elements).
#' @param n_r [`integer(1)`] Number of states (size of the
#' square matrix).
#'
#' @returns [`matrix(, n_r, n_r)`] Probability matrix.
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
#' @returns [`matrix(, n_r, n_r)`] Probability matrix.
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
#' @returns [`numeric()`] Logistic CDF evaluated at `x`.
logistic_cdf <- function(x, location = 0, scale = 1) {
  1 / (1 + exp(- (x - location) / scale))
}


#' Exponential CDF
#'
#' @param x [`numeric()`] Input values.
#' @param location [`numeric(1)`] Location parameter.
#' @param scale [`numeric(1)`] Scale parameter.
#'
#' @returns [`numeric()`] Exponential CDF evaluated at `x`.
exponential_cdf <- function(x, location = 0, scale = 1) {
  1 - exp(- (x - location)^2 / scale)
}


#' Names dictionary
#' @export
options_names <- c(
  multinomial_equal      = "Multinomial (Equal Probabilities)",
  multinomial_reg1       = "Multinomial (Regime 1 Favored)",
  markov_symm_high       = "Markov (Symmetric, High Persistence)",
  markov_symm_low        = "Markov (Symmetric, Low Persistence)",
  markov_asymm_high      = "Markov (Asymmetric, High Persistence)",
  markov_asymm_low       = "Markov (Asymmetric, Low Persistence)",
  sbreak_mid             = "Structural Break (Middle)",
  sbreak_end             = "Structural Break (2/3)",
  threshold_0            = "Threshold at 0",
  threshold_05           = "Threshold at 0.5",
  threshold_abs_05       = "Threshold |x| at 0.5",
  threshold_abs_2        = "Threshold |x| at 2",
  lstar_0                = "LSTAR at 0",
  lstar_05               = "LSTAR at 0.5",
  estar_0                = "ESTAR at 0",
  estar_05               = "ESTAR at 0.5"
)



# Options ----------------------------------------------------------------------

#' RGP options
#' @export
options <- list()


# 2 regimes, multinomial (equal probs)
options$multinomial_equal <- list3(
  n_r = 2,
  fun = create_rgp$markov(transmat_diag(0.5, n_r)),
  r_start = sample(1:n_r, 1)
)

# 2 regimes, multinomial (more likely to be in regime 1)
options$multinomial_reg1 <- list3(
  n_r = 2,
  fun = create_rgp$markov(transmat_main_col(0.7, n_r)),
  r_start = sample(1:n_r, 1)
)


# 2 regimes, markov, symmetric, high persistence
options$markov_symm_high <- list3(
  n_r = 2,
  fun = create_rgp$markov(transmat_diag(0.9, n_r)),
  r_start = sample(1:n_r, 1)
)

# 2 regimes, markov, symmetric, low persistence
options$markov_symm_low <- list3(
  n_r = 2,
  fun = create_rgp$markov(transmat_diag(0.6, n_r)),
  r_start = sample(1:n_r, 1)
)


# 2 regimes, markov, asymmetric, high persistence
options$markov_asymm_high <- list3(
  n_r = 2,
  fun = create_rgp$markov(transmat_main_col(c(0.9, 0.7), n_r)),
  r_start = sample(1:n_r, 1)
)

# 2 regimes, markov, asymmetric, low persistence
options$markov_asymm_low <- list3(
  n_r = 2,
  fun = create_rgp$markov(transmat_main_col(c(0.8, 0.6), n_r)),
  r_start = sample(1:n_r, 1)
)


# 2 regimes, sbreak, at the middle
n_t <- 100L # Todo: correct this temporary fix
options$sbreak_mid <- list3(
  n_r = 2,
  fun = create_rgp$sbreak(c(as.integer(n_t / 2))),
  r_start = 1
)

# 2 regimes, sbreak, at 2/3 of the way
options$sbreak_end <- list3(
  n_r = 2,
  fun = create_rgp$sbreak(c(as.integer(n_t * 2 / 3))),
  r_start = 1
)


# 2 regimes, threshold at 0
options$threshold_0 <- list3(
  n_r = 2,
  fun = create_rgp$threshold(c(0)),
  r_start = quo(fun(y, r, t_start - 1))
)

# 2 regimes, threshold at 0.5
options$threshold_05 <- list3(
  n_r = 2,
  fun = create_rgp$threshold(c(0.5)),
  r_start = quo(fun(y, r, t_start - 1))
)


# 2 regimes, threshold, g = abs, at 0.5
options$threshold_abs_05 <- list3(
  n_r = 2,
  fun = create_rgp$threshold(c(0.5), g = abs),
  r_start = quo(fun(y, r, t_start - 1))
)

# 2 regimes, threshold, g = abs, at 2
options$threshold_abs_2 <- list3(
  n_r = 2,
  fun = create_rgp$threshold(c(2), g = abs),
  r_start = quo(fun(y, r, t_start - 1))
)


# 2 regimes, star, lstar, at 0
options$lstar_0 <- list3(
  n_r = 2,
  fun = create_rgp$smooth_threshold(c(0), g = logistic_cdf),
  r_start = quo(fun(y, r, t_start - 1))
)

# 2 regimes, star, estar, at 0.5
options$lstar_05 <- list3(
  n_r = 2,
  fun = create_rgp$smooth_threshold(c(0.5), g = logistic_cdf),
  r_start = quo(fun(y, r, t_start - 1))
)


# 2 regimes, star, lstar, at 0
options$estar_0 <- list3(
  n_r = 2,
  fun = create_rgp$smooth_threshold(c(0), g = exponential_cdf),
  r_start = quo(fun(y, r, t_start - 1))
)

# 2 regimes, star, estar, at 0.5
options$estar_05 <- list3(
  n_r = 2,
  fun = create_rgp$smooth_threshold(c(0.5), g = exponential_cdf),
  r_start = quo(fun(y, r, t_start - 1))
)
