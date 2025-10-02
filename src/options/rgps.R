
# Setup ------------------------------------------------------------------------

# Loading dependencies
box::use(
  src/utils[...],
  create_rgp = src/creators/rgps
)



# Helpers ----------------------------------------------------------------------

#' Internal: Create a diagonal transition matrix
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

#' Internal: Create a transition matrix with favored column
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

#' Internal: Logistic CDF
#'
#' @param x [`numeric()`] input values.
#' @param location, scale [`numeric(1)`] Location and scale parameters.
#'
#' @returns [`numeric()`] Logistic CDF evaluated at `x`.
logistic_cdf <- function(x, location = 0, scale = 1) {
  1 / (1 + exp(- (x - location) / scale))
}

#' Internal: Exponential CDF
#'
#' @param x [`numeric()`] Input values.
#' @param location, scale [`numeric(1)`] Location and scale parameters.
#'
#' @returns [`numeric()`] Exponential CDF evaluated at `x`.
exponential_cdf <- function(x, location = 0, scale = 1) {
  1 - exp(- (x - location)^2 / scale)
}

#' Names dictionary
#' @export
options_names <- c(
  r2_multinomial_equal      = "Multinomial (Equal Probabilities)",
  r2_multinomial_reg1       = "Multinomial (Regime 1 Favored)",
  r2_markov_symm_high       = "Markov (Symmetric, High Persistence)",
  r2_markov_symm_low        = "Markov (Symmetric, Low Persistence)",
  r2_markov_asymm_high      = "Markov (Asymmetric, High Persistence)",
  r2_markov_asymm_low       = "Markov (Asymmetric, Low Persistence)",
  r2_sbreak_mid             = "Structural Break (Middle)",
  r2_sbreak_end             = "Structural Break (2/3)",
  r2_threshold_x_0          = "Threshold at 0",
  r2_threshold_x_05         = "Threshold at 0.5",
  r2_threshold_abs_05       = "Threshold |x| at 0.5",
  r2_threshold_abs_2        = "Threshold |x| at 2",
  r2_threshold_diff_05      = "Threshold Dx at 0.5",
  r2_threshold_diff_2       = "Threshold Dx at 2",
  r2_lstar_0                = "LSTAR at 0",
  r2_lstar_05               = "LSTAR at 0.5",
  r2_estar_0                = "ESTAR at 0",
  r2_estar_05               = "ESTAR at 0.5"
)



# Options ----------------------------------------------------------------------

#' RGP options
#' @export
options <- list()


# 2 regimes, multinomial (equal probs)
options$r2_multinomial_equal <- list3(
  n_r = 2,
  fun = create_rgp$markov(transmat_diag(0.5, n_r)),
  r_start = sample(1:n_r, 1)
)

# 2 regimes, multinomial (more likely to be in regime 1)
options$r2_multinomial_reg1 <- list3(
  n_r = 2,
  fun = create_rgp$markov(transmat_main_col(0.7, n_r)),
  r_start = sample(1:n_r, 1)
)


# 2 regimes, markov, symmetric, high persistence
options$r2_markov_symm_high <- list3(
  n_r = 2,
  fun = create_rgp$markov(transmat_diag(0.9, n_r)),
  r_start = sample(1:n_r, 1)
)

# 2 regimes, markov, symmetric, low persistence
options$r2_markov_symm_low <- list3(
  n_r = 2,
  fun = create_rgp$markov(transmat_diag(0.6, n_r)),
  r_start = sample(1:n_r, 1)
)


# 2 regimes, markov, asymmetric, high persistence
options$r2_markov_asymm_high <- list3(
  n_r = 2,
  fun = create_rgp$markov(transmat_main_col(c(0.9, 0.7), n_r)),
  r_start = sample(1:n_r, 1)
)

# 2 regimes, markov, asymmetric, low persistence
options$r2_markov_asymm_low <- list3(
  n_r = 2,
  fun = create_rgp$markov(transmat_main_col(c(0.8, 0.6), n_r)),
  r_start = sample(1:n_r, 1)
)


# 2 regimes, sbreak, at the middle
n_t <- 100L # Todo: correct this temporary fix
options$r2_sbreak_mid <- list3(
  n_r = 2,
  fun = create_rgp$sbreak(c(as.integer(n_t / 2))),
  r_start = 1
)

# 2 regimes, sbreak, at 2/3 of the way
options$r2_sbreak_end <- list3(
  n_r = 2,
  fun = create_rgp$sbreak(c(as.integer(n_t * 2 / 3))),
  r_start = 1
)


# 2 regimes, threshold at 0
options$r2_threshold_x_0 <- list3(
  n_r = 2,
  fun = create_rgp$threshold(c(0)),
  r_start = expr(fun(y, r, t_start))
)

# 2 regimes, threshold at 0.5
options$r2_threshold_x_05 <- list3(
  n_r = 2,
  fun = create_rgp$threshold(c(0.5)),
  r_start = expr(fun(y, r, t_start))
)


# 2 regimes, threshold, g = abs, at 0.5
options$r2_threshold_abs_05 <- list3(
  n_r = 2,
  fun = create_rgp$threshold(c(0.5), g = \(y, t) abs(y[t - 1])),
  r_start = expr(fun(y, r, t_start))
)

# 2 regimes, threshold, g = abs, at 2
options$r2_threshold_abs_2 <- list3(
  n_r = 2,
  fun = create_rgp$threshold(c(2), g = \(y, t) abs(y[t - 1])),
  r_start = expr(fun(y, r, t_start))
)


# 2 regimes, threshold, g = diff, at 0
options$r2_threshold_diff_05 <- list3(
  n_r = 2,
  fun = create_rgp$threshold(c(0.5), g = \(y, t) diff(y[(t-2):(t-1)])),
  r_start = expr(fun(y, r, t_start))
)

# 2 regimes, threshold, g = diff, at 0.5
options$r2_threshold_diff_2 <- list3(
  n_r = 2,
  fun = create_rgp$threshold(c(2), g = \(y, t) diff(y[(t-2):(t-1)])),
  r_start = expr(fun(y, r, t_start))
)


# 2 regimes, star, lstar, at 0
options$r2_lstar_0 <- list3(
  n_r = 2,
  fun = create_rgp$smooth_threshold(
    c(0), g = \(y, t, breaks) logistic_cdf(y[t-1], location = breaks, scale = 1)
  ),
  r_start = expr(fun(y, r, t_start))
)

# 2 regimes, star, estar, at 0.5
options$r2_lstar_05 <- list3(
  n_r = 2,
  fun = create_rgp$smooth_threshold(
    c(0.5), g = \(y, t, breaks) logistic_cdf(y[t-1], location = breaks, scale = 1)
  ),
  r_start = expr(fun(y, r, t_start))
)


# 2 regimes, star, lstar, at 0
options$r2_estar_0 <- list3(
  n_r = 2,
  fun = create_rgp$smooth_threshold(
    c(0), g = \(y, t, breaks) exponential_cdf(y[t-1], location = breaks, scale = 1)
  ),
  r_start = expr(fun(y, r, t_start))
)

# 2 regimes, star, estar, at 0.5
options$r2_estar_05 <- list3(
  n_r = 2,
  fun = create_rgp$smooth_threshold(
    c(0.5), g = \(y, t, breaks) exponential_cdf(y[t-1], location = breaks, scale = 1)
  ),
  r_start = expr(fun(y, r, t_start))
)
