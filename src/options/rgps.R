
# Setup ------------------------------------------------------------------------

# Loading dependencies:
box::use(
  src/utils[...],
  latex2exp[TeX],
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



# Options ----------------------------------------------------------------------

#' RGPs' names dictionary
#' @export
dict <- list(
  gt = c(
    r2_markov_symm_high = "MS, $p_{21} = 0.1$",
    r2_markov_symm_low = "MS, $p_{21} = 0.7$",
    r2_threshold_x_0 = "SET, $\\\\tau = 0$",
    r2_threshold_x_05 = "SET, $\\\\tau = 0.5$",
    r2_stransition_l0 = "ST, $\\\\tau = 0$",
    r2_stransition_l05 = "ST, $\\\\tau = 0.5$",
    r2_sbreak_mid = "SB, mid",
    r2_sbreak_end = "SB, end"
  ),
  gg = c(
    r2_markov_symm_high = r"(MS, $p_{21} = 0.1$)",
    r2_markov_symm_low = r"(MS, $p_{21} = 0.7$)",
    r2_threshold_x_0 = r"(SET, $\tau = 0$)",
    r2_threshold_x_05 = r"(SET, $\tau = 0.5$)",
    r2_stransition_l0 = r"(ST, $\tau = 0$)",
    r2_stransition_l05 = r"(ST, $\tau = 0.5$)",
    r2_sbreak_mid = r"(SB, mid)",
    r2_sbreak_end = r"(SB, end)"
  ) |>
    map_chr(~ TeX(.x) %@% "plotmath")
)

n_t <- 120L # TODO: correct this temporary fix
#' RGPs' parameters
#' @export
params <- list(
  # Multinomial:
  r2_multinomial_equal = list3(
    n_r = 2, rgp = "markov", args = list(transmat_diag(0.5, n_r)),
    r_start = sample(1:2, 1)
  ),
  r2_multinomial_reg1 = list3(
    n_r = 2, rgp = "markov", args = list(transmat_main_col(0.7, n_r)),
    r_start = sample(1:2, 1)
  ),
  # Markov, symmetric:
  r2_markov_symm_high = list3(
    n_r = 2, rgp = "markov", args = list(transmat_diag(0.9, n_r)),
    r_start = sample(1:2, 1)
  ),
  r2_markov_symm_low = list3(
    n_r = 2, rgp = "markov", args = list(transmat_diag(0.6, n_r)),
    r_start = sample(1:2, 1)
  ),
  # Markov, asymmetric:
  r2_markov_asymm_high = list3(
    n_r = 2, rgp = "markov", args = list(transmat_main_col(c(0.9, 0.7), n_r)),
    r_start = sample(1:2, 1)
  ),
  r2_markov_asymm_low = list3(
    n_r = 2, rgp = "markov", args = list(transmat_main_col(c(0.8, 0.6), n_r)),
    r_start = sample(1:2, 1)
  ),
  # S-break:
  r2_sbreak_mid = list3(
    n_r = 2, rgp = "sbreak", args = list(c(as.integer(n_t / 2))),
    r_start = 1
  ),
  r2_sbreak_end = list3(
    n_r = 2, rgp = "sbreak", args = list(c(as.integer(n_t * 2 / 3))),
    r_start = 1
  ),
  # Threshold x:
  r2_threshold_x_0 = list3(
    n_r = 2, rgp = "threshold", args = list(c(0)),
    r_start = expr(fun(y, r, t_start))
  ),
  r2_threshold_x_05 = list3(
    n_r = 2, rgp = "threshold", args = list(c(0.5)),
    r_start = expr(fun(y, r, t_start))
  ),
  # Threshold |x|:
  r2_threshold_abs_05 = list3(
    n_r = 2, rgp = "threshold", args = list(c(0.5), g = \(y, t) abs(y[t - 1])),
    r_start = expr(fun(y, r, t_start))
  ),
  r2_threshold_abs_2 = list3(
    n_r = 2, rgp = "threshold", args = list(c(2), g = \(y, t) abs(y[t - 1])),
    r_start = expr(fun(y, r, t_start))
  ),
  # Threshold Dx:
  r2_threshold_diff_05 = list3(
    n_r = 2, rgp = "threshold", args = list(
      c(0.5), g = \(y, t) diff(y[(t-2):(t-1)])
    ),
    r_start = expr(fun(y, r, t_start))
  ),
  r2_threshold_diff_2 = list3(
    n_r = 2, rgp = "threshold", args = list(
      c(2), g = \(y, t) diff(y[(t-2):(t-1)])
    ),
    r_start = expr(fun(y, r, t_start))
  ),
  # LSTAR, ESTAR:
  r2_stransition_l0 = list3(
    n_r = 2, rgp = "stransition", args = list(
      c(0), g = \(y, t, breaks) 1 / (1 + exp(- (y[t-1] - breaks) / 1))
    ),
    r_start = expr(fun(y, r, t_start))
  ),
  r2_stransition_l05 = list3(
    n_r = 2, rgp = "stransition", args = list(
      c(0.5), g = \(y, t, breaks) 1 / (1 + exp(- (y[t-1] - breaks) / 1))
    ),
    r_start = expr(fun(y, r, t_start))
  ),
  r2_stransition_e0 = list3(
    n_r = 2, rgp = "stransition", args = list(
      c(0), g = \(y, t, breaks) 1 - exp(- (y[t-1] - breaks)^2 / 1)
    ),
    r_start = expr(fun(y, r, t_start))
  ),
  r2_stransition_e05 = list3(
    n_r = 2, rgp = "stransition", args = list(
      c(0.5), g = \(y, t, breaks) 1 - exp(- (y[t-1] - breaks)^2 / 1)
    ),
    r_start = expr(fun(y, r, t_start))
  )
)

#' RGP options
#' @export
options <- map(params, \(p) {
  list3(
    n_r = p$n_r,
    fun = inject(create_rgp[[p$rgp]](!!!p$args)),
    r_start = p$r_start
  )
})
