
# Setup ------------------------------------------------------------------------

# Loading dependencies:
box::use(
  src/utils[...],
  latex2exp[TeX],
  create_rgp = src/creators/rgps,
  src/parameters[n_t]
)

# ! SGPs, RGPs, and models names must not contain hiphens



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
dict <- list3(
  gt_param = c(
    r1_no_rs = r"(No RS)",
    r2_markov_symm_high = r"(MS, $p_{21} = 0.1$)",
    r2_markov_asymm_high = r"(MS, $p_{21} = 0.3$)",
    r2_threshold_symm_x = r"(SET, $\tau = 0.5$)",
    r2_threshold_asymm_x = r"(SET, $\tau = 0.9$)",
    r2_stransition_symm_l = r"(ST, $\tau = 0.5$)",
    r2_stransition_asymm_l = r"(ST, $\tau = 0.9$)",
    r2_sbreak_symm = r"(SB, mid)",
    r2_sbreak_asymm = r"(SB, end)"
  ),
  gt = map_chr(gt_param, ~ str_replace(.x, "([^,]+),?.*$", "\\1")), # Map to keep names
  gg_param = map_chr(gt_param, ~ TeX(.x) %@% "plotmath"),
  gg = map_chr(gt, ~ TeX(.x) %@% "plotmath")
)


#' RGPs' parameters
#' @export
params <- list2(
  # No-RS AR:
  r1_no_rs = list3(
    n_r = 1, rgp = "no_rs", r_start = 1
  ),
  # Multinomial:
  # r2_multinomial_equal = list3(
  #   n_r = 2, rgp = "markov", args = list(transmat_diag(0.5, n_r)),
  #   r_start = sample(1:2, 1)
  # ),
  # r2_multinomial_reg1 = list3(
  #   n_r = 2, rgp = "markov", args = list(transmat_main_col(0.7, n_r)),
  #   r_start = sample(1:2, 1)
  # ),
  # Markov, high persistence:
  r2_markov_symm_high = list3(
    n_r = 2, rgp = "markov", args = list(transmat_diag(0.9, n_r)),
    r_start = sample(1:2, 1)
  ),
  r2_markov_asymm_high = list3(
    n_r = 2, rgp = "markov", args = list(transmat_main_col(c(0.9, 0.3), n_r)),
    r_start = sample(1:2, 1)
  ),
  # Markov, low persistence:
  # r2_markov_symm_low = list3(
  #   n_r = 2, rgp = "markov", args = list(transmat_diag(0.6, n_r)),
  #   r_start = sample(1:2, 1)
  # ),
  # r2_markov_asymm_low = list3(
  #   n_r = 2, rgp = "markov", args = list(transmat_main_col(c(0.8, 0.6), n_r)),
  #   r_start = sample(1:2, 1)
  # ),
  # S-break:
  # r2_sbreak_symm = list3(
  #   n_r = 2, rgp = "sbreak", args = list(c(as.integer(n_t / 2))),
  #   r_start = 1
  # ),
  # r2_sbreak_asymm = list3(
  #   n_r = 2, rgp = "sbreak", args = list(c(as.integer(n_t * 2 / 3))),
  #   r_start = 1
  # ),
  # Threshold x:
  r2_threshold_symm_x = list3(
    n_r = 2, rgp = "threshold", args = list(c(0.5)),
    r_start = expr(fun(y, r, t_start))
  ),
  r2_threshold_asymm_x = list3(
    n_r = 2, rgp = "threshold", args = list(c(0.9)),
    r_start = expr(fun(y, r, t_start))
  ),
  # Threshold |x|:
  # r2_threshold_symm_abs = list3(
  #   n_r = 2, rgp = "threshold", args = list(c(0.5), g = \(y, t) abs(y[t - 1])),
  #   r_start = expr(fun(y, r, t_start))
  # ),
  # r2_threshold_asymm_abs = list3(
  #   n_r = 2, rgp = "threshold", args = list(c(2), g = \(y, t) abs(y[t - 1])),
  #   r_start = expr(fun(y, r, t_start))
  # ),
  # Threshold Dx:
  # r2_threshold_symm_diff = list3(
  #   n_r = 2, rgp = "threshold", args = list(
  #     c(0.5), g = \(y, t) diff(y[(t-2):(t-1)])
  #   ),
  #   r_start = expr(fun(y, r, t_start))
  # ),
  # r2_threshold_asymm_diff = list3(
  #   n_r = 2, rgp = "threshold", args = list(
  #     c(2), g = \(y, t) diff(y[(t-2):(t-1)])
  #   ),
  #   r_start = expr(fun(y, r, t_start))
  # ),
  # LSTAR, ESTAR:
  r2_stransition_symm_l = list3(
    n_r = 2, rgp = "stransition", args = list(
      c(0.5), g = \(y, t, breaks) 1 / (1 + exp(- (y[t-1] - breaks) / 1))
    ),
    r_start = expr(fun(y, r, t_start))
  ),
  r2_stransition_asymm_l = list3(
    n_r = 2, rgp = "stransition", args = list(
      c(0.9), g = \(y, t, breaks) 1 / (1 + exp(- (y[t-1] - breaks) / 1))
    ),
    r_start = expr(fun(y, r, t_start))
  ),
  # r2_stransition_symm_e = list3(
  #   n_r = 2, rgp = "stransition", args = list(
  #     c(0), g = \(y, t, breaks) 1 - exp(- (y[t-1] - breaks)^2 / 1)
  #   ),
  #   r_start = expr(fun(y, r, t_start))
  # ),
  # r2_stransition_asymm_e = list3(
  #   n_r = 2, rgp = "stransition", args = list(
  #     c(0.5), g = \(y, t, breaks) 1 - exp(- (y[t-1] - breaks)^2 / 1)
  #   ),
  #   r_start = expr(fun(y, r, t_start))
  # )
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
