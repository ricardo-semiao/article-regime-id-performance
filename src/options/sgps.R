
# Setup ------------------------------------------------------------------------

# Loading dependencies:
box::use(
  src/utils[...],
  latex2exp[TeX],
  create_sgp = src/creators/sgps
)



# Helpers ----------------------------------------------------------------------

#' Interal: Create a unconditional SGP from conditional SGP and regime nature
#'
#' @param funs [`function(){}` or `list(function(){})`] Conditional SGPs.
#' @param args [`list(list())`] SGP parameters for each regime.
#'
#' @returns [`function(){}`] A new function combining the regimes.
unconditional_sgp <- function(funs, args) {
  if (!is_list(args) || !all(map_lgl(args, is_list))) {
    cli_abort("{.arg args} must be a list of lists.)")
  }

  n_r <- length(args)

  if (is_function(funs)) {
    funs <- map(seq_len(n_r), ~ funs)
  } else if (!is_list(funs) || length(funs) != n_r) {
    cli_abort("
    {.arg funs} must be a function or a list with {.code length(args)} \\
    ({n_r}) functions.
    ")
  }

  ys_expr <- map2(funs, args, \(f, arg) inject(f(!!!arg)))

  new_function(
    args = pairlist2(y = , r = , t = ),
    body = expr(sum(c(!!!ys_expr) * r[t, ])),
    env = pkg_env("base")
  )
}

#' Internal: Standardize SGP parameters by adding missing defaults
#'
#' Currently based on `create_sgp$ar` defaults, i.e. `mu = 0` and `sigma = 1`.
standardize_params_sgp <- function(args) {
  imap(args, \(arg, s) {
    arg <- if (!"mu" %in% names(arg)) c(mu = 0, arg[]) else arg
    arg <- if (!"sigma" %in% names(arg)) c(arg[], sigma = 1) else arg
    arg
  })
}



# Options ----------------------------------------------------------------------

#' SGPs' names dictionary
#' @export
dict <- list(
  gt = c(
    "r2_ar1_mu1" = r"($\mu ~ (0, 0.5)$)",
    "r2_ar1_mu2" = r"($\mu ~ (0, 1)$)",
    "r2_ar1_rho1" = r"($\rho_{1} ~ (0.1, 0.9)$)",
    "r2_ar1_rho2" = r"($\rho_{1} ~ (0.4, 0.6)$)",
    "r2_ar1_sigma1" = r"($\sigma ~ (1, 1.5)$)",
    "r2_ar1_sigma2" = r"($\sigma ~ (1, 2)$)"
  ),
  gg = c(
    r2_ar1_mu1 = r"($\mu$)",
    r2_ar1_mu2 = r"($\mu$)",
    r2_ar1_rho1 = r"($\rho_{1}$)",
    r2_ar1_rho2 = r"($\rho_{1}$)",
    r2_ar1_sigma1 = r"($\sigma$)",
    r2_ar1_sigma2 = r"($\sigma$)"
    # r2_ar1_mu1 = r"($\mu ~ (0, 0.5)$)",
    # r2_ar1_mu2 = r"($\mu ~ (0, 2)$)",
    # r2_ar1_rho1 = r"($\rho_{1} ~ (0.1, 0.9)$)",
    # r2_ar1_rho2 = r"($\rho_{1} ~ (0.4, 0.6)$)",
    # r2_ar1_sigma1 = r"($\sigma ~ (1, 2)$)",
    # r2_ar1_sigma2 = r"($\sigma ~ (1, 4)$)"
  ) |>
    map_chr(~ TeX(.x) %@% "plotmath")
)


# All regime natures are ordered by the changing parameter, with the first
# regime being the one with its smallest value

#' SGPs' parameters
#' @export
params <- list2(
  # AR mu:
  r2_ar1_mu1 = list3(
    n_r = 2, sgp = "ar",
    args = list(list(rho1 = 0.5), list(mu = 0.5, rho1 = 0.5))
  ),
  r2_ar1_mu2 = list3(
    n_r = 2, sgp = "ar",
    args = list(list(rho1 = 0.5), list(mu = 1, rho1 = 0.5))
  ),
  # AR rho:
  r2_ar1_rho1 = list3(
    n_r = 2, sgp = "ar",
    args = list(list(rho1 = 0.4), list(rho1 = 0.6))
  ),
  r2_ar1_rho2 = list3(
    n_r = 2, sgp = "ar",
    args = list(list(rho1 = 0.2), list(rho1 = 0.8))
  ),
  # AR sign:
  # r2_ar1_sign1 = list3(
  #   n_r = 2, sgp = "ar",
  #   args = list(list(rho1 = -0.3), list(rho1 = 0.3))
  # ),
  # r2_ar1_sign2 = list3(
  #   n_r = 2, sgp = "ar",
  #   args = list(list(rho1 = -0.7), list(rho1 = 0.7))
  # ),
  # AR new lag:
  # r2_ar2_pos1 = list3(
  #   n_r = 2, sgp = "ar",
  #   args = list(list(rho1 = 0.5), list(rho1 = 0.5, rho2 = 0.2))
  # ),
  # r2_ar2_pos2 = list3(
  #   n_r = 2, sgp = "ar",
  #   args = list(list(rho1 = 0.5), list(rho1 = 0.5, rho2 = 0.5))
  # ),
  # r2_ar2_neg1 = list3(
  #   n_r = 2, sgp = "ar",
  #   args = list(list(rho1 = 0.5, rho2 = -0.2), list(rho1 = 0.5))
  # ),
  # r2_ar2_neg2 = list3(
  #   n_r = 2, sgp = "ar",
  #   args = list(list(rho1 = 0.5, rho2 = -0.5), list(rho1 = 0.5))
  # ),
  # AR sigma:
  r2_ar1_sigma1 = list3(
    n_r = 2, sgp = "ar",
    args = list(list(rho1 = 0.5), list(rho1 = 0.5, sigma = 1.5))
  ),
  r2_ar1_sigma2 = list3(
    n_r = 2, sgp = "ar",
    args = list(list(rho1 = 0.5), list(rho1 = 0.5, sigma = 2))
  )
)

# Standardizing parameters (adding missing defaults)
params <- map(params, \(p) {
  p$args <- standardize_params_sgp(p$args)
  p
})


#' SGP options
#' @export
options <- map(params, \(p) {
  list3(
    t_cut = length(p$args) - 1,
    fun = unconditional_sgp(create_sgp[[p$sgp]], p$args)
  )
})
