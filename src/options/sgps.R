
# Setup ------------------------------------------------------------------------

# Loading dependencies:
box::use(
  src/utils[...],
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
#' Currently based on `create_sgp$ar` defaults, i.e. `mu = 0` and `vol = 1`.
standardize_params_sgp <- function(args) {
  imap(args, \(arg, s) {
    arg <- if (!"mu" %in% names(arg)) c(mu = 0, arg[]) else arg
    arg <- if (!"vol" %in% names(arg)) c(arg[], vol = 1) else arg
    arg
  })
}



# Options ----------------------------------------------------------------------

#' SGPs' names dictionary
#' @export
dict <- c(
  r2_ar1_mu1   = "\u03BC (small change)",
  r2_ar1_mu2   = "\u03BC (big change)",
  r2_ar1_rho1  = "\u03C1 (small change)",
  r2_ar1_rho2  = "\u03C1 (big change)",
  r2_ar1_sign1 = "sign(\u03C1) (small change)",
  r2_ar1_sign2 = "sign(\u03C1) (big change)",
  r2_ar2_pos1  = "New positive lag (small)",
  r2_ar2_pos2  = "New positive lag (big)",
  r2_ar2_neg1  = "New negative lag (small)",
  r2_ar2_neg2  = "New negative lag (big)",
  r2_ar1_vol1  = "\u03C3 (small change)",
  r2_ar1_vol2  = "\u03C3 (big change)"
)


# All regime natures are ordered by the changing parameter, with the first
# regime being the one with its smallest value

#' SGPs' parameters
#' @export
params <- list(
  # AR mu:
  r2_ar1_mu1 = list3(
    n_r = 2, sgp = "ar",
    args = list(list(rho1 = 0.5), list(mu = 0.5, rho1 = 0.5))
  ),
  r2_ar1_mu2 = list3(
    n_r = 2, sgp = "ar",
    args = list(list(rho1 = 0.5), list(mu = 2, rho1 = 0.5))
  ),
  # AR rho:
  r2_ar1_rho1 = list3(
    n_r = 2, sgp = "ar",
    args = list(list(rho1 = 0.1), list(rho1 = 0.9))
  ),
  r2_ar1_rho2 = list3(
    n_r = 2, sgp = "ar",
    args = list(list(rho1 = 0.4), list(rho1 = 0.6))
  ),
  # AR sign:
  r2_ar1_sign1 = list3(
    n_r = 2, sgp = "ar",
    args = list(list(rho1 = -0.3), list(rho1 = 0.3))
  ),
  r2_ar1_sign2 = list3(
    n_r = 2, sgp = "ar",
    args = list(list(rho1 = -0.7), list(rho1 = 0.7))
  ),
  # AR new lag:
  r2_ar2_pos1 = list3(
    n_r = 2, sgp = "ar",
    args = list(list(rho1 = 0.5), list(rho1 = 0.5, rho2 = 0.2))
  ),
  r2_ar2_pos2 = list3(
    n_r = 2, sgp = "ar",
    args = list(list(rho1 = 0.5), list(rho1 = 0.5, rho2 = 0.5))
  ),
  r2_ar2_neg1 = list3(
    n_r = 2, sgp = "ar",
    args = list(list(rho1 = 0.5, rho2 = -0.2), list(rho1 = 0.5))
  ),
  r2_ar2_neg2 = list3(
    n_r = 2, sgp = "ar",
    args = list(list(rho1 = 0.5, rho2 = -0.5), list(rho1 = 0.5))
  ),
  # AR vol:
  r2_ar1_vol1 = list3(
    n_r = 2, sgp = "ar",
    args = list(list(rho1 = 0.5), list(rho1 = 0.5, vol = 2))
  ),
  r2_ar1_vol2 = list3(
    n_r = 2, sgp = "ar",
    args = list(list(rho1 = 0.5), list(rho1 = 0.5, vol = 4))
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
