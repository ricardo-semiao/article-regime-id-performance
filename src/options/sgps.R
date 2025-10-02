
# Setup ------------------------------------------------------------------------

# Loading dependencies
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
    env = global_env()
  )
}

#' Names dictionary
#' @export
options_names <- c(
  r2_ar1_mu1   = "Mean Change, Small Difference",
  r2_ar1_mu2   = "Mean Change, Big Difference",
  r2_ar1_rho1  = "Persistence Change, Big Difference",
  r2_ar1_rho2  = "Persistence Change, Small Difference",
  r2_ar1_sign1 = "Sign Switching, Small Difference",
  r2_ar1_sign2 = "Sign Switching, Big Difference",
  r2_ar2_pos1  = "New Lag Positive, Small",
  r2_ar2_pos2  = "New Lag Positive, Big",
  r2_ar2_neg1  = "New Lag Negative, Small",
  r2_ar2_neg2  = "New Lag Negative, Big",
  r2_ar1_vol1  = "Volatility Change, Small Difference",
  r2_ar1_vol2  = "Volatility Change, Big Difference"
)



# Options ----------------------------------------------------------------------

#' SGP options
#' @export
options <- list()


# 2 regimes, mu change, small difference
options$r2_ar1_mu1 <- list3(
  t_cut = 1,
  fun = unconditional_sgp(
    create_sgp$ar,
    list(
      list(mu = 0, rho1 = 0.5),
      list(mu = 0.5, rho1 = 0.5)
    )
  )
)

# 2 regimes, mu change, big difference
options$r2_ar1_mu2 <- list3(
  t_cut = 1,
  fun = unconditional_sgp(
    create_sgp$ar,
    list(
      list(mu = 0, rho1 = 0.5),
      list(mu = 2, rho1 = 0.5)
    )
  )
)


# 2 regimes, persistence change, big difference
options$r2_ar1_rho1 <- list3(
  t_cut = 1,
  fun = unconditional_sgp(
    create_sgp$ar,
    list(
      list(mu = 0, rho1 = 0.9),
      list(mu = 0, rho1 = 0.1)
    )
  )
)

# 2 regimes, persistence change, small difference
options$r2_ar1_rho2 <- list3(
  t_cut = 1,
  fun = unconditional_sgp(
    create_sgp$ar,
    list(
      list(mu = 0, rho1 = 0.6),
      list(mu = 0, rho1 = 0.4)
    )
  )
)


# 2 regimes, sign switching, small difference
options$r2_ar1_sign1 <- list3(
  t_cut = 1,
  fun = unconditional_sgp(
    create_sgp$ar,
    list(
      list(mu = 0, rho1 = 0.3),
      list(mu = 0, rho1 = -0.3)
    )
  )
)

# 2 regimes, sign switching, big difference
options$r2_ar1_sign2 <- list3(
  t_cut = 1,
  fun = unconditional_sgp(
    create_sgp$ar,
    list(
      list(mu = 0, rho1 = 0.7),
      list(mu = 0, rho1 = -0.7)
    )
  )
)


# 2 regimes, new lag, positive, small
options$r2_ar2_pos1 <- list3(
  t_cut = 1,
  fun = unconditional_sgp(
    create_sgp$ar,
    list(
      list(mu = 0, rho1 = 0.6),
      list(mu = 0, rho1 = 0.6, rho2 = 0.2)
    )
  )
)

# 2 regimes, new lag, positive, big
options$r2_ar2_pos2 <- list3(
  t_cut = 1,
  fun = unconditional_sgp(
    create_sgp$ar,
    list(
      list(mu = 0, rho1 = 0.6),
      list(mu = 0, rho1 = 0.6, rho2 = 0.5)
    )
  )
)


# 2 regimes, new lag, negative, small
options$r2_ar2_neg1 <- list3(
  t_cut = 1,
  fun = unconditional_sgp(
    create_sgp$ar,
    list(
      list(mu = 0, rho1 = 0.6),
      list(mu = 0, rho1 = 0.6, rho2 = -0.2)
    )
  )
)

# 2 regimes, new lag, negative, big
options$r2_ar2_neg2 <- list3(
  t_cut = 1,
  fun = unconditional_sgp(
    create_sgp$ar,
    list(
      list(mu = 0, rho1 = 0.6),
      list(mu = 0, rho1 = 0.6, rho2 = -0.5)
    )
  )
)


# 2 regimes, vol change, small difference
options$r2_ar1_vol1 <- list3(
  t_cut = 1,
  fun = unconditional_sgp(
    create_sgp$ar,
    list(
      list(mu = 0, rho1 = 0.6, vol = 1),
      list(mu = 0, rho1 = 0.6, vol = 2)
    )
  )
)

# 2 regimes, vol change, big difference
options$r2_ar1_vol2 <- list3(
  t_cut = 1,
  fun = unconditional_sgp(
    create_sgp$ar,
    list(
      list(mu = 0, rho1 = 0.6, vol = 1),
      list(mu = 0, rho1 = 0.6, vol = 4)
    )
  )
)
