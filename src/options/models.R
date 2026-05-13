
# Setup ------------------------------------------------------------------------

# Loading dependencies:
box::use(
  src/utils[...],
  create_model = src/creators/models,
  latex2exp[TeX]
)

# ! SGPs, RGPs, and models names must not contain hiphens



# Options ----------------------------------------------------------------------

#' Models' names dictionary
#' @export
dict <- list3(
  gt = c(
    r1_nors = "No RS",
    r2_ms = "MS",
    r2_sbreak = "SB",
    r2_set = "SET",
    r2_st = "ST",
    r2_km = "KM",
    r1_rf = "RF"
  ),
  gg = map_chr(gt, ~ TeX(.x) %@% "plotmath")
)

#' Models' parameters
#' @export
params <- list(
  r2_sb = list3(
    n_r = 2, rgp = "sb", args = list(n_r = n_r)
  ),
  r1_nors = list3(
    n_r = 1, rgp = "nors", args = list(n_r = n_r)
  ),
  r2_set = list3(
    n_r = 2, rgp = "set", args = list(n_r = n_r)
  ),
  r2_set_abs = list3(
    n_r = 2, rgp = "set", args = list(n_r = n_r, g = \(y) abs(y))
  ),
  r2_set_diff = list3(
    n_r = 2, rgp = "set", args = list(n_r = n_r, g = \(y) diff(y))
  ),
  r2_st = list3(
    n_r = 2, rgp = "st", args = list(n_r = n_r)
  ),
  r2_ms = list3(
    n_r = 2, rgp = "ms", args = list(n_r = n_r)
  ),
  r2_km = list3(
    n_r = 2, rgp = "km", args = list(n_r = n_r, n_l_r = 4)
  ),
  r1_rf = list3( # TODO: create concept of no regimes
    n_r = 1, rgp = "rf", args = list(n_r = n_r, n_l_r = 3, ntree = 50, mtry = 1)
  )
)

#' Model options
#' @export
options <- map(params, \(p) {
  inject(create_model[[p$rgp]](!!!p$args))
})
