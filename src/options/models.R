
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
    r2_set_x = "SET",
    r2_st = "ST"
  ),
  gg = map_chr(gt, ~ TeX(.x) %@% "plotmath")
)

#' Models' parameters
#' @export
params <- list(
  # r2_sbreak = list3(
  #   n_r = 2, rgp = "sbreak", args = list(n_r = n_r)
  # ),
  r1_nors = list3(
    n_r = 1, rgp = "ar", args = list(n_r = n_r)
  ),
  r2_set_x = list3(
    n_r = 2, rgp = "threshold", args = list(n_r = n_r)
  ),
  r2_set_abs = list3(
    n_r = 2, rgp = "threshold", args = list(n_r = n_r, g = \(y) abs(y))
  ),
  r2_set_diff = list3(
    n_r = 2, rgp = "threshold", args = list(n_r = n_r, g = \(y) diff(y))
  ),
  r2_st = list3(
    n_r = 2, rgp = "stransition", args = list(n_r = n_r)
  ),
  r2_ms = list3(
    n_r = 2, rgp = "markov", args = list(n_r = n_r)
  )
)

#' Model options
#' @export
options <- map(params, \(p) {
  inject(create_model[[p$rgp]](!!!p$args))
})
