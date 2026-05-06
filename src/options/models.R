
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
    r1_no_rs = "No RS",
    r2_markov = "MS",
    r2_threshold_x = "SET",
    r2_stransition = "ST",
    r2_sbreak = "SB"
  ),
  gg = map_chr(gt, ~ TeX(.x) %@% "plotmath")
)

#' Models' parameters
#' @export
params <- list(
  # r2_sbreak = list3(
  #   n_r = 2, rgp = "sbreak", args = list(n_r = n_r)
  # ),
  r1_no_rs = list3(
    n_r = 1, rgp = "ar", args = list(n_r = n_r)
  ),
  r2_threshold_x = list3(
    n_r = 2, rgp = "threshold", args = list(n_r = n_r)
  ),
  r2_threshold_abs = list3(
    n_r = 2, rgp = "threshold", args = list(n_r = n_r, g = \(y) abs(y))
  ),
  r2_threshold_diff = list3(
    n_r = 2, rgp = "threshold", args = list(n_r = n_r, g = \(y) diff(y))
  ),
  r2_stransition = list3(
    n_r = 2, rgp = "stransition", args = list(n_r = n_r)
  ),
  r2_markov = list3(
    n_r = 2, rgp = "markov", args = list(n_r = n_r)
  )
)

#' Model options
#' @export
options <- map(params, \(p) {
  inject(create_model[[p$rgp]](!!!p$args))
})
