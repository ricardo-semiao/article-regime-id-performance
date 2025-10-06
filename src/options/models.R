
# Setup ------------------------------------------------------------------------

# Loading dependencies:
box::use(
  src/utils[...],
  create_model = src/creators/models
)



# Options ----------------------------------------------------------------------

#' Models' names dictionary
#' @export
dict <- c(
  r2_sbreak = "Structural Breaks",
  r2_threshold_x = "Threshold x",
  r2_threshold_abs = "Threshold |x|",
  r2_threshold_abs = "Threshold Dx",
  r2_stransition = "Smooth Transition",
  r2_markov = "Markov Switching"
)

#' Models' parameters
#' @export
params <- list(
  r2_sbreak = list3(
    n_r = 2, rgp = "sbreak", args = list(n_r = n_r)
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
