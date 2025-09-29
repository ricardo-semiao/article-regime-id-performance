
# Setup ------------------------------------------------------------------------

# Loading dependencies
box::use(
  ../utils[...],
  create_model = ../creators/models
)



# Helpers ----------------------------------------------------------------------

#' Names dictionary
#' @export
options_names <- c(
  sbreak = "Structural Breaks",
  threshold = "Threshold",
  threshold_abs = "Absolute Threshold",
  smooth_threshold = "Smooth Transition",
  markov = "Markov Switching"
)



# Options ----------------------------------------------------------------------

#' Model options
#' @export
options <- list()


# 2 Regimes
options$r2_sbreak <- create_model$sbreak(2)
options$r2_threshold_x <- create_model$threshold(2)
options$r2_threshold_abs <- create_model$threshold(2, g = abs)
options$r2_threshold_diff <- create_model$threshold(2, g = diff)
options$r2_smooth_threshold <- create_model$smooth_threshold(2)
options$r2_markov <- create_model$markov(2)
