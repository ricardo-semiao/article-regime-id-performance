
box::use(
  ./models,
  ./rgps,
  ./sgps
)

#' Names' dictionaries for each option
#' @export
dicts <- list(
  models = models$dict,
  rgps = rgps$dict,
  sgps = sgps$dict
)

#' Parameters for each option
#' @export
params <- list(
  models = models$params,
  rgps = rgps$params,
  sgps = sgps$params
)

#' Functions for each option
#' @export
options <- list(
  models = models$options,
  rgps = rgps$options,
  sgps = sgps$options
)
