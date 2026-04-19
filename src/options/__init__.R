
box::use(
  ./models,
  ./rgps,
  ./sgps,
  gt[md],
  latex2exp[TeX]
)


# NOTE: based on n_r = 2, can be expanded
dict_regimes <- list(
  metrics = list(
    avg_1 = md("$s = 1$"), avg_2 = md("$s = 2$"), avg_0 = md("$\\perp s$"),
    acf_1 = md("$s = 1$"), acf_2 = md("$s = 2$"), acf_0 = md("$\\perp s$"),
    sd_1 =  md("$s = 1$"), sd_2 =  md("$s = 2$"), sd_0 =  md("$\\perp s$")
  ),
  coefs = list(
    r1_mu = md("$s = 1$"), r2_mu = md("$s = 2$"),
    r1_rho1 = md("$s = 1$"), r2_rho1 = md("$s = 2$"),
    r1_sigma =  md("$s = 1$"), r2_sigma =  md("$s = 2$")
  )
)

dict_metrics <- list(
  gt = list(
    avg = md("$\\hat{\\mu}(.)$"),
    acf = md("$\\hat{\\rho}_1(.)$"),
    sd = md("$\\hat{\\sigma}(.)$")
  ),
  gg = list(
    avg = "$\\hat{\\mu}(.)$",
    acf = "$\\hat{\\rho}_1(.)$",
    sd = "$\\hat{\\sigma}(.)$"
  ) |>
    vapply(\(x) attr(TeX(x), "plotmath"), character(1))
)

#' Names' dictionaries for each option
#' @export
dicts <- list(
  models = models$dict,
  rgps = rgps$dict,
  sgps = sgps$dict,
  regimes = dict_regimes,
  metrics = dict_metrics
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
