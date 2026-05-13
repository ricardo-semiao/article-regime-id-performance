
box::use(
  src/utils[...],
  ./models,
  ./rgps,
  ./sgps,
  ./metrics,
  gt[md],
  latex2exp[TeX]
)


dict_params <- list3(
  gt_s = c(
    mu = r"($\mu^s$)", rho1 = r"($\rho^s_1$)", sigma = r"($\sigma^s$)"
  ),
  gt = c(
    mu = r"($\mu$)", rho1 = r"($\rho_1$)", sigma = r"($\sigma$)"
  ),
  gg = map_chr(gt, \(x) attr(TeX(x), "plotmath"))
)

dicts_reg <- c(
  #"Constant" = "Constant",
  "(Intercept)" = "Constant",
  #
  "poly(sim, 3)1" = "$i$",
  "poly(sim, 3)2" = "$i^2$",
  "poly(sim, 3)3" = "$i^3$",
  "log(sim)" = "$\\log(i)$",
  #
  models$dict$gt %>% set_names(paste0("model", names(.))),
  regimes_bme = "$BME(r)$",
  switches_diff = "$\\Delta \\text{Switches}$",
  duration_diff = "$\\Delta \\text{Duration}$",
  r2 = "$R^2$",
  mu_diff = "$\\Delta \\mu$",
  rho1_diff = "$\\Delta \\rho_1$",
  sigma_diff = "$\\Delta \\sigma$",
  avg_diff = "$\\Delta d(\\hat{\\mu}(.))$",
  acf_diff = "$\\Delta d(\\hat{\\rho}_1(.))$",
  sd_diff = "$\\Delta d(\\hat{\\sigma}(.))$",
  #
  is_mis = "Baseline",
  models$dict$gt %>% {set_names(
    paste0("Model: ", .),
    paste0("is_mis:model", names(.))
  )},
  rgps$dict$gt %>% {set_names(
    paste0("RGP: ", .),
   paste0("is_mis:rgp", str_replace(names(.), "(r[0-9]+_[^_]+)_.+", "\\1"))
  )},
  sgps$dict$gt %>% {set_names(
    paste0("RN: ", .),
    paste0("is_mis:sgp", str_replace(names(.), "(.+)[0-9]+", "\\1"))
  )},
  "is_mis:sgp1" = "RN: small",
  "is_mis:sgp2" = "RN: big",
  "is_mis:rgpasymm" = "RGP: asym.",
  "is_mis:rgpsymm" = "RGP: sym.",
  #
  models$dict$gt %>% {set_names(
    paste0("$R^2 ~ \\cdot$ ", .),
    paste0("model", names(.), ":r2")
  )},
  models$dict$gt %>% {set_names(
    paste0("$BME(r) ~ \\cdot$ ", .),
    paste0("model", names(.), ":regimes_bme")
  )},
  #
  "model_r != dgp_rTRUE" = "$\\hat{S} \\neq S$",
  "model_r > dgp_rTRUE" = "$\\hat{S} > S$",
  "model_r < dgp_rTRUE" = "$\\hat{S} < S$",
  "model_r > dgp_rTRUE:avg_sim" = "$\\hat{S} > S$ : $d(\\hat{\\mu}(.))$",
  "model_r > dgp_rTRUE:acf_sim" = "$\\hat{S} > S$ : $d(\\hat{\\rho}_1(.))$",
  "model_r > dgp_rTRUE:sd_sim" = "$\\hat{S} > S$ : $d(\\hat{\\sigma}(.))$",
  "model_r < dgp_rTRUE:avg_sim" = "$\\hat{S} < S$ : $d(\\hat{\\mu}(.))$",
  "model_r < dgp_rTRUE:acf_sim" = "$\\hat{S} < S$ : $d(\\hat{\\rho}_1(.))$",
  "model_r < dgp_rTRUE:sd_sim" = "$\\hat{S} < S$ : $d(\\hat{\\sigma}(.))$",
  #
  "as.integer(model_r > dgp_r):modelr1_nors" = "$\\hat{S} > S$ : No RS",
  "as.integer(model_r > dgp_r):modelr2_ms" = "$\\hat{S} > S$ : MS",
  "as.integer(model_r > dgp_r):modelr2_set" = "$\\hat{S} > S$ : SET",
  "as.integer(model_r > dgp_r):modelr2_st" = "$\\hat{S} > S$ : ST",
  "as.integer(model_r > dgp_r):modelr2_km" = "$\\hat{S} > S$ : KM"
)

#' Names' dictionaries for each option
#' @export
dicts <- list(
  models = models$dict,
  rgps = rgps$dict,
  sgps = sgps$dict,
  metrics = metrics$dict,
  params = dict_params,
  reg = dicts_reg
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
  sgps = sgps$options,
  metrics = metrics$get_metrics_data
)

#' Groups of options
#' @export
groups <- list(
  rgp_sym = c("r2_ms_symm_high", "r2_set_symm_x", "r2_st_symm_l"),
  rgp_asymm = c("r2_ms_asymm_high", "r2_set_asymm_x", "r2_st_asymm_l"),
  sgp_big = c("r2_ar1_mu2", "r2_ar1_rho2", "r2_ar1_sigma2"),
  sgp_small = c("r2_ar1_mu1", "r2_ar1_rho1", "r2_ar1_sigma1")
)
