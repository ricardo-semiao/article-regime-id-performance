
box::use(
  src/utils[...],
  ./models,
  ./rgps,
  ./sgps,
  gt[md],
  latex2exp[TeX]
)


dict_metrics <- list3(
  cond_gt = c(
    avg = r"($\hat{\mu}(. | s)$)",
    acf = r"($\hat{\rho}_1(. | s)$)",
    sd = r"($\hat{\sigma}(. | s)$)"
  ),
  disp_gt = c(
    avg = r"($d(\hat{\mu}(.))$)",
    acf = r"($d(\hat{\rho}_1(.))$)",
    sd = r"($d(\hat{\sigma}(.))$)"
  ),
  disp_gg = map_chr(disp_gt, \(x) attr(TeX(x), "plotmath"))
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
  switches_diff = "$\\Delta \\text{switches}$",
  duration_diff = "$\\Delta \\text{duration}$",
  r2 = "$R^2$",
  mu_diff = "$\\Delta \\mu$",
  rho1_diff = "$\\Delta \\rho_1$",
  sigma_diff = "$\\Delta \\sigma$",
  avg_diff = "$\\Delta \\text{avg}(.)$",
  acf_diff = "$\\Delta \\text{acf}(.)$",
  sd_diff = "$\\Delta \\text{sd}(.)$",
  #
  is_misTRUE = "Is misspecified"
)

#' Names' dictionaries for each option
#' @export
dicts <- list(
  models = models$dict,
  rgps = rgps$dict,
  sgps = sgps$dict,
  metrics = dict_metrics,
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
  sgps = sgps$options
)

#' Groups of options
#' @export
groups <- list(
  rgp_sym = c("r2_ms_symm_high", "r2_set_symm_x", "r2_st_symm_l"),
  rgp_asymm = c("r2_ms_asymm_high", "r2_set_asymm_x", "r2_st_asymm_l"),
  sgp_big = c("r2_ar1_mu2", "r2_ar1_rho2", "r2_ar1_sigma2"),
  sgp_small = c("r2_ar1_mu1", "r2_ar1_rho1", "r2_ar1_sigma1")
)
