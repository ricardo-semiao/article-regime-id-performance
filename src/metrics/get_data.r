
# Setup ------------------------------------------------------------------------

# Loading dependencies:
box::use(
  src/utils[...],
  src/utils2[...],
  ./metrics,
)

box::use(
  dtplyr[lazy_dt],
  mirai[mirai_map, mirai_collect = collect_mirai, mirai_daemons = daemons]
)



# Getting Metrics Data ---------------------------------------------------------

#' Todo: document
get_models_metrics <- function(data, n_t, n_h) {
  lazy_dt(data) |>
    group_by(sgp, rgp, sim, model, arrange = FALSE) |>
    summarise(
      avg_est = metrics$analytical_average(meta_est[[1]]$coefs) |> metrics$diff_p(),
      avg_true = metrics$analytical_average(meta_true[[1]]$coefs) |> metrics$diff_p(),
      acf_est = metrics$analytical_autocorr(meta_est[[1]]$coefs) |> metrics$diff_p(),
      acf_true = metrics$analytical_autocorr(meta_true[[1]]$coefs) |> metrics$diff_p(),
      vol_est = metrics$analytical_volatility(meta_est[[1]]$coefs) |> metrics$diff_p(),
      vol_true = metrics$analytical_volatility(meta_true[[1]]$coefs) |> metrics$diff_p(),
      mu_est = meta_est[[1]]$coefs["mu", 1:2] |> metrics$diff_p(),
      mu_true = meta_true[[1]]$coefs["mu", 1:2] |> metrics$diff_p(),
      rho1_est = meta_est[[1]]$coefs["rho1", 1:2] |> metrics$diff_p(),
      rho1_true = meta_true[[1]]$coefs["rho1", 1:2] |> metrics$diff_p(),
      sigma_est = meta_est[[1]]$coefs["vol", 1:2] |> metrics$diff_p(),
      sigma_true = meta_true[[1]]$coefs["vol", 1:2] |> metrics$diff_p()
    ) |>
    ungroup() |>
    as_tibble() |>
    mutate(
      avg_diff = abs(avg_est - avg_true),
      acf_diff = abs(acf_est - acf_true),
      vol_diff = abs(vol_est - vol_true),
      mu_diff = abs(mu_est - mu_true),
      rho1_diff = abs(rho1_est - rho1_true),
      sigma_diff = abs(sigma_est - sigma_true)
    )
}
# Todo: calculate the true ones after, in a separate group only by sgp-rgp-sim.
# Do a benchmark to see which is faster.

#' Todo: document
get_regimes_metrics <- function(data, n_t, n_h) {
  lazy_dt(data) |>
    group_by(sgp, rgp, sim, model, arrange = FALSE) |>
    summarise(
      rmse = metrics$performance_rmse(y_est, y_true, n_h, n_t, t = t),
      mape = metrics$performance_mape(y_est, y_true, n_h, n_t, t = t),
      r2 = metrics$performance_r2(y_est, y_true, n_h, n_t, t = t),
      regimes_me = sum(r_est != r_true) / n_t,
      switches_est = metrics$average_switches(y_est, r_est),
      duration_est = metrics$duration_diff(y_est, r_est),
      switches_true = metrics$average_switches(y_true, r_true),
      duration_true = metrics$duration_diff(y_true, r_true)
    ) |>
    ungroup() |>
    as_tibble() |>
    mutate(
      switches_diff = abs(switches_est - switches_true),
      duration_diff = abs(duration_est - duration_true)
    )
}

#' Todo: document
#' @export
get_data_final <- function(
  data_s, data_e, data_sm, data_em,
  n_t, n_warm, n_h, model_names
) {
  cat("Getting model metrics...\n")
  metrics_model <- left_join(
    rename(data_em, meta_est = meta),
    rename(data_sm, meta_true = meta),
    by = c("sgp", "rgp", "sim")
  ) |>
    #filter(sgp %in% unique(sgp)[1:2], rgp %in% unique(rgp)[1:2], model %in% unique(model)[1:2]) |>
    arrange(sgp, rgp, model, sim) |>
    get_models_metrics(n_t = n_t, n_h = n_h)

  cat("Getting regime metrics...\n")
  metrics_regimes <- left_join(
    rename(data_e, y_est = y, r_est = r),
    rename(data_s, y_true = y, r_true = r),
    by = c("sgp", "rgp", "sim", "t")
  ) |>
    filter(t > n_warm) |>
    #filter(sgp %in% unique(sgp)[1:2], rgp %in% unique(rgp)[1:2], model %in% unique(model)[1:2]) |>
    arrange(sgp, rgp, model, sim, t) |>
    get_regimes_metrics(n_t = n_t, n_h = n_h)

  left_join(
    metrics_regimes, metrics_model,
    by = c("sgp", "rgp", "sim", "model")
  )
}
