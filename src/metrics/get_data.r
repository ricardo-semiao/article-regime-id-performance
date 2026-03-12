
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
      rmse = metrics$performance_rmse(y_est, y_true, n_h, n_t, t = t, na.rm = TRUE),
      mape = metrics$performance_mape(y_est, y_true, n_h, n_t, t = t, na.rm = TRUE),
      sgp_metric_est = metrics$sgp_metric(sgp[1], y_est, r_est) |> sd(),
      rgp_metric_est = metrics$rgp_metric(rgp[1], y_est, r_est) |> sd(),
      sgp_metric_true = metrics$sgp_metric(sgp[1], y_true, r_true) |> sd(),
      rgp_metric_true = metrics$rgp_metric(rgp[1], y_true, r_true) |> sd()
    ) |>
    ungroup() |>
    mutate(
      sgp_metric_diff = abs(sgp_metric_est - sgp_metric_true),
      rgp_metric_diff = abs(rgp_metric_est - rgp_metric_true)
    ) |>
    as_tibble()
}
# Todo: calculate the true ones after, in a separate group only by sgp-rgp-sim.
# Do a benchmark to see which is faster.

#' Todo: document
get_regimes_metrics <- function(data, n_h) {
  lazy_dt(data) |>
  group_by(sgp, rgp, sim, model, r_est, arrange = FALSE) |>
  summarise(
    rmse = metrics$performance_rmse(y_est, y_true, n_h, n_t, t = t),
    #mape = metrics$performance_mape(y_est, y_true, n_h, n_t, t = t),
    sgp_metric_est = metrics$sgp_metric(sgp[1], y_est, r_est[1], 2)[r_est[1]],
    sgp_metric_true = metrics$sgp_metric(sgp[1], y_true, r_est[1], 2)[r_est[1]] # Todo: correct
  ) |>
  ungroup() |>
  mutate(
    sgp_metric_diff = abs(sgp_metric_est - sgp_metric_true)
  ) |>
    as_tibble()
}

#' Todo: document
#' @export
get_data_final <- function(
  data_s, data_e, data_m, type = "models",
  n_t, n_warm, n_h, model_names
) {
  cat("Getting metrics: joining true and estimated data...\n")
  data_base <- left_join(
    rename(data_e, y_est = y, r_est = r),
    rename(data_s, y_true = y, r_true = r),
    by = c("sgp", "rgp", "sim", "t")
  ) |>
    filter(t > n_warm) |>
    arrange(sgp, rgp, model, sim, t)

  cat("Getting metrics: calculating metrics...\n")
  get_metrics_f <- switch(type,
    "models" = get_models_metrics,
    "regimes" = get_regimes_metrics
  )
  data_metrics <- get_metrics_f(data_base, n_t = n_t, n_h = n_h)

  cat("Getting metrics: joining true and estimated parameters...\n")
  data_meta <- data_m %>%
    unnest_wider(meta) %>%
    rowwise() %>%
    mutate(as.data.frame(map(asplit(coefs, 1), sd)))
  # Todo: get more meta, specially rgp-related meta

  data_params <- get_correct_params(model_names, FALSE) |>
    pivot_wider(
      names_from = c("coef"), values_from = "value"
    ) |>
    group_by(sgp) |>
    summarise(across(c(mu, rho1), sd)) |>
    mutate(sgp = fct(sgp))
  # Todo: get the true RGP params too

  data_meta <- left_join(
    data_meta, data_params,
    by = "sgp", suffix = c("_est", "_true")
  ) |>
    mutate(
      mu_diff = abs(mu_est - mu_true),
      rho1_diff = abs(rho1_est - rho1_true)
    )

  data_final <- left_join(
    data_metrics,
    select(data_meta, sgp:model, matches("_est$|_true$|_diff$")),
    by = c("sgp", "rgp", "model", "sim"),
  )
}
