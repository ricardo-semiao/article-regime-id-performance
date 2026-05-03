
# Setup ------------------------------------------------------------------------

# Loading dependencies:
box::use(
  src/utils[...],
  src/utils2[...],
  metrics = ./metrics_definitions,
)

box::use(
  dtplyr[lazy_dt]
)

disp_mpe <- metrics$disp_mpe


# Example:
if (FALSE) {
  data_s = simulations_data; data_e = estimations_data;
  data_sm = simulations_meta; data_em = estimations_meta;
  n_t = n_t; n_b = n_b + n_l + 1; n_h = n_h
}



# Getting Metrics Data ---------------------------------------------------------

#' TODO: document
#' @export
get_metrics_data <- function(
  data_s, data_e, data_em,
  n_t, n_b, n_h
) {
  cat("Getting model metrics...\n")
  metrics_meta <- data_em |>
    arrange(sgp, rgp, model, sim) |>
    get_meta_metrics()

  cat("Getting regime metrics...\n")
  metrics_estimation <- left_join(
    rename(data_e, y_est = y, r_est = r),
    rename(data_s, y_true = y, r_true = r),
    by = c("sgp", "rgp", "sim", "t")
  ) |>
    arrange(sgp, rgp, model, sim, t) |>
    get_estimation_metrics(n_t, n_h, n_b)

  left_join(
    metrics_meta, metrics_estimation,
    by = c("sgp", "rgp", "sim", "model")
  ) |>
    mutate(
      switches_diff = abs(switches_est - switches_true),
      duration_diff = abs(duration_est - duration_true),
      avg_diff = abs(avg_est - avg_true),
      acf_diff = abs(acf_est - acf_true),
      sd_diff = abs(sd_est - sd_true),
      mu_diff = abs(mu_est - mu_true),
      rho1_diff = abs(rho1_est - rho1_true),
      sigma_diff = abs(sigma_est - sigma_true)
    )
}



# Estimation and Meta-Based Data -----------------------------------------------

#' TODO: document
get_meta_metrics <- function(data_em) {
  col <- \(x, col) x[, col] # Direct access bug in dtplyr

  lazy_dt(data_em) |>
    group_by(sgp, rgp, sim, model, arrange = FALSE) |> # TODO: consider rowwise()
    summarise(
      avg_true = metrics$analytical_avg(meta_sim[[1]]$coefs) |> disp_mpe(),
      acf_true = metrics$analytical_acf(meta_sim[[1]]$coefs) |> disp_mpe(),
      sd_true = metrics$analytical_sd(meta_sim[[1]]$coefs) |> disp_mpe(),
      mu_est = meta_est[[1]]$coefs |> col("mu") |> disp_mpe(),
      mu_true = meta_sim[[1]]$coefs |> col("mu") |> disp_mpe(),
      rho1_est = meta_est[[1]]$coefs |> col("rho1") |> disp_mpe(),
      rho1_true = meta_sim[[1]]$coefs |> col("rho1") |> disp_mpe(),
      sigma_est = meta_est[[1]]$coefs |> col("sigma") |> disp_mpe(),
      sigma_true = meta_sim[[1]]$coefs |> col("sigma") |> disp_mpe()
    ) |>
    ungroup() |>
    as_tibble()
}

#' TODO: document
get_estimation_metrics <- function(data_s_e, n_t, n_h, n_b) {
  lazy_dt(data_s_e) |>
    mutate(
      idx_fit = t > n_b & t <= n_t - n_h,
      idx_pred = t > n_t - n_h,
    ) |>
    group_by(sgp, rgp, sim, model, arrange = FALSE) |>
    summarise(
      avg_est = metrics$series_avg(y_true[idx_fit], r_est[idx_fit], na.rm = TRUE) |> disp_mpe(),
      acf_est = metrics$series_acf(y_true[idx_fit], r_est[idx_fit], na.rm = TRUE) |> disp_mpe(),
      sd_est = metrics$series_sd(y_true[idx_fit], r_est[idx_fit], na.rm = TRUE) |> disp_mpe(),
      rmse = metrics$performance_rmse(y_est[idx_pred], y_true[idx_pred]),
      mape = metrics$performance_mape(y_est[idx_pred], y_true[idx_pred]),
      r2 = metrics$performance_r2(y_est[idx_fit], y_true[idx_fit]),
      regimes_bme = metrics$performance_bme(r_est[idx_fit], r_true[idx_fit]),
      switches_est = metrics$average_switches(y_est[idx_fit], r_est[idx_fit]),
      duration_est = metrics$duration_diff(y_est[idx_fit], r_est[idx_fit]),
      switches_true = metrics$average_switches(y_true[idx_fit], r_true[idx_fit]),
      duration_true = metrics$duration_diff(y_true[idx_fit], r_true[idx_fit]),
      #skewness = metrics$inconditional_skewness(y_true[idx_fit], na.rm = TRUE),
      #kurtosis = metrics$inconditional_kurtosis(y_true[idx_fit], na.rm = TRUE)
    ) |>
    ungroup() |>
    as_tibble()
}
