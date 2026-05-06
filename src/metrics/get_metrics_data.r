
# Setup ------------------------------------------------------------------------

# Loading dependencies:
box::use(
  src/utils[...],
  metrics = ./metrics_definitions,
  src/parameters[n_t, n_h, n_b]
)

box::use(
  dtplyr[lazy_dt]
)

disp_mpe <- metrics$disp_mpe


# Example:
if (FALSE) {
  data_s = simulations_data; data_e = estimations_data;
  meta_s = simulations_meta; meta_e = estimations_meta;
  n_t = n_t; n_b = n_b + n_l + 1; n_h = n_h
}



# Getting Metrics Data ---------------------------------------------------------

#' TODO: document
#' @export
get_metrics_data <- function(data_e, meta_e) {
  cat("Getting model metrics...\n")
  metrics_meta <- get_meta_metrics(meta_e)

  cat("Getting regime metrics...\n")
  metrics_estimation <- get_estimation_metrics(data_e)

  left_join(
    metrics_meta, metrics_estimation,
    by = c("sgp", "rgp", "sim", "model"),
    na_matches = "never", relationship = "one-to-one", unmatched = "error"
  ) |>
    mutate(
      switches_diff = abs(switches_est - switches_sim),
      duration_diff = abs(duration_est - duration_sim),
      avg_diff = abs(avg_est - avg_sim),
      acf_diff = abs(acf_est - acf_sim),
      sd_diff = abs(sd_est - sd_sim),
      mu_diff = abs(mu_est - mu_sim),
      rho1_diff = abs(rho1_est - rho1_sim),
      sigma_diff = abs(sigma_est - sigma_sim)
    )
}



# Estimation and Meta-Based Data -----------------------------------------------

#' TODO: document
get_meta_metrics <- function(meta_e) {
  col <- \(x, col) x[, col] # Direct access bug in dtplyr

  lazy_dt(meta_e) |>
    group_by(sgp, rgp, sim, model, arrange = FALSE) |> # rowwise not supported
    summarise(
      avg_sim = metrics$analytical_avg(meta_sim[[1]]$coefs) |> disp_mpe(),
      acf_sim = metrics$analytical_acf(meta_sim[[1]]$coefs) |> disp_mpe(),
      sd_sim = metrics$analytical_sd(meta_sim[[1]]$coefs) |> disp_mpe(),
      mu_est = meta_est[[1]]$coefs |> col("mu") |> disp_mpe(),
      mu_sim = meta_sim[[1]]$coefs |> col("mu") |> disp_mpe(),
      rho1_est = meta_est[[1]]$coefs |> col("rho1") |> disp_mpe(),
      rho1_sim = meta_sim[[1]]$coefs |> col("rho1") |> disp_mpe(),
      sigma_est = meta_est[[1]]$coefs |> col("sigma") |> disp_mpe(),
      sigma_sim = meta_sim[[1]]$coefs |> col("sigma") |> disp_mpe()
    ) |>
    ungroup() |>
    as_tibble()
}

#' TODO: document
get_estimation_metrics <- function(data_e) {
  lazy_dt(data_e) |>
    mutate(
      idx_fit = t > n_b & t <= n_t - n_h & !is.na(y_est),
      # is.na(r_est): Tested in diag to be only the initial estimation window
      idx_pred = t > n_t - n_h,
    ) |>
    group_by(sgp, rgp, sim, model, arrange = FALSE) |>
    mutate( # Unsure if it hels or hurts performance
      n_r = max(r_est[idx_fit])
    ) |>
    summarise(
      avg_est = metrics$series_avg(y_sim[idx_fit], r_est[idx_fit], n_r = n_r[1]) |> disp_mpe(),
      acf_est = metrics$series_acf(y_sim[idx_fit], r_est[idx_fit], n_r = n_r[1]) |> disp_mpe(),
      sd_est = metrics$series_sd(y_sim[idx_fit], r_est[idx_fit], n_r = n_r[1]) |> disp_mpe(),
      rmse = metrics$performance_rmse(y_err[idx_pred]),
      mape = metrics$performance_mape(y_err[idx_pred], y_sim[idx_pred]),
      r2 = metrics$performance_r2(y_est[idx_fit], y_sim[idx_fit]),
      regimes_bme = metrics$performance_bme(r_err[idx_fit]),
      switches_est = metrics$average_switches(y_sim[idx_fit], r_est[idx_fit], n_r = n_r[1]),
      duration_est = metrics$duration_diff(y_sim[idx_fit], r_est[idx_fit], n_r = n_r[1]),
      switches_sim = metrics$average_switches(y_sim[idx_fit], r_sim[idx_fit], n_r = n_r[1]),
      duration_sim = metrics$duration_diff(y_sim[idx_fit], r_sim[idx_fit], n_r = n_r[1]),
      #skewness = metrics$inconditional_skewness(y_sim[idx_fit]),
      #kurtosis = metrics$inconditional_kurtosis(y_sim[idx_fit])
    ) |>
    ungroup() |>
    as_tibble()
}
