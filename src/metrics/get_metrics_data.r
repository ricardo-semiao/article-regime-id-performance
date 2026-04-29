
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

diff_k_2 <- metrics$diff_k_2


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
    rename(meta_true = meta_sim) |>
    arrange(sgp, rgp, model, sim) |>
    get_meta_metrics(n_t, n_h)

  cat("Getting regime metrics...\n")
  metrics_estimation <- left_join(
    rename(data_e, y_est = y, r_est = r),
    rename(data_s, y_true = y, r_true = r),
    by = c("sgp", "rgp", "sim", "t")
  ) |>
    filter(t > n_b) |> # * Should be unecessary
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
get_meta_metrics <- function(data, n_t, n_h) {
  lazy_dt(data) |>
    group_by(sgp, rgp, sim, model, arrange = FALSE) |>
    summarise(
      avg_true = metrics$analytical_avg(meta_true[[1]]$coefs) |> diff_k_2(),
      acf_true = metrics$analytical_acf(meta_true[[1]]$coefs) |> diff_k_2(),
      sd_true = metrics$analytical_sd(meta_true[[1]]$coefs) |> diff_k_2(),
      mu_est = meta_est[[1]]$coefs[1:2, "mu"] |> diff_k_2(),
      mu_true = meta_true[[1]]$coefs[1:2, "mu"] |> diff_k_2(),
      rho1_est = meta_est[[1]]$coefs[1:2, "rho1"] |> diff_k_2(),
      rho1_true = meta_true[[1]]$coefs[1:2, "rho1"] |> diff_k_2(),
      sigma_est = meta_est[[1]]$coefs[1:2, "sigma"] |> diff_k_2(),
      sigma_true = meta_true[[1]]$coefs[1:2, "sigma"] |> diff_k_2()
    ) |>
    ungroup() |>
    as_tibble()
}

#' TODO: document
get_estimation_metrics <- function(data, n_t, n_h, n_b) {
  lazy_dt(data) |>
    group_by(sgp, rgp, sim, model, arrange = FALSE) |>
    summarise(
      avg_est = metrics$series_avg(y_true, r_est, na.rm = TRUE) |> diff_k_2(),
      acf_est = metrics$series_acf(y_true, r_est, na.rm = TRUE) |> diff_k_2(),
      sd_est = metrics$series_sd(y_true, r_est, na.rm = TRUE) |> diff_k_2(),
      rmse = metrics$performance_rmse(y_est, y_true, n_h, n_t, t = t),
      mape = metrics$performance_mape(y_est, y_true, n_h, n_t, t = t),
      r2 = metrics$performance_r2(y_est, y_true, n_h, n_t, n_b, t = t),
      regimes_bme = metrics$performance_bme(r_est, r_true, n_h, n_t, t = t),
      switches_est = metrics$average_switches(y_est, r_est),
      duration_est = metrics$duration_diff(y_est, r_est),
      switches_true = metrics$average_switches(y_true, r_true),
      duration_true = metrics$duration_diff(y_true, r_true),
      #skewness = metrics$inconditional_skewness(y_true, na.rm = TRUE),
      #kurtosis = metrics$inconditional_kurtosis(y_true, na.rm = TRUE)
    ) |>
    ungroup() |>
    as_tibble()
}
