
# Setup ------------------------------------------------------------------------

# Loading dependencies:
box::use(
  src/utils[...],
  metrics = src/creators/metrics,
  src/parameters[n_t, n_h, n_b],
  dtplyr[lazy_dt],
  latex2exp[TeX]
)

disp_mpe <- metrics$disp_mpe


# Example:
if (FALSE) {
  data_s = simulations_data; data_e = estimations_data;
  meta_s = simulations_meta; meta_e = estimations_meta;
  n_t = n_t; n_b = n_b + n_l + 1; n_h = n_h
}



# Dictionary -------------------------------------------------------------------

#' @export
dict <- list3(
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

#' TODO: document
get_meta_metrics <- function(meta_e) {
  col <- \(x, col) x[, col] # Direct access bug in dtplyr

  lazy_dt(meta_e) |>
    group_by(sgp, rgp, sim, model, arrange = FALSE) |> # rowwise() not supported
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
      idx_pred = t > n_t - n_h,
      # * is.na(y_est): equal to is.na(r_est) and only the initial estimation window
    ) |>
    group_by(sgp, rgp, sim, model, arrange = FALSE) |>
    mutate( # TODO: check if helps performance
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
      duration_est = metrics$regimes_duration(y_sim[idx_fit], r_est[idx_fit], n_r = n_r[1]) |> disp_mpe(),
      switches_sim = metrics$average_switches(y_sim[idx_fit], r_sim[idx_fit], n_r = n_r[1]),
      duration_sim = metrics$regimes_duration(y_sim[idx_fit], r_sim[idx_fit], n_r = n_r[1]) |> disp_mpe()
    ) |>
    ungroup() |>
    as_tibble()
}
