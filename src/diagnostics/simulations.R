

# Setup ----------------------------------------------------------

box::use(
  src/utils[...],
  src/options[dicts],
  src/metrics[analytical_avg, analytical_acf, analytical_sd],
  gt[...],
  ggplot2[...]
)


# Temporary example:
if (FALSE) {
  data = simulations_data; data_meta = simulations_meta; test = TRUE
  filters = exprs(
    rgp %in% c("r2_markov_symm_high", "r2_sbreak_mid", "r2_threshold_x_0", "r2_stransition_l0"),
    sgp %in% c("r2_ar1_mu2", "r2_ar1_rho2", "r2_ar1_sigma2")
  )
  data = estimations_meta
  filters = exprs(
    rgp %in% c("r2_markov_symm_high", "r2_sbreak_mid", "r2_threshold_x_0", "r2_stransition_l0"),
    sgp %in% c("r2_ar1_mu2", "r2_ar1_rho2", "r2_ar1_sigma2"),
    (rgp == "r2_markov_symm_high" & model == "r2_markov") |
      (rgp == "r2_sbreak_mid" & model == "r2_sbreak") |
      (rgp == "r2_threshold_x_0" & model == "r2_threshold_x") |
      (rgp == "r2_stransition_l0" & model == "r2_stransition")
  )
}



# Helpers ----------------------------------------------------------

matrix_to_vec <- function(x, sep = "_", pref = "", suf = "") {
  structure(x,
    dim = NULL,
    names = pmap_chr(
      expand_grid(colnames(x), rownames(x)),
      ~ paste0(pref, .x, sep, .y, suf)
    )
  )
}

glue_test <- function(x, h0, n = 2, test = TRUE) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)

  stars <- if (test) {
    # ndf <- sum(!is.na(x))
    # t <- sqrt(ndf) * (m - h0) / s
    # p <- 2 * pt(-abs(t), df = ndf - 1)
    add_star(t.test(x, mu = h0, conf.level = 0.95)$p.value)
  } else {
    ""
  }

  glue("{round(m, n)} ({round(s, n)}){stars}")
}

get_moments <- function(x, xh0, cond = TRUE, test = cond) {
  add_r <- if (cond) expr(r)

  x <- x |>
    group_by(sgp, rgp, sim, !!add_r) |>
    summarise(
      avg = mean(y, na.rm = TRUE),
      acf = cor(y[-n()], y[-1], use = "na.or.complete"),
      sd = sd(y, na.rm = TRUE)
    )

  opts <- expand_grid(
    sgp = unique(x$sgp),
    rgp = unique(x$rgp),
    r = if (cond) unique(x$r) else NULL
  )

  pmap_dfr(opts, \(sgp, rgp, r = NULL) {
    x_sub <- x |> filter(sgp == !!sgp, rgp == !!rgp, if (cond) r == !!r else TRUE)
    xh0_sub <- xh0 |> filter(sgp == !!sgp, rgp == !!rgp, if (cond) r == !!r else TRUE)

    c(
      sgp = sgp, rgp = rgp, r = r,
      avg = glue_test(x_sub$avg, h0 = xh0_sub$avg, test = test & cond),
      acf = glue_test(x_sub$acf, h0 = xh0_sub$acf, test = test & cond),
      sd = glue_test(x_sub$sd, h0 = xh0_sub$sd, test = test & cond)
    )
  })
}

format_gt_metrics <- function(moments_conditional, moments_unconditional) {
  cols <- list(
    avg = c("avg_1", "avg_2", "avg_0"),
    acf = c("acf_1", "acf_2", "acf_0"),
    sd = c("sd_1", "sd_2", "sd_0")
  )

  bind_rows(moments_conditional, moments_unconditional) |>
    pivot_wider(
      names_from = r,
      values_from = c(avg, acf, sd),
    ) |>
    relocate(rgp, sgp) |>
    arrange(rgp, sgp) |>
    gt(rowname_col = c("rgp", "sgp"), groupname_col = NULL) %>%
    {do.call(cols_label, c(.data = list(.), dicts$regimes$metrics[]))} |>
    tab_spanner(label = "DGP", columns = c("rgp", "sgp")) |>
    tab_spanner(label = md("$\\hat{\\mu}(.)$"), columns = cols$avg) |>
    tab_spanner(label = md("$\\hat{\\rho}_1(.)$"), columns = cols$acf) |>
    tab_spanner(label = md("$\\hat{\\sigma}(.)$"), columns = cols$sd) |>
    fmt_markdown(c("rgp", "sgp")) |>
    cols_align(align = "left", columns = list_c(cols)) |>
    fmt(columns = list_c(cols), fns = \(x) gsub("0(\\.[0-9]|$)", "\\1", x))
}

format_gt_coefs <- function(data) {
  cols <- list(
    mu = c("r1_mu", "r2_mu"),
    rho1 = c("r1_rho1", "r2_rho1"),
    sigma = c("r1_sigma", "r2_sigma")
  )

  data |>
    relocate(rgp, sgp) |>
    arrange(rgp, sgp) |>
    gt(rowname_col = c("rgp", "sgp"), groupname_col = NULL) %>%
    {do.call(cols_label, c(.data = list(.), dicts$regimes$coefs[]))} |>
    tab_stubhead(c("RGP", "RN")) |>
    tab_spanner(label = dicts$metrics$gt$avg, columns = cols$mu) |>
    tab_spanner(label = dicts$metrics$gt$acf, columns = cols$rho1) |>
    tab_spanner(label = dicts$metrics$gt$sd, columns = cols$sigma) |>
    fmt_markdown(c("rgp", "sgp")) |>
    cols_align(align = "left", columns = list_c(cols)) |>
    fmt(columns = list_c(cols), fns = \(x) gsub("0(\\.[0-9]|$)", "\\1", x))
}



# Moments Table ----------------------------------------------------------

#' @export
moments_table <- function(data, data_meta, ..., test = TRUE) {
  filters <- enquos(...)

  data_meta_formatted <- data_meta |>
    mutate(
      map_dfr(meta, \(x) {
        metrics <- c(
          analytical_avg(x$coefs), analytical_acf(x$coefs), analytical_sd(x$coefs)
        ) |>
          set_names(c(
            "r1_avg", "r2_avg", "r1_acf", "r2_acf", "r1_sd", "r2_sd"
          ))
      })
    ) |>
    pivot_longer(r1_avg:r2_sd, names_to = c("r", ".value"), names_sep = "_") |>
    mutate(r = as.integer(str_remove(r, "r"))) |>
    mutate(
      rgp = dicts$rgp$gt[rgp],
      sgp = dicts$sgp$gt[sgp]
    )

  data_formatted <- data |>
    filter(!!!filters) |>
    mutate(
      rgp = dicts$rgp$gt[rgp],
      sgp = dicts$sgp$gt[sgp]
    )

  moments_conditional <- get_moments(
    data_formatted, data_meta_formatted,
    cond = TRUE, test = test
  )

  moments_unconditional <- get_moments(
    data_formatted, data_meta_formatted,
    cond = FALSE, test = test
  ) |>
    mutate(r = "0")

  format_gt_metrics(moments_conditional, moments_unconditional)
}



# Coefficients Table ----------------------------------------------------------

#' @export
coefs_table <- function(data, ..., test = test) {
  filters <- enquos(...)

  data_formatted <- data |>
    filter(!!!filters) |>
    mutate(
      rgp = dicts$rgp$gt[rgp],
      sgp = dicts$sgp$gt[sgp]
    ) |>
    relocate(rgp, sgp) |>
    mutate(
      map_dfr(meta_est, ~ matrix_to_vec(.x$coefs, suf = "_est")),
      map_dfr(meta_sim, ~ matrix_to_vec(.x$coefs, suf = "_sim"))
    ) |>
    #na.omit() |> # TODO: check reason, clean up earlier maybe
    group_by(rgp, sgp) |>
    summarise(
      r1_mu = glue_test(mu_R1_est, unique(mu_R1_sim), test = test),
      r2_mu = glue_test(mu_R2_est, unique(mu_R2_sim), test = test),
      r1_rho1 = glue_test(rho1_R1_est, unique(rho1_R1_sim), test = test),
      r2_rho1 = glue_test(rho1_R2_est, unique(rho1_R2_sim), test = test),
      r1_sigma = glue_test(sigma_R1_est, unique(sigma_R1_sim), test = test),
      r2_sigma = glue_test(sigma_R2_est, unique(sigma_R2_sim), test = test)
    )

  format_gt_coefs(data_formatted)
}



# Improbable Things ----------------------------------------------------------

#' @export
improbable_counts <- function(data_e, data_s, n_b, n_t, n_h) {
  te1 <- n_b + 1
  te2 <- n_t - n_h - 1
  tp1 <- n_t - n_h

  left_join(
    data_e, data_s,
    by = c("sgp", "rgp", "sim", "t"), suffix = c("_est", "_sim")
  ) |>
    group_by(sgp, rgp, sim) |>
    summarise(
      fit = sum(
        y_est[t %in% te1:te2] > mean(y_sim, na.rm = TRUE) + 3 * sd(y_sim, na.rm = TRUE),
        na.rm = TRUE
      ) / sum(!is.na(y_est[t %in% te1:te2])),
      pred = sum(
        y_est[t %in% tp1:n_t] > y_sim[t %in% tp1:n_t] + 3 * sd(y_sim, na.rm = TRUE),
        na.rm = TRUE
      ) / sum(!is.na(y_est[t %in% tp1:n_t]))
    ) |>
    ungroup() |>
    summarise(
      fit = mean(fit, na.rm = TRUE),
      pred = mean(pred, na.rm = TRUE)
    )
}
# TODO: optimize
