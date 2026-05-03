
# Setup ----------------------------------------------------------

box::use(
  src/utils[...],
  src/options[dicts],
  src/metrics,
  ggplot2[...],
  gt[...]
)


# Temporary example:
if (FALSE) {
  data_s = simulations_data; data_e = estimations_data
  filters = quos(
    sim %in% sample(n_s, 10),
    rgp %in% c("r1_no_rs", groups$rgp_sym),
    sgp %in% groups$sgp_big
  )
  trim = 0.005
  opts = c("r2_threshold_symm_x", "r2_threshold_asymm_x")
  mod_name = set_names(unique(data_e$model))[[1]]
}



# Metrics Separation at T ------------------------------------------------------

glue_test <- function(x, r, formula, n = 2, test = TRUE) {
  m <- metrics$series_avg(x, r, na.rm = TRUE) |> metrics$disp_mpe()
  s <- metrics$series_sd(x, r, na.rm = TRUE) |> metrics$disp_mpe()

  stars <- if (test) {
    p <- tryCatch(
      anova(lm(formula))[["Pr(>F)"]][1],
      error = function(e) NA_real_
    )
    add_star(p)
  } else {
    ""
  }

  glue("{round(m, n)} ({round(s, n)}){stars}")
}


#' @export
metrics_sep_table <- function(data_s, ..., n = 2, test = TRUE) {
  filters <- enquos(...)

  cols <- list(
    avg = c("avg_small", "avg_big"),
    acf = c("acf_small", "acf_big"),
    sd = c("sd_small", "sd_big")
  )

  data_raw <- data_s |>
    filter(!!! filters) |>
    group_by(sgp, rgp, sim) |>
    reframe(
      avg = metrics$series_avg(y, r, na.rm = TRUE),
      acf = metrics$series_acf(y, r, na.rm = TRUE),
      sd = metrics$series_sd(y, r, na.rm = TRUE),
      r = 1:max(r)
    ) |>
    group_by(sgp, rgp) |>
    summarise(
      avg = glue_test(avg, r, avg ~ r, n, test = test),
      acf = glue_test(acf, r, acf ~ r, n, test = test),
      sd = glue_test(sd, r, sd ~ r, n, test = test)
    ) |>
    ungroup()

  data_raw |>
    relocate(rgp, sgp) |>
    arrange(rgp, sgp) |>
    mutate(
      big_rn = c("small", "big")[grepl("2$", sgp) + 1],
      rgp = dicts$rgp$gt[rgp],
      sgp = dicts$sgp$gt[sgp] %>%
        {str_replace(as.character(.), " ~ .+", "$")}
    ) |>
    pivot_wider(names_from = big_rn, values_from = c(avg, acf, sd)) |>
    gt(rowname_col = c("rgp", "sgp"), groupname_col = NULL) |>
    tab_stubhead(c("RGP", "RN")) |>
    tab_spanner(label = dicts$metrics$gt$avg, columns = cols$avg) |>
    tab_spanner(label = dicts$metrics$gt$acf, columns = cols$acf) |>
    tab_spanner(label = dicts$metrics$gt$sd, columns = cols$sd) |>
    cols_label(
      .list = set_names(
        map(dicts$sgp$gt, ~ md(str_replace(.x, ".+ ~ ", "$"))),
        list_c(cols)
      )
    ) |>
    fmt_markdown(c("rgp", "sgp"))
}



# Metrics Separation across t ----------------------------------------------------------

#' @export
metrics_sep_graphs <- function(data_s, ..., n_t) {
  filters <- enquos(...)

  stats <- function(y, r, n) {
    c(
      avg = metrics$series_avg(y, r, n_r = n) |> metrics$disp_mpe(n = n),
      acf = metrics$series_acf(y, r, n_r = n) |> metrics$disp_mpe(n = n),
      sd = metrics$series_sd(y, r, n_r = n) |> metrics$disp_mpe(n = n)
    )
  }

  rgp_list <- list(
    nors = c("r1_no_rs"),
    set = c("r2_threshold_symm_x", "r2_threshold_asymm_x"),
    st = c("r2_stransition_symm_l", "r2_stransition_asymm_l"),
    ms = c("r2_markov_symm_high", "r2_markov_asymm_high")
  )

  rgp_sym <- map_chr(rgp_list[-1], 2)

  data_raw <- data_s |>
    filter(!!!filters) |>
    group_by(rgp, sgp, sim) |>
    reframe(
      map_dfr(1:n_t, \(tmax) {
        idx <- t <= tmax
        stats(y = y[idx], r = r[idx], n = max(r))
      }),
      t = 1:n_t
    ) |>
    mutate(
      sgp = dicts$sgp$gg[sgp] |> fct(unique(dicts$sgp$gg)) # * Clumps SGPs by RN Parameter
    ) |>
    group_by(rgp, sgp, t) |>
    reframe(
      across(
        c(avg, acf, sd),
        list(avg = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE))
      )
    )

  data_formatted <- data_raw |>
    pivot_longer(
      matches("^avg|^acf|^sd"),
      names_to = c("stat", ".value"), values_to = "value", names_sep = "_"
    ) |>
    mutate(
      sym_rgp = c("Symm.", "Asymm.")[rgp %in% rgp_sym + 1],
      stat = fct(stat, c("avg", "acf", "sd")) |>
        fct_recode(!!!(dicts$metrics$gg %>% {set_names(names(.), .)}))
    )

  map(rgp_list, function(opts) {
    ggplot(filter(data_formatted, rgp %in% opts), aes(t, avg)) +
      geom_line(aes(color = sym_rgp)) +
      geom_ribbon(aes(ymin = avg - sd * 1.96, ymax = avg + sd, fill = sym_rgp), alpha = 0.1) +
      geom_hline(yintercept = 0) +
      xlim(10, n_t) +
      labs(color = "DGP symmetry", fill = "DGP symmetry", x = "Time", y = "Moment's dispersion") +
      ggh4x::facet_grid2(
        vars(sgp), vars(stat), scales = "free_y", labeller = label_parsed
      )
  })
}


# Forecasting errors and regime prediction ----------------------------------------------------------

#' @export
regimes_rmse_graphs <- function(data_e, data_s, n_t, n_h, ..., trim = 0.0005) {
  filters <- enquos(...)

  data_formatted <- data_e |>
    filter(!!!filters, t > n_t - n_h) |>
    left_join(
      data_s, by = c("sgp", "rgp", "sim", "t"),
      suffix = c("_est", "_sim")
    ) |>
    mutate(
      error = y_est - y_sim,
      correct_r = c("Correct", "Incorrect")[(r_est == r_sim) + 1],
      sgp = dicts$sgps$gg[sgp] |> fct(unique(dicts$sgps$gg)),
      rgp = dicts$rgps$gg[rgp]
    ) |>
    group_by(sgp, rgp) |>
    filter(
      error >= quantile(error, trim, na.rm = TRUE),
      error <= quantile(error, 1 - trim, na.rm = TRUE)
    )

  map(set_names(unique(data_e$model)), \(mod_name) {
    ggplot(filter(data_formatted, model == mod_name), aes(x = error)) +
      geom_density(aes(color = correct_r)) +
      ggh4x::facet_grid2(
        vars(sgp), vars(rgp),
        scales = "free", independent = "all", labeller = label_parsed
      ) +
      labs(y = "Density", x = "Forecasting error", color = "Regime ID")
  })
}
