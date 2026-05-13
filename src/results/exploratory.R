
# Setup ------------------------------------------------------------------------

box::use(
  src/utils[...],
  src/options[dicts],
  src/creators/metrics,
  src/parameters[n_t, n_h],
  ggplot2[...],
  gt[...],
  ggh4x[facet_grid2]
)


# Temporary example:
if (FALSE) {
  data_s = simulations_data; data_e = estimations_data
  filters = quos(
    sim %in% filter_sim_i, rgp %in% groups$rgp_sym
  )
  n = 2; test = TRUE
  filters = quos(
    sim %in% filter_sim_i, sgp %in% groups$sgp_big, rgp != "r1_nors"
  )
  opts = c("r2_set_symm_x", "r2_set_asymm_x")
  filters = quos(
    sim %in% filter_sim_i,
    rgp %in% c("r1_nors", groups$rgp_sym), sgp %in% groups$sgp_big
  )
  trim = 0.005
  mod_name = set_names(unique(data_e$model))[[1]]
}



# Metrics Separation at T ------------------------------------------------------

glue_test <- function(x, r, formula, n = 2, test = TRUE) {
  m <- metrics$series_avg(x, r, na.rm = TRUE) |> metrics$disp_mpe()
  s <- metrics$series_sd(x, r, na.rm = TRUE) |> metrics$disp_mpe()

  stars <- if (test) {
    p <- tryCatch(
      anova(lm(formula))[["Pr(>F)"]][1],
      error = \(e) NA_real_
    )
    add_star(p)
  } else {
    ""
  }

  glue("{fmt_decimal(m, n)}{stars} ({fmt_decimal(s, n)})")
}


#' @export
metrics_sep_table <- function(data_s, ..., n = 2, test = TRUE, rows = c(3, 7)) {
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
    ungroup() |>
    mutate(
      rgp = dicts$rgps$gt[rgp], # * Clumps RGPs by symmetry
    ) |>
    group_by(sgp, rgp) |>
    summarise(
      avg = glue_test(avg, r, avg ~ r, n, test = test),
      acf = glue_test(acf, r, acf ~ r, n, test = test),
      sd = glue_test(sd, r, sd ~ r, n, test = test)
    ) |>
    ungroup()

  data <- data_raw |>
    mutate(
      big_rn = c("small", "big")[grepl("2$", sgp) + 1],
      sgp = dicts$sgps$gt[sgp] |> fct(unique(dicts$sgps$gt)),
      rgp = fct(rgp, unique(dicts$rgps$gt))
    ) |>
    pivot_wider(names_from = big_rn, values_from = c(avg, acf, sd)) |>
    relocate(rgp, sgp) |>
    arrange(rgp, sgp)

  data |>
    mutate(across(everything(), as.character)) |>
    add_emtpy_rows(rows) |>
    gt(rowname_col = c("rgp", "sgp"), groupname_col = NULL) |>
    tab_stubhead(c("RGP", "RN")) |>
    reduce_spanners(cols, dicts$metrics$disp_gt) |>
    cols_align("left", list_c(cols)) |>
    cols_label(
      .list = set_names(
        map(dicts$sgps$gt_param, ~ md(str_replace(.x, ".+ ~ ", "$"))),
        list_c(cols)
      )
    ) |>
    fmt_markdown(c("rgp", "sgp")) |>
    add_footnote()
}



# Metrics Separation across t --------------------------------------------------

#' @export
metrics_sep_graphs <- function(data_s, ...) {
  filters <- enquos(...)

  stats <- function(y, r, n) {
    c(
      avg = metrics$series_avg(y, r, n_r = n) |> metrics$disp_mpe(n = n),
      acf = metrics$series_acf(y, r, n_r = n) |> metrics$disp_mpe(n = n),
      sd = metrics$series_sd(y, r, n_r = n) |> metrics$disp_mpe(n = n)
    )
  }

  rgp_list <- list(
    set = c("r2_set_symm_x", "r2_set_asymm_x"),
    st = c("r2_st_symm_l", "r2_st_asymm_l"),
    ms = c("r2_ms_symm_high", "r2_ms_asymm_high")
  )

  rgp_sym <- map_chr(rgp_list, 2)

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
    ungroup() |>
    mutate(
      sgp = dicts$sgps$gg[sgp] |> fct(unique(dicts$sgps$gg)) # * Clumps SGPs by RN Parameter
    ) |>
    group_by(rgp, sgp, t) |>
    reframe(
      across(
        c(avg, acf, sd),
        list(avg = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE))
      )
    ) |>
    ungroup()

  data_formatted <- data_raw |>
    pivot_longer(
      matches("^avg|^acf|^sd"),
      names_to = c("stat", ".value"), values_to = "value", names_sep = "_"
    ) |>
    mutate(
      sym_rgp = c("Symm.", "Asymm.")[rgp %in% rgp_sym + 1],
      stat = dicts$metrics$disp_gg[stat] |> fct(unique(dicts$metrics$disp_gg)),
      rgp = dicts$rgps$gg[rgp] |> fct(unique(dicts$rgps$gg))
    )

  map(rgp_list, function(opts) {
    ggplot(filter(data_formatted, rgp %in% dicts$rgps$gg[opts]), aes(t, avg)) +
      geom_line(aes(color = sym_rgp)) +
      geom_ribbon(aes(ymin = avg - sd * 1.96, ymax = avg + sd, fill = sym_rgp), alpha = 0.1) +
      geom_hline(yintercept = 0) +
      xlim(10, n_t) +
      labs(color = "DGP symmetry", fill = "DGP symmetry", x = "Time", y = "Moment's dispersion") +
      facet_grid2(
        vars(sgp), vars(stat), scales = "free_y", independent = "y",
        labeller = label_parsed
      ) +
      scale_fill_manual(values = unname(pal$main)) +
      scale_color_manual(values = unname(pal$main))
  })
}



# Forecasting errors and regime prediction -------------------------------------

#' @export
regimes_rmse_graphs <- function(data_e, data_s, ..., trim = 0.0005) {
  filters <- enquos(...)

  data_raw <- data_e |>
    filter(!!!filters, t > n_t - n_h)

  data_formatted <- data_raw |>
    left_join(
      data_s, by = c("sgp", "rgp", "sim", "t"),
      suffix = c("_est", "_sim")
    ) |>
    mutate(
      correct_r = c("Correct", "Incorrect")[r_err + 1],
      sgp = dicts$sgps$gg[sgp] |> fct(unique(dicts$sgps$gg)),
      rgp = dicts$rgps$gg[rgp] |> fct(unique(dicts$rgps$gg)) # * Clumps RGPs by symmetry and SGPs by RN Parameter
    ) |>
    group_by(sgp, rgp) |>
    filter(
      y_err >= quantile(y_err, trim, na.rm = TRUE),
      y_err <= quantile(y_err, 1 - trim, na.rm = TRUE)
    ) |>
    ungroup()

  map(set_names(unique(data_raw$model)), \(mod_name) {
    ggplot(filter(data_formatted, model == mod_name), aes(x = y_err)) +
      geom_density(aes(color = correct_r)) +
      facet_grid2(
        vars(sgp), vars(rgp),
        scales = "free", independent = "all", labeller = label_parsed
      ) +
      labs(y = "Density", x = "Forecasting error", color = "Regime ID") +
      scale_color_manual(values = unname(pal$main))
  })
}



# Metrics diff -----------------------------------------------------------------
#' @export
metrics_diff_graph <- function(data_m) {
  data_m |>
    #clump_dgps() |>
    filter(rgp != "r1_nors", ! model %in% c("r1_nors", "r1_rf")) |>
    pivot_longer(c(avg_diff, acf_diff, sd_diff)) |>
    mutate(
      rgp = dicts$rgps$gg[rgp] |> fct(unique(dicts$rgps$gg)),
      sgp = dicts$sgps$gg[sgp] |> fct(unique(dicts$sgps$gg)),
      model = dicts$models$gg[model] |> fct(unique(dicts$models$gg)),
      name = dicts$metrics$disp_gg[str_replace(name, "_diff", "")],
    ) |>
    group_by(name) |>
    filter(value < quantile(value, 0.99, na.rm = TRUE)) |>
    mutate(value = value / mad(value, na.rm = TRUE)) |>
    ggplot(aes(value, after_stat(count / sum(count)), fill = model)) +
    geom_histogram(bins = 50, position = "stack") +
    facet_grid(vars(sgp), vars(name), labeller = label_parsed) +
    scale_fill_manual(values = unname(pal$main)) +
    labs(x = "Metrics' absolute difference", y = "Frequency", fill = "Model")
}