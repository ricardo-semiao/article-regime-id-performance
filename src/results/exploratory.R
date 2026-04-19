
# Setup ----------------------------------------------------------

box::use(
  src/utils[...],
  src/options[dicts],
  src/metrics,
  gt[...]
)


# Temporary example:
if (FALSE) {
  data = simulations_data
  filters = exprs(
    row_number() %in% sample(n(), 100),
    rgp %in% c("r2_markov_symm_high", "r2_sbreak_mid", "r2_threshold_x_0", "r2_stransition_l0")
  )
  data = estimations_data
}



# Metrics Separation at T ------------------------------------------------------

glue_test <- function(x, r, formula, n = 2, test = TRUE) {
  m <- metrics$series_avg(x, r, na.rm = TRUE) |> metrics$diff_k_2()
  s <- metrics$series_sd(x, r, na.rm = TRUE) |> metrics$diff_k_2()

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
metrics_sep_table <- function(data, ..., test = TRUE) {
  filters <- enquos(...)

  cols <- list(
    avg = c("avg_small", "avg_big"),
    acf = c("acf_small", "acf_big"),
    sd = c("sd_small", "sd_big")
  )

  data |>
    filter(!!! filters) |>
    group_by(sgp, rgp, sim) |>
    reframe(
      r = 1:max(r),
      avg = metrics$series_avg(y, r, na.rm = TRUE),
      acf = metrics$series_acf(y, r, na.rm = TRUE),
      sd = metrics$series_sd(y, r, na.rm = TRUE),
    ) |>
    group_by(sgp, rgp) |>
    summarise(
      avg = glue_test(avg, r, avg ~ r, test = test),
      acf = glue_test(acf, r, acf ~ r, test = test),
      sd = glue_test(sd, r, sd ~ r, test = test)
    ) |>
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
metrics_sep_graphs <- function(data, ..., n_t) {
  filters <- enquos(...)

  stats <- function(y, r) {
    c(
      avg = metrics$series_avg(y, r, na.rm = TRUE) |> metrics$diff_k_2(),
      acf = metrics$series_acf(y, r, use = "na.or.complete") |> metrics$diff_k_2(),
      sd = metrics$series_sd(y, r, na.rm = TRUE) |> metrics$diff_k_2()
    )
  }

  rgp_list <- list(
    set = c("r2_threshold_x_0", "r2_threshold_x_05"),
    sb = c("r2_sbreak_mid", "r2_sbreak_end"),
    st = c("r2_stransition_l0", "r2_stransition_l05"),
    ms = c("r2_markov_symm_high", "r2_markov_asymm_high")
  )

  rgp_sym <- c(
    "r2_markov_symm_high", "r2_sbreak_mid",
    "r2_threshold_x_0", "r2_stransition_l0"
  )

  data_formatted <- data |>
    filter(!!!filters) |>
    group_by(rgp, sgp, sim) |>
    reframe(
      map_dfr(1:n_t, \(tmax) stats(y = y[t <= tmax], r = r[t <= tmax])),
      t = 1:n_t
    ) |>
    group_by(rgp, sgp, t) |>
    reframe(
      across(
        c(avg, acf, sd), 
        list(avg = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE))
      )
    ) |>
    pivot_longer(
      matches("^avg|^acf|^sd"),
      names_to = c("stat", ".value"), values_to = "value", names_sep = "_"
    ) |>
    mutate(
      sym_rgp = c("Symm.", "Asymm.")[rgp %in% rgp_sym + 1],
      sgp = dicts$sgp$gg[sgp] |>
        fct(c("mu", "rho*'_1'", "sigma")) |>
        fct_recode("rho[1]" = "rho*'_1'"),
      stat = fct(stat, c("avg", "acf", "sd")) |>
        fct_recode(!!!(dicts$metrics$gg %>% {set_names(names(.), .)}))
    )

  map(rgp_list, function(opts) {
    ggplot(filter(data_formatted, rgp %in% opts), aes(t, avg)) +
      geom_line(aes(color = sym_rgp)) +
      geom_ribbon(aes(ymin = avg - sd, ymax = avg + sd, fill = sym_rgp), alpha = 0.1) +
      geom_hline(yintercept = 0) +
      xlim(10, n_t) +
      labs(color = "DGP symmetry", fill = "DGP symmetry", x = "Time", y = "Moment's dispersion") +
      ggh4x::facet_grid2(
        vars(sgp), vars(stat), scales = "free_y", labeller = label_parsed
      )
  })
}
