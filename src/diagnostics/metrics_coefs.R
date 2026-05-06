
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
  data_s = simulations_data; meta_s = simulations_meta; meta_e = estimations_meta
  test = TRUE; cond = TRUE
  filters = quos(
    rgp %in% groups$rgp_sym, sgp %in% groups$sgp_big,
  )
  filters = quos(
    rgp %in% groups$rgp_sym,
    sgp %in% groups$sgp_big,
    (rgp == "r2_markov_symm_high" & model == "r2_markov") |
      (rgp == "r2_threshold_symm_x" & model == "r2_threshold_x") |
      (rgp == "r2_stransition_symm_l" & model == "r2_stransition")
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
  ndf <- sum(!is.na(x))

  stars <- if (test) {
    # t <- sqrt(ndf) * (m - h0) / s
    # p <- 2 * pt(-abs(t), df = ndf - 1)
    add_star(t.test(x, mu = h0, conf.level = 0.95)$p.value)
  } else {
    ""
  }

  glue("{round(m, n)} ({round(s / sqrt(ndf), n + 1)}){stars}")
}


get_moments <- function(data_s, meta_s, cond = TRUE, test = cond) {
  add_r <- if (cond) expr(r)

  x <- data_s |>
    group_by(sgp, rgp, sim, !!add_r) |>
    summarise(
      avg = mean(y),
      acf = acor(y, p = 1),
      sd = sd(y)
    ) |>
    ungroup()

  opts <- expand_grid(
    o_sgp = unique(x$sgp),
    o_rgp = unique(x$rgp),
    o_r = if (cond) unique(x$r)
  )

  # Example `sgp = opts$sgp[1]; rgp = opts$rgp[1]; r = opts$r[1]`
  pmap_dfr(opts, \(o_sgp, o_rgp, o_r = NULL) {
    x_sub <- x |> filter(sgp == !!o_sgp, rgp == !!o_rgp, if (cond) r == !!o_r else TRUE)
    xh0 <- meta_s |> filter(sgp == !!o_sgp, rgp == !!o_rgp, if (cond) r == !!o_r else TRUE)

    c(
      sgp = o_sgp, rgp = o_rgp, r = o_r,
      avg = glue_test(x_sub$avg, xh0$avg, test = test & cond),
      acf = glue_test(atan(x_sub$acf), xh0$acf, test = test & cond), # Consider removing atan()
      sd = glue_test(x_sub$sd, xh0$sd, test = test & cond)
    )
  })
}



# Moments Table ----------------------------------------------------------

format_gt_metrics <- function(moments_conditional, moments_unconditional) {
  cols <- list(
    avg = c("avg_1", "avg_2", "avg_0"),
    acf = c("acf_1", "acf_2", "acf_0"),
    sd = c("sd_1", "sd_2", "sd_0")
  )

  bind_rows(moments_conditional, moments_unconditional) |>
    pivot_wider(names_from = r, values_from = c(avg, acf, sd)) |>
    relocate(rgp, sgp) |>
    arrange(rgp, sgp) |>
    gt(rowname_col = c("rgp", "sgp"), groupname_col = NULL) %>%
    cols_label_with(fn = \(x) {
      md(str_replace_all(x, c(
        "[^_]+_([1-9]+)" = "$s = \\1$",
        "[^_]+_0" = "$⊥ ~ s$" #\\perp
      )))
    }) |>
    tab_spanner(label = "DGP", columns = c("rgp", "sgp")) |>
    tab_stubhead(c("RGP", "RN")) |>
    reduce_spanners(cols, dicts$metrics$cond_gt) |>
    fmt_markdown(c("rgp", "sgp")) |>
    cols_align(align = "left", columns = list_c(cols)) |>
    fmt(columns = list_c(cols), fns = \(x) gsub("0(\\.[0-9]|$)", "\\1", x))
}

#' @export
metrics_table <- function(data_s, meta_s, ..., test = TRUE) {
  filters <- enquos(...)
  meta_names <- c("r1_avg", "r2_avg", "r1_acf", "r2_acf", "r1_sd", "r2_sd")

  meta_s <- meta_s |>
    mutate(
      map_dfr(meta, \(x) {
        coefs <- x$coefs
        metrics <- set_names(
          c(analytical_avg(coefs), analytical_acf(coefs), analytical_sd(coefs)),
          meta_names
        )
      })
    ) |>
    pivot_longer(r1_avg:r2_sd, names_to = c("r", ".value"), names_sep = "_") |>
    mutate(r = as.integer(str_remove(r, "r"))) |>
    mutate(
      rgp = dicts$rgps$gt_param[rgp],
      sgp = dicts$sgps$gt_param[sgp]
    )

  data_s <- data_s |>
    filter(!!!filters) |>
    mutate(
      rgp = dicts$rgps$gt_param[rgp],
      sgp = dicts$sgps$gt_param[sgp]
    )

  moments_conditional <- get_moments(data_s, meta_s, cond = TRUE, test = test)
  moments_unconditional <- get_moments(data_s, meta_s, cond = FALSE, test = test) |>
    mutate(r = "0")

  format_gt_metrics(moments_conditional, moments_unconditional)
}



# Coefficients Table ----------------------------------------------------------

format_gt_coefs <- function(meta_e) {
  cols <- list(
    mu = c("r1_mu", "r2_mu"),
    rho1 = c("r1_rho1", "r2_rho1"),
    sigma = c("r1_sigma", "r2_sigma")
  )

  meta_e |>
    relocate(rgp, sgp) |>
    arrange(rgp, sgp) |>
    gt(rowname_col = c("rgp", "sgp"), groupname_col = NULL) |>
    cols_label_with(fn = \(x) {
      md(str_replace_all(x, c(
        "r([0-9]+)_.+" = "$s = \\1$"
      )))
    }) |>
    tab_stubhead(c("RGP", "RN")) |>
    reduce_spanners(cols, dicts$params$gt_s) |>
    fmt_markdown(c("rgp", "sgp")) |>
    cols_align(align = "left", columns = list_c(cols)) |>
    fmt(columns = list_c(cols), fns = \(x) gsub("0(\\.[0-9]|$)", "\\1", x))
}

#' @export
coefs_table <- function(meta_e, ..., test = TRUE) {
  filters <- enquos(...)

  meta_e <- meta_e |>
    filter(!!!filters) |>
    mutate(
      rgp = dicts$rgps$gt_param[rgp],
      sgp = dicts$sgps$gt_param[sgp]
    ) |>
    relocate(rgp, sgp) |>
    mutate(
      map_dfr(meta_est, ~ matrix_to_vec(.x$coefs, suf = "_est")),
      map_dfr(meta_sim, ~ matrix_to_vec(.x$coefs, suf = "_sim"))
    ) |>
    group_by(rgp, sgp) |>
    summarise(
      r1_mu = glue_test(mu_R1_est, unique(mu_R1_sim), test = test),
      r2_mu = glue_test(mu_R2_est, unique(mu_R2_sim), test = test),
      r1_rho1 = glue_test(rho1_R1_est, unique(rho1_R1_sim), test = test),
      r2_rho1 = glue_test(rho1_R2_est, unique(rho1_R2_sim), test = test),
      r1_sigma = glue_test(sigma_R1_est, unique(sigma_R1_sim), test = test),
      r2_sigma = glue_test(sigma_R2_est, unique(sigma_R2_sim), test = test)
    ) |>
    ungroup()

  format_gt_coefs(meta_e)
}
