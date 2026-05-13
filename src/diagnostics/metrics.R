
# Setup ------------------------------------------------------------------------

box::use(
  src/utils[...],
  src/options[dicts],
  src/creators/metrics[analytical_avg, analytical_acf, analytical_sd],
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
    (rgp == "r2_ms_symm_high" & model == "r2_ms") |
      (rgp == "r2_set_symm_x" & model == "r2_set") |
      (rgp == "r2_st_symm_l" & model == "r2_st")
  )
}



# Helpers ----------------------------------------------------------------------

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
      avg = glue_t_test(x_sub$avg, xh0$avg, test = test & cond),
      acf = glue_t_test(atan(x_sub$acf), xh0$acf, test = test & cond), # Consider removing atan()
      sd = glue_t_test(x_sub$sd, xh0$sd, test = test & cond)
    )
  })
}



# Moments Table ----------------------------------------------------------------

format_gt_metrics <- function(
  moments_conditional, moments_unconditional, rows = c(3, 7)
) {
  cols <- list(
    avg = c("avg_1", "avg_2", "avg_0"),
    acf = c("acf_1", "acf_2", "acf_0"),
    sd = c("sd_1", "sd_2", "sd_0")
  )

  bind_rows(moments_conditional, moments_unconditional) |>
    mutate(
      rgp = fct(rgp, unique(dicts$rgps$gt_param)),
      sgp = fct(sgp, unique(dicts$sgps$gt_param))
    ) |>
    pivot_wider(names_from = r, values_from = c(avg, acf, sd)) |>
    relocate(rgp, sgp) |>
    arrange(rgp, sgp) |>
    mutate(across(everything(), as.character)) |>
    add_emtpy_rows(rows) |>
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
    fmt(columns = list_c(cols), fns = \(x) gsub("0(\\.[0-9]|$)", "\\1", x)) |>
    add_footnote()
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
