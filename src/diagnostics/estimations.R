
box::use(
  src/utils[...],
  src/metrics[series_sd],
  src/options[dicts],
  src/parameters[...],
  ggplot2[...],
  patchwork[...],
  gt[...]
)

# - NAs in y and r: should be the same, and exactly as many as the model requires (n_b + n_l + possibly more)
# - NAs in coefs: should only exist on sigma with 1 regime or 2 regimes but only one observation in one of them. There are other cases breaking this rule that should be investigated, but are currently removed
# - Distribution of R: number of unique R's and distribution of less frequent regime. Should remove cases with 1 regime or 2 but only one observation in one of them
# - Distribution of errors: of residuals and forecasting errors. Should look good
# - Distribution of coefs: should look good
# - Distribution of other metadata: currently ignored

# Temporary example:
if (FALSE) {
  meta_e = estimations_meta; data_e = estimations_data
  rmv = diag_obs_remove; n_l = 1
  residuals = FALSE; bins = 50; rmv_out = TRUE
}



# NAs ----------------------------------------------------------

#' @export
nas_on_fit <- function(data_e) {
  data <- data_e |>
    group_by(sgp, rgp, sim, model) |>
    summarise(
      na_y = sum(is.na(r_est)),
      na_r = sum(is.na(r_est)),
    )

  cat("NAs in fit counts:")
  data |>
    group_by(model) |>
    summarise(
      y = length(unique(na_y)),
      r = length(unique(na_r)),
      equal = all(na_y == na_r)
    ) |>
    print()

  invisible(data)
}

#' @export
nas_on_coefs <- function(meta_e, data_e) {
  coefs <- meta_e |>
    rowwise() |>
    summarise(
      sgp = sgp[1], rgp = rgp[1], sim = sim[1], model = model[1],
      na_mu = anyNA(meta_est$coefs[, "mu"]),
      na_rho1 = anyNA(meta_est$coefs[, "rho1"]),
      na_sigma = anyNA(meta_est$coefs[, "sigma"])
    ) |>
    ungroup()

  cat("NAs in coefs counts:")
  coefs |>
    pivot_longer(c(na_mu, na_rho1, na_sigma)) |>
    filter(value) |>
    with(table(model, name)) |>
    print()

  invisible(coefs)
}

# Unexported for now, as sigma NAs are being investigated and are currently removed
study_sigma <- function(coefs, data_e) {
  fab <- function(ik) {
    obs <- with(ik, paste0(sgp, rgp, sim, model, sep = "-"))
    data_e |>
      filter(paste0(sgp, rgp, sim, model, sep = "-") %in% obs) |>
      group_by(sgp, rgp, sim, model) |>
      reframe(
        sd = series_sd(y_err, r_est, n_r = 2, na.rm = TRUE),
        r = 1:2
      ) |>
      pivot_wider(names_from = r, values_from = sd, names_prefix = "sd_r")
  }

  rs <- data_e |>
    group_by(sgp, rgp, sim, model) |>
    summarise(
      r_unique = length(unique(na.omit(r_est))),
      r_prop = min(tabulate(r_est)) # ! Incorrect
    ) |>
    ungroup()

  data <- left_join(
    coefs, rs, by = c("sgp", "rgp", "sim", "model"),
    na_matches = "never", unmatched = "error", relationship = "one-to-one"
  )

  i1 <- ab |> filter(r_unique == 2 & r_prop == 1 & na_sigma)
  i2 <- ab |> filter(r_unique == 2 & r_prop == 1 & !na_sigma)
  i3 <- ab |> filter(r_unique == 2 & r_prop == 2 & na_sigma)
  i4 <- ab |> filter(r_unique == 2 & r_prop == 2 & !na_sigma)
  i5 <- ab |> filter(r_unique == 2 & r_prop == 3 & na_sigma)

  fab(i1) |> complete.cases() |> table() |> print() # ok
  fab(i2) |> complete.cases() |> table() |> print() # these should have had na_sigma = TRUE
  fab(i3) |> complete.cases() |> table() |> print() # these should have had na_sigma = FALSE
  fab(i4) |> complete.cases() |> table() |> print() # these should have had na_sigma = FALSE and all the same
  fab(i5) |> complete.cases() |> table() |> print() # these should have had na_sigma = FALSE
}



# Distributions ---------------------------------------------------------

#' Errors distribution
#' @export
erros_distribution_est <- function(
  data_e, residuals = TRUE, lims = c(x = NA, y = NA), bins = 50,
  breaks = c(0, 2, 5, 10, 20, 50, 100, 500, 1000, 10000, Inf), cut_n = 1
) {
  filt <- if (residuals) expr(t > n_b & t <= n_t - n_h) else expr(t > n_t - n_h)

  g <- data_e |>
    filter(!!filt, !is.na(y_err)) |>
    mutate(model = dicts$models$gg[model] |> fct(dicts$models$gg)) |>
    ggplot(aes(y_err, after_stat(count / sum(count)))) +
    geom_histogram(bins = bins) +
    facet_wrap(vars(model), labeller = label_parsed) +
    xlim(- lims["x"], lims["x"]) + ylim(0, lims["y"]) +
    labs(x = if (residuals) "Residuals" else "Forecasting errors", y = "Frequency")
  plot(g)

  diag_high_rmse <- data_e |>
    filter(!!filt, !is.na(y_err)) |>
    group_by(sgp, rgp, sim, model) |>
    summarise(rmse = sqrt(mean(y_err^2))) |>
    ungroup() |>
    mutate(outlier = rmse > breaks[length(breaks) - cut_n])

  cat("RMSE counts:")
  with(diag_high_rmse, {
    cuts <- cut(rmse, breaks = breaks)
    tab <- table(cuts)

    rbind(
      tab, #%>% {. / sum(.)}
      map_dbl(breaks[-1], ~ sd(rmse[rmse < .x]) |> round(3))
    )
  }) |>
    print()

  invisible(diag_high_rmse)
}
# Also consider trimmed_sd(rmse, trim)
# TODO: also consider removing series with high amount of individual outliers
# TODO: make lims dynamic
# TODO: maybe MAE is a better measure than RMSE for estimation issues
# TODO: maybe consider outliers in y_est in relation to mean(meta_sim$mu) instead of y_err
# TODO: use the correct sigma from meta_e


#' Regimes proportions
#' @export
regimes_proportions_est <- function(data_e, n_l, bins = 50) {
  diag_regime_data <- data_e |>
    filter(t > n_b & t <= n_t - n_h & !is.na(y_est)) |>
    group_by(sgp, rgp, sim, model) |>
    summarise(
      n_r_model = as.integer(gsub("r([0-9])+_.+", "\\1", model[1])),
      tab = list(tabulate(r_est, n_r_model)),
      n_rare = min(tab[[1]]),
      n_rare_prop = n_rare / sum(tab[[1]]),
      n_r_est = sum(tab[[1]] > 0)
    ) |>
    ungroup()

  cat("Amount of regime obs:")
  diag_regime_data |>
    with(table(cut(n_rare, c(0, 1, 2, 3, Inf), include.lowest = TRUE))) |>
    print()

  g <- diag_regime_data |>
    mutate(model = dicts$models$gg[model] |> fct(dicts$models$gg)) |>
    ggplot(aes(n_rare_prop)) +
    geom_histogram(bins = 50) +
    geom_vline(xintercept = 1 / (n_t - n_b - n_h - n_l), linetype = "dashed") +
    facet_wrap(vars(model), labeller = label_parsed) +
    xlim(0, 0.5) +
    labs(x = "Proportion of observations in the smallest regime", y = "Count")
  plot(g)

  invisible(diag_regime_data)
}
# TODO: Consider generalize prop and n_r to the same thing, i.e. value 0 when
# only one regime



# Metadata Distribution ------------------------------------------------------

#' @export
parameters_distribution <- function(meta_e, data_e, q = 0.95, k = 20, rmv_out = FALSE, bins = 50) {
  max_y <- quantile(abs(data_e$y_sim), q)

  diag_param_data <- meta_e |>
    rowwise() |>
    reframe( # TODO: Also consider summarise + mean(...)
      sgp = sgp[1], rgp = rgp[1], sim = sim[1], model = model[1],
      mu = (meta_est$coefs[, "mu"]),
      rho1 = (meta_est$coefs[, "rho1"]),
      sigma = (meta_est$coefs[, "sigma"])
    ) |>
    ungroup() |>
    pivot_longer(c(mu, rho1, sigma), names_to = "param")

  cat("Parameters' statistics:")
  diag_param_data |>
    group_by(param) |>
    summarise(
      mean = mean(value, na.rm = TRUE),
      median = median(value, na.rm = TRUE),
      sd = sd(value, na.rm = TRUE),
      mad = mad(value, na.rm = TRUE)
    ) |>
    print()

  diag_param_data_filt <- diag_param_data |>
    group_by(param) |>
    mutate(
      out = abs(value - median(value, na.rm = TRUE)) > k * mad(value, na.rm = TRUE),
      out = case_when(
       param == "mu" ~ abs(value) > max_y,
       param == "rho1" ~ out,
       param == "sigma" ~ FALSE,
       TRUE ~ NA
      )
    ) |>
    ungroup()

  cat("\nParameters' outliers counts:")
  diag_param_data_filt |>
    filter(out) |>
    with(table(model, param)) |>
    print()

  g <- diag_param_data_filt %>%
    {if (rmv_out) filter(., !out) else .} |>
    mutate(
      model = dicts$models$gg[model] |> fct(dicts$models$gg),
      param = dicts$params$gg[param] |> fct(dicts$params$gg)
    ) |>
    ggplot(aes(value, after_stat(count / sum(count)))) +
    geom_histogram(bins = bins) +
    facet_grid(vars(model), scales = "free", vars(param), labeller = label_parsed) +
    labs(x = "Estimated parameter's value", y = "Frequency")
  plot(g)

  invisible(diag_param_data_filt)
}
# TODO: consider other out measures


#' @export
meta_distribution <- function(meta_e) {
  data_gamma <- meta_e |>
    filter(model == "r2_stransition") |>
    mutate(
      gamma = map_dbl(meta_est, ~ .x$gamma %||% NA),
      model = dicts$models$gg[model] |> fct(dicts$models$gg)
    )

  g1 <- ggplot(data_gamma, aes(gamma, after_stat(count / sum(count)))) +
    geom_histogram() +
    facet_wrap(vars(model), nrow = 1) +
    labs(x = "Gamma")

  data_tau <- meta_e |>
    filter(model %in% c("r2_threshold_x", "r2_stransition")) |>
    mutate(
      tau = map_dbl(meta_est, ~ .x$switches %||% NA),
      model = dicts$models$gg[model] |> fct(dicts$models$gg)
    )

  g2 <- ggplot(data_tau, aes(tau, after_stat(count / sum(count)))) +
    geom_histogram() +
    facet_wrap(vars(model), nrow = 1) +
    labs(x = "Tau")

  data_p <- meta_e |>
    filter(model == "r2_markov") |>
    mutate(
      p = map(meta_est, ~ .x$switches %>% {c(.[1, 1], .[2, 2])}),
      model = dicts$models$gg[model] |> fct(dicts$models$gg)
    ) |>
    unnest_wider(p, names_sep = "") |>
    pivot_longer(c(p1, p2)) |>
    mutate(name = c(p1 = "P(1 | 1)", p2 = "P(2 | 2)")[name])

  g3 <- ggplot(data_p, aes(value, after_stat(count / sum(count)), fill = name)) +
    geom_histogram(position = "stack") +
    facet_wrap(vars(model), nrow = 1) +
    labs(x = "Probability", fill = "")

  g <- g2 / (g1 + g3) & labs(y = "Frequency")
  plot(g + plot_layout(axis_titles = "collect"))

  invisible(list(gamma = data_gamma, tau = data_tau, p = data_p))
}
# TODO: Pannelize



# Save Errors ----------------------------------------------------------

#' @export
save_obs_removed <- function(rmv) {
  total <- n_s * n_m
  ids <- c("full_sample", "no_convergence", "na_coefs", "few_r_obs", "high_params", "high_error")

  removals <- tibble(
    id = ids,
    text = c(
      "Full sample", "Bad convergence", "NAs in parameters", "Low #obs. in regimes",
      "Unreasonable parameters", "Unreasonable errors"
    ),
    items = c(list(NULL), list(NULL), rmv[ids[-(1:2)]][]),
    n_bad = NA,
    n_rmv = NA
  )

  removals$n_bad[1] <- removals$n_rmv[1] <- 0L
  removals$items[[1]] <- character(0)

  removals$n_bad[2] <- removals$n_rmv[2] <-
    readLines("outputs/diagnostics/estimation_errors.md", 1) |>
    str_replace("Total: ([0-9]+)", "\\1") |>
    as.integer()
  removals$items[[2]] <- character(removals$n_bad[2])

  removals$n_bad <- map_int(removals$items, length)

  removed <- character(0)
  for (i in 1:nrow(removals)) {
    removals$n_rmv[i] <- length(removals$items[[i]] %>% .[! . %in% removed])
    removed <- union(removed, removals$items[[i]])
  }

  removals |>
    mutate(n_left = total - cumsum(n_rmv)) %>%
    with({
      add_row(.,
        text = "Total", n_bad = sum(n_bad), n_left = total - sum(n_rmv),
        n_rmv = sum(n_rmv)
      )
    }) |>
    mutate(
      prop_bad = n_bad / total, prop_rmv = n_rmv / total,
      across(c(prop_bad, prop_rmv), ~ round(.x * 100, 1) |> str_c("%"))
    )  |>
    print() |>
    select(
      text, `Bad obs.` = n_bad, `% Bad` = prop_bad, `% Removed` = prop_rmv,
      `Obs. left` = n_left
    ) |>
    gt(rowname_col = "text") |>
    #tab_subhead("")
    fmt_number(c("Bad obs.", "Obs. left"), decimals = 0) %>%
    tab_style(
      cell_text(weight = "bold"),
      list(cells_body(rows = nrow(.[["_data"]])), cells_stub(nrow(.[["_data"]])))
    )
}

# Currently not used:
save_errors <- function(errors, out) {
  total <- 0

  imap(errors, \(items, error) {
    n <- length(items)
    total <<- total + n

    paste0(
      "Error: ", error, "\n",
      "- Occurences: ", n, "\n",
      "- Items: ", str_c(items, collapse = ", ")
    )
  }) |>
    str_c(collapse = "\n\n") |>
    str_c("Total:", total, "\n\n", .x = _) |>
    writeLines(out)
}
