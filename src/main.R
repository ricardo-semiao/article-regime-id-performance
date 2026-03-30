
# Setup: Modules and Parameters ------------------------------------------------

box::purge_cache()
box::use(
  src/utils[...],
  src/utils2[...],
  src/options[dicts, params, options],
  src/diagnostics,
  src/metrics,
  rTRNG[rnorm_trng]
)


# Simulation parameters:
n_s <- 500L # Number of simulations
n_burn <- 10L # Burn-in periods
n_h <- 10L # Number of periods to predict
n_t <- 100L + n_burn + n_h # Number of time periods


# Debug:
if (FALSE) {
  #save.image("personal/workspace.RData")
  load("personal/workspace.RData")
}



# Simulation: Considered Options -----------------------------------------------

# Used combinations:
walk(list(options$sgps, options$rgps), ~ dput(names(.x)))

dgp_names <- expand_grid(
  sgp = c(
    "r2_ar1_mu1", "r2_ar1_mu2",
    "r2_ar1_rho1", "r2_ar1_rho2",
      #"r2_ar1_sign1", "r2_ar1_sign2",
      #"r2_ar2_pos1", "r2_ar2_pos2",
      #"r2_ar2_neg1", "r2_ar2_neg2",
    "r2_ar1_vol1", "r2_ar1_vol2",
    NULL
  ),
  rgp = c(
      #"r2_multinomial_equal", "r2_multinomial_reg1",
    "r2_markov_symm_high", "r2_markov_asymm_high",
    #"r2_markov_symm_low", "r2_markov_asymm_low",
    "r2_sbreak_mid", "r2_sbreak_end",
    "r2_threshold_x_0", "r2_threshold_x_05",
    #"r2_threshold_abs_05", "r2_threshold_abs_2",
    #"r2_threshold_diff_05", "r2_threshold_diff_2",
    "r2_stransition_l0", "r2_stransition_l05",
      #"r2_stransition_e0", "r2_stransition_e05",
    NULL # To correct trailing comma
  )
) |>
  mutate(dgp = str_c(sgp, "-", rgp))

n_p <- nrow(dgp_names)

sim_names <- expand_grid(dgp_names, sim = 1:n_s) |>
  mutate(dgp_sim = str_c(dgp, "-", sim))



# Simulation: Errors -----------------------------------------------------------

# Error generation:
errors_raw <- rnorm_trng(n_t * n_p * n_s, parallelGrain = 100)

diagnostics$errors$errors_dependence(errors_raw)
if (FALSE) ggsave2("outputs/errors/dependence.png", 17, 14)

diagnostics$errors$errors_distribution(errors_raw)
if (FALSE) ggsave2("outputs/errors/distribution.png", 28, 14)

errors <- errors_raw |>
  matrix(nrow = n_t, ncol = n_p * n_s) |>
  `colnames<-`(sim_names$dgp_sim)



# Simulation: Series -----------------------------------------------------------

# Simulation inputs:
sim_inputs <- pmap(sim_names, \(sgp, rgp, dgp, sim, dgp_sim) {
  list(
    sgp = options$sgps[[sgp]],
    rgp = options$rgps[[rgp]],
    errors = errors[, dgp_sim]
  )
}) |>
  set_names(sim_names$dgp_sim)

# Simulation function:
# Example: `input <- sim_inputs[["r2_ar1_vol2-r2_lstar_05-14"]]`
simulate_serie <- function(input) {
  sfun <- input$sgp$fun
  rfun <- input$rgp$fun

  n_r <- input$rgp$n_r
  t_start <- input$sgp$t_cut + 1
  y <- input$errors

  r <- matrix(0, nrow = n_t, ncol = n_r)
  r_start <- eval(
    input$rgp$r_start,
    list(y = y, r = r, t_start = t_start, fun = rfun)
  )
  r[seq_len(t_start - 1), r_start] <- 1

  for (t in t_start:n_t) {
    r[t, ] <- rfun(y, r, t)
    y[t] <- sfun(y, r, t)
  }

  list(r = r, y = y) # Todo: summarize r into vector might be needed for memory
}

# Running simulations:
safe <- TRUE
simulations <- map_parallel(
  sim_inputs, simulate_serie,
  n_t = n_t,
  parallel = TRUE, safe = safe
)


# Saving results:
if (FALSE) {
  write_rds(simulations, "data/simulations.rds") %>%
    {cli$cli_alert_success("Simulations saved to {.file data/simulations.rds}")}
  simulations <- read_rds("data/simulations.rds")
}

# Checking errors:
if (safe) {
  names(compact(map(simulations, "error"))) |> cli_alert_items()
  simulations <- map(simulations, "result")
}


# Collecting results:
simulations_data <- imap(simulations, \(res, sim_name) {
  sim_opts <- str_split_1(sim_name, "-")
  tibble(
    sgp = sim_opts[1], rgp = sim_opts[2], sim = sim_opts[3],
    t = 1:n_t, y = res$y, r = max.col(res$r)
  )
}) |>
  bind_rows() |>
  mutate(
    across(c(sgp, rgp), fct),
    across(c(sim, r), as.integer)
  )



# Simulation: Diagnostics ------------------------------------------------------

# Setup:
sims_sample <- sample(n_s, 10)

dgp_names_main <- dgp_names |>
  filter(
    str_detect(sgp, "2$"),
    str_detect(rgp, "_high$|_0$|_mid$|_l0$")
  ) |>
  distinct(sgp, rgp, .keep_all = TRUE)
# Example: `list2env(dgp_names_main[3, ], globalenv())`


# Simulations panel for each RGP (each with all SGPs within):
pwalk(distinct(dgp_names_main, rgp), \(rgp) {
  plot_sgps_sim(
    simulations_data, diagnostics$simulations$panel_simulations,
    unique(dgp_names_main$sgp), rgp, n_burn = n_burn, lims = c(0, 0.4)
  )
  ggsave2("outputs/simulations/values-{rgp}.png", 28, 14)
})


# Stats panels for each SPG and RGP combination:
pwalk(dgp_names_main, \(sgp, rgp, dgp) {
  plot_sgps_sim(
    simulations_data, diagnostics$simulations$panel_stats,
    unique(dgp_names_main$sgp), rgp,
    dimension = "sgp", option = sgp, sims = sims_sample[1:5], n_burn = n_burn,
    regime_aligned = TRUE, lims = c(0, 3)
  )
  ggsave2("outputs/simulations/stats_sgp-{rgp}.png", 28, 14)

  plot_sgps_sim(
    simulations_data, diagnostics$simulations$panel_stats,
    unique(dgp_names_main$sgp), rgp,
    dimension = "rgp", option = rgp, sims = sims_sample[1:5], n_burn = n_burn,
    regime_aligned = !grepl("threshold", rgp), lims = c(0, 15)
  ) & ylim(0, 1)
  ggsave2("outputs/simulations/stats_rpg-{rgp}.png", 28, 14)
})


# Table of statistics:
.tab <- diagnostics$simulations$table_sgps(simulations_data, dgp_names_main$dgp)
if (FALSE) cat(gt::as_latex(.tab), file = "outputs/simulations/table_sgps.tex")

.tab <- diagnostics$simulations$table_rgps(simulations_data, dgp_names_main$dgp)
if (FALSE) cat(gt::as_latex(.tab), file = "outputs/simulations/table_rgps.tex")
rm(.tab)



# Estimation: Models -----------------------------------------------------------

# Used models:
dput(names(options$models))

model_names <- expand_grid(
  sim_names,
  model = c(
    "r2_sbreak",
    "r2_threshold_x",
    #"r2_threshold_abs",
    #"r2_threshold_diff",
    "r2_stransition",
    "r2_markov",
    NULL
  )
) |>
  mutate(dgp_sim_model = str_c(dgp_sim, "-", model))

n_l <- 1 # Can be arbitrarily large, must be at least the max n_l used in models
n_m <- length(unique(model_names$model))

# Estimation inputs:
est_inputs <- map(simulations, \(sim) list(y = sim$y))
#est_inputs <- keep_at(est_inputs, ~ grepl("-[1-2]$", .x))

# Estimation function:
# Example: `input <- est_inputs[["r2_ar1_rho2-r2_threshold_x_0-2"]]`
estimate_models <- function(input) {
  data <- data_lags(data.frame(y = input$y), n_l = n_l)

  results <- vector("list", n_m)
  names(results) <- names(mods)

  for (mod_name in names(mods)) {
    results[[mod_name]] <- mods[[mod_name]](data, n_t, n_h, n_burn)
  }

  results # Todo: transpose?
}

# Running estimations:
safe <- TRUE
considered_models <- options$models[unique(model_names$model)]
if (safe) considered_models <- map(considered_models, safely_modify)

estimations <- map_parallel(
  est_inputs, estimate_models,
  mods = considered_models, data_lags = data_lags,
  n_burn = n_burn, n_h = n_h, n_t = n_t, n_l = n_l, n_m = n_m,
  parallel = TRUE, safe = FALSE
)


# Saving results:
if (FALSE) {
  write_rds(estimations, "data/estimations.rds") %>%
    {cli$cli_alert_success("Estimations saved to {.file data/estimations.rds}")}
  estimations <- read_rds("data/estimations.rds")
}

# Checking errors:
if (safe) {
  names(compact(map(estimations, ~ compact(map(.x, "error"))))) |>
    cli_alert_items()
  estimations <- map(estimations, ~ compact(map(.x, "result")))
}

# Checking regimes:
check_n_regimes <- imap_dfr(estimations, \(sim, name) {
  c(name = name, map(sim, \(model) length(table(model$r)))[])
}) |>
  pivot_longer(-name, names_to = "model") |>
  filter(value != 2)
table(check_n_regimes$model)

for (l in split(check_n_regimes, 1:nrow(check_n_regimes))) {
  estimations[[l$name]][[l$model]] <- NULL
}
estimations <- map(estimations, compact)
rm(check_n_regimes, l)


# Collecting results:
estimations_data <- list_flatten(estimations, name_spec = "{outer}-{inner}") |>
  imap(\(res, sim_name) {
    sim_opts <- str_split_1(sim_name, "-")
    ord <- regimes_order(res$meta$coefs, sim_opts[1])

    list(
      sgp = sim_opts[1], rgp = sim_opts[2], sim = sim_opts[3],
      model = sim_opts[4], t = 1:n_t, y = res$y, r = ord[res$r]
    )
  }) |>
  bind_rows() |>
  mutate(
    across(c(sgp, rgp, model), fct),
    across(c(sim, r), as.integer)
  )

estimations_meta <- list_flatten(estimations, name_spec = "{outer}-{inner}") |>
  imap(\(res, sim_name) {
    sim_opts <- str_split_1(sim_name, "-")
    meta <- res$meta

    ord <- regimes_order(meta$coefs, sim_opts[1])
    meta$coefs <- meta$coefs[ord, ]
    dimnames(meta$coefs) <- list(
      colnames = c("mu", paste0("rho", 1:n_l)),
      rownames = paste0("R", 1:nrow(meta$coefs))
    )

    list(
      sgp = sim_opts[1], rgp = sim_opts[2], sim = sim_opts[3],
      model = sim_opts[4], meta = list(meta)
    )
  }) |>
  bind_rows() |>
  mutate(
    across(c(sgp, rgp, model), fct),
    across(c(sim), as.integer)
  )
# Todo: other metas also need to be ordered



# Estimation: Diagnostics ------------------------------------------------------

# Setup:
model_names_main <- model_names |>
  filter(
    str_detect(sgp, "2$"),
    str_detect(rgp, "_high$|_0$|_mid$|_l0$")
  ) |>
  distinct(sgp, rgp, .keep_all = TRUE)
# Example: `list2env(model_names_main[1, ], globalenv())`


# Example of true vs fitted plot:
{
  sgp <- "r2_ar1_rho2"; rgp <- "r2_markov_symm_high"; model <- "r2_markov"
  sgp <- "r2_ar1_mu2"; rgp <- "r2_threshold_x_05"; model <- "r2_threshold_x"
  diagnostics$estimations$panel_estimations(
    subset_results(simulations_data, sgps = sgp, rgps = rgp, model = model),
    subset_results(estimations_data, sgps = sgp, rgps = rgp, model = model),
    n_burn = n_burn, n_t = n_t, n_h = n_h,
    title = glue("SGP: {dicts$sgps[sgp]}\nRGP: {dicts$rgps[rgp]}\nModel: {dicts$models[model]}")
  )
  ggsave2("outputs/estimations/fit-{rgp}-{model}.png", 28, 14)
}


# Residuals panel for each RGP-Model pair (each with all SGPs within):
pwalk(distinct(model_names_main, rgp, model), \(rgp, model, ...) {
  plot_sgps_est(
    estimations_data, simulations_data, diagnostics$estimations$panel_residuals,
    sgps = unique(model_names_main$sgp), rgp = rgp, model = model,
    n_burn = n_burn, n_t = n_t, n_h = n_h,
    regime_aligned = FALSE, hline = 0, title = NULL, lims = c(0, 0.4)
  ) +
    plot_annotation(
      title = glue("RGP: {dicts$rgps[rgp]};  Model: {dicts$models[model]}")
    )
  ggsave2("outputs/estimations/residuals-{rgp}-{model}-na.png", 28, 14)
})


# Residuals panel for each RGP-Model pair (each with all SGPs within):
pwalk(distinct(model_names, rgp, sim, model), \(rgp, model, ...) {
  data <- subset_results(
    estimations_meta, rgps = rgp, model = model
  )
  diagnostics$estimations$coefs_distribution(
    data, params, model_names
  )
  if (FALSE) ggsave2("outputs/estimations/coefs-{rgp}-{model}.png", 26, 16)
})


# Table of statistics:
.tab <- diagnostics$estimations$table_residuals(
  simulations_data, estimations_data, TRUE,
  dgp_names_main$dgp, na.rm = TRUE, n_burn = n_burn
)
if (FALSE) cat(gt::as_latex(.tab), file = "outputs/estimations/table_residuals.tex")
# Todo: table across regime correctness



# Metrics ----------------------------------------------------------------------

simulations_meta <- params$sgps[unique(dgp_names$sgp)] |>
  map(\(xsgp) {
    map(xsgp$args, \(x) {
      x$mu <- as.integer(x$mu || 0)
      x$vol <- as.integer(x$vol || 1)
      unlist(x)
    }) |>
      do.call(cbind, args = _) |>
      `colnames<-`(c("R1", "R2"))
  }) |>
  map(~ list(coefs = .x)) %>%
  {tibble(sgp = names(.), meta = .)} |>
  full_join(sim_names, by = "sgp")

if (FALSE) {
  estimations_meta$meta <- map(estimations_meta$meta, \(meta) {
    meta$coefs <- rbind(meta$coefs, "vol" = rnorm(2, 1, 0.1))
    meta
  })
}

# data_s = simulations_data; data_e = estimations_data; data_m = estimations_meta
data_models_final <- metrics$get_data_final(
  simulations_data, estimations_data, simulations_meta, estimations_meta,
  n_t = n_t, n_warm = n_burn + n_l + 1, n_h = n_h, model_names = model_names
)

if (FALSE) {
  write_rds(data_models_final, "data/data_models_final.rds") %>%
    {cli$cli_alert_success("Final models data saved to {.file data/data_models_final.rds}")}
  data_models_final <- read_rds("data/data_models_final.rds")
}



# Comparisons ------------------------------------------------------------------

# Do for both data_models_final and data_regimes_final. Regimes has an extra
# regime variable

colnames(data_models_final)

# Adjusting factor levels:
map(data_models_final[c("rgp", "sgp", "model")], levels)
reg_data <- data_models_final |>
  mutate(
    sgp = str_remove(sgp, "[^_]$"),
    rgp = str_remove(rgp, "_[^_]+$"),
  )
map(reg_data[c("rgp", "sgp", "model")], unique)


# Removing extreme outliers:
reg_data$rmse |> cut(breaks = c(0, 5, 10, 25, 50, 100, 1000, Inf)) |> table()
reg_data$mape |> cut(breaks = c(0, 5, 10, 25, 50, 100, 1000, Inf)) |> table()
# Todo: histogram with log_x
reg_data <- filter(reg_data, rmse < 10)


# First step:
# Add regressions on the identification of regime variable and metrics
# themselves


# Basic regressions:
lm(rmse ~ sim, reg_data) |> summary()
lm(rmse ~ sgp * rgp, reg_data) |> summary()
lm(rmse ~ model * rgp, hi) |> summary()
lm(rmse ~ model * rgp + sgp * rgp, reg_data) |> summary()


# Metrics must be separated by the relevant factor (SGP metrics run separately
# for each SGP, same for RGP)
lm(rmse ~ model*rgp + rgp + sgp_metric_est, filter(reg_data, sgp == "r2_ar1_vol")) |> summary()
lm(rmse ~ model*rgp + rgp + sgp_metric_diff, filter(reg_data, sgp == "r2_ar1_vol")) |> summary()
lm(rmse ~ model*rgp + rgp + model*sgp_metric_est, reg_data) |> summary()

lm(rmse ~ model*rgp + rgp + mu_diff, filter(reg_data, sgp == "r2_ar1_mu")) |> summary()


# Graphs:
reg_data$rmse |> cut(breaks = c(5, 10, 25, 50, 100, 1000)) |> table()
(reg_data$rmse > 5) |> table()

plot_results <- function(
  data, x,
  logx = FALSE, logy = FALSE, smooth = TRUE, n = 3000
) {
  data |>
    slice_sample(n = n) |>
    ggplot(aes({{x}}, rmse)) +
    geom_point(aes(color = model, shape = model), alpha = 0.5) +
    ggh4x::facet_grid2(vars(rgp), vars(sgp), scales = "free", independent = "all") +
    list(
      if (logx) scale_x_log10(),
      if (logy) scale_y_log10(),
      if (smooth) geom_smooth(aes(color = model), method = "lm", fill = NA)
    )
}

plot_results(reg_data, mape, logx = TRUE)

plot_results(reg_data, sgp_metric_est)
plot_results(reg_data, sgp_metric_true)
plot_results(reg_data, sgp_metric_diff)

plot_results(reg_data, mu_est)
plot_results(reg_data, mu_diff)

plot_results(reg_data, rho1_est)
plot_results(reg_data, rho1_diff)



# New analysis =================================================================

# Diagnostics ------------------------------------------------------------------

box::use(gt[...])

glue_mean <- function(x, n = 2, random_stars = FALSE) {
  stars <- if (random_stars) {
    sample(c("", "*", "**", "***"), 1, prob = c(0.95, 0.05, 0, 0))
  } else {
    ""
  }
  glue("{round(mean(x, na.rm = TRUE), n)} ({round(sd(x, na.rm = TRUE), n)}){stars}")
}

diag_t1_a <- simulations_data |>
  filter(
    rgp %in% c("r2_markov_symm_high", "r2_sbreak_mid", "r2_threshold_x_0", "r2_stransition_l0"),
    sgp %in% c("r2_ar1_mu2", "r2_ar1_rho2", "r2_ar1_vol2")
  ) |>
  group_by(sgp, rgp, sim, r) |>

  summarise(
    avg = mean(y, na.rm = TRUE),
    acf = tryCatch(cor(y[-n()], y[-1], use = "complete.obs"), error = \(e) NA_real_),
    vol = sd(y, na.rm = TRUE)
  ) |>
  group_by(sgp, rgp, r) |>
  summarise(
    avg = glue_mean(avg),
    acf = glue_mean(acf),
    vol = glue_mean(vol)
  )

diag_t1_b <- simulations_data |>
  filter(
    rgp %in% c("r2_markov_symm_high", "r2_sbreak_mid", "r2_threshold_x_0", "r2_stransition_l0"),
    sgp %in% c("r2_ar1_mu2", "r2_ar1_rho2", "r2_ar1_vol2")
  ) |>
  group_by(sgp, rgp, sim) |>
  summarise(
    avg = mean(y, na.rm = TRUE),
    acf = tryCatch(cor(y[-n()], y[-1], use = "complete.obs"), error = \(e) NA_real_),
    vol = sd(y, na.rm = TRUE)
  ) |>
  group_by(sgp, rgp) |>
  summarise(
    avg = glue_mean(avg),
    acf = glue_mean(acf),
    vol = glue_mean(vol)
  ) |>
  mutate(r = 0)

fmt_cols <- c(
  "avg_1", "avg_2", "avg_0",
  "acf_1", "acf_2", "acf_0",
  "vol_1", "vol_2", "vol_0"
)

bind_rows(diag_t1_a, diag_t1_b) |>
  pivot_wider(
    names_from = r,
    values_from = c(avg, acf, vol),
  ) |>
  relocate(rgp, sgp) |>
  arrange(rgp, sgp) |>
  mutate(
    rgp = str_replace_all(rgp, c(
      "r2_markov_symm_high" = "MS, symm.",
      "r2_markov_symm_low" = "MS, asymm.",
      "r2_threshold_x_0" = "SET, τ = 0",
      "r2_threshold_x_05" = "SET, τ = 0.5",
      "r2_stransition_l0" = "ST, τ = 0",
      "r2_stransition_l05" = "ST, τ = 0.5",
      "r2_sbreak_mid" = "SB, mid",
      "r2_sbreak_end" = "SB, end"
    )),
    sgp = str_replace_all(sgp, c(
      "r2_ar1_mu1" = "μ, (0, 0.5)",
      "r2_ar1_mu2" = "μ, (0, 2)",
      "r2_ar1_rho1" = "ρ, (0.1, 0.9)",
      "r2_ar1_rho2" = "ρ, (0.4, 0.6)",
      "r2_ar1_vol1" = "σ, (1, 2)",
      "r2_ar1_vol2" = "σ, (1, 4)"
    ))
  ) |>
  gt(rowname_col = c("rgp", "sgp"), groupname_col = NULL) |>
  cols_label(
    rgp = "RGP",
    sgp = "SGP",
    avg_1 = "s = 1",
    avg_2 = "s = 2",
    avg_0 = "⫠s",
    acf_1 = "s = 1",
    acf_2 = "s = 2",
    acf_0 = "⫠s",
    vol_1 = "s = 1",
    vol_2 = "s = 2",
    vol_0 = "⫠s",
  ) |>
  tab_spanner(label = "DGP", columns = c("rgp", "sgp")) |>
  tab_spanner(label = "Average", columns = c("avg_1", "avg_2", "avg_0")) |>
  tab_spanner(label = "ACF", columns = c("acf_1", "acf_2", "acf_0")) |>
  tab_spanner(label = "Volatility", columns = c("vol_1", "vol_2", "vol_0")) |>
  cols_align(align = "left", columns = fmt_cols) |>
  fmt(columns = fmt_cols, fns = \(x) gsub("0(\\.[0-9]|$)", "\\1", x)) |>
  gtsave("outputs/table_dgp.tex")
  #as_latex() |>
  #clipr::write_clip()

# TODO: use latex math in the labels and such


diag_t2_a <- estimations_data |>
  filter(
    rgp %in% c("r2_markov_symm_high", "r2_sbreak_mid", "r2_threshold_x_0", "r2_stransition_l0"),
    sgp %in% c("r2_ar1_mu2", "r2_ar1_rho2", "r2_ar1_vol2"),
    (rgp == "r2_markov_symm_high" & model == "r2_markov") |
      (rgp == "r2_sbreak_mid" & model == "r2_sbreak") |
      (rgp == "r2_threshold_x_0" & model == "r2_threshold_x") |
      (rgp == "r2_stransition_l0" & model == "r2_stransition"),
    !is.na(r)
  ) |>
  group_by(sgp, rgp, sim, r) |>
  summarise(
    avg = mean(y, na.rm = TRUE),
    acf = tryCatch(cor(y[-n()], y[-1], use = "complete.obs"), error = \(e) NA_real_),
    vol = sd(y, na.rm = TRUE)
  ) |>
  group_by(sgp, rgp, r) |>
  summarise(
    avg = glue_mean(avg),
    acf = glue_mean(acf),
    vol = glue_mean(vol)
  )

diag_t2_b <- estimations_data |>
  filter(
    rgp %in% c("r2_markov_symm_high", "r2_sbreak_mid", "r2_threshold_x_0", "r2_stransition_l0"),
    sgp %in% c("r2_ar1_mu2", "r2_ar1_rho2", "r2_ar1_vol2"),
    (rgp == "r2_markov_symm_high" & model == "r2_markov") |
      (rgp == "r2_sbreak_mid" & model == "r2_sbreak") |
      (rgp == "r2_threshold_x_0" & model == "r2_threshold_x") |
      (rgp == "r2_stransition_l0" & model == "r2_stransition"),
    !is.na(r)
  ) |>
  group_by(sgp, rgp, sim) |>
  summarise(
    avg = mean(y, na.rm = TRUE),
    acf = tryCatch(cor(y[-n()], y[-1], use = "complete.obs"), error = \(e) NA_real_),
    vol = sd(y, na.rm = TRUE)
  ) |>
  group_by(sgp, rgp) |>
  summarise(
    avg = glue_mean(avg),
    acf = glue_mean(acf),
    vol = glue_mean(vol)
  ) |>
  mutate(r = 0)

bind_rows(diag_t2_a, diag_t2_b) |>
  pivot_wider(
    names_from = r,
    values_from = c(avg, acf, vol),
  ) |>
  relocate(rgp, sgp) |>
  arrange(rgp, sgp) |>
  mutate(
    rgp = str_replace_all(rgp, c(
      "r2_markov_symm_high" = "MS, symm.",
      "r2_markov_symm_low" = "MS, asymm.",
      "r2_threshold_x_0" = "SET, τ = 0",
      "r2_threshold_x_05" = "SET, τ = 0.5",
      "r2_stransition_l0" = "ST, τ = 0",
      "r2_stransition_l05" = "ST, τ = 0.5",
      "r2_sbreak_mid" = "SB, mid",
      "r2_sbreak_end" = "SB, end"
    )),
    sgp = str_replace_all(sgp, c(
      "r2_ar1_mu1" = "μ, (0, 0.5)",
      "r2_ar1_mu2" = "μ, (0, 2)",
      "r2_ar1_rho1" = "ρ, (0.1, 0.9)",
      "r2_ar1_rho2" = "ρ, (0.4, 0.6)",
      "r2_ar1_vol1" = "σ, (1, 2)",
      "r2_ar1_vol2" = "σ, (1, 4)"
    ))
  ) |>
  gt(rowname_col = c("rgp", "sgp"), groupname_col = NULL) |>
  cols_label(
    rgp = "RGP",
    sgp = "SGP",
    avg_1 = "s = 1",
    avg_2 = "s = 2",
    avg_0 = "⫠s",
    acf_1 = "s = 1",
    acf_2 = "s = 2",
    acf_0 = "⫠s",
    vol_1 = "s = 1",
    vol_2 = "s = 2",
    vol_0 = "⫠s",
  ) |>
  tab_spanner(label = "DGP", columns = c("rgp", "sgp")) |>
  tab_spanner(label = "Average", columns = c("avg_1", "avg_2", "avg_0")) |>
  tab_spanner(label = "ACF", columns = c("acf_1", "acf_2", "acf_0")) |>
  tab_spanner(label = "Volatility", columns = c("vol_1", "vol_2", "vol_0")) |>
  cols_align(align = "left", columns = fmt_cols) |>
  fmt(columns = fmt_cols, fns = \(x) gsub("0(\\.[0-9]|$)", "\\1", x)) |>
  gtsave("outputs/table_dgp2.tex")
  #as_latex() |>
  #clipr::write_clip()


left_join(
  estimations_data, simulations_data,
  by = c("sgp", "rgp", "sim", "t"), suffix = c("_est", "_sim")
) |>
  group_by(sgp, rgp, sim) |>
  summarise(
    fit = sum(
      y_est[t %in% 1:(n_t - n_h - 1)] > mean(y_sim, na.rm = TRUE) + 3 * sd(y_sim, na.rm = TRUE),
      na.rm = TRUE
    ) / sum(!is.na(y_est[t %in% 1:(n_t - n_h - 1)])),
    pred = sum(
      y_est[t %in% (n_t - n_h):n_t] > y_sim[t %in% (n_t - n_h):n_t] + 3 * sd(y_sim, na.rm = TRUE),
      na.rm = TRUE
    ) / sum(!is.na(y_est[t %in% (n_t - n_h):n_t]))
  ) |>
  ungroup() |>
  summarise(
    fit = mean(fit, na.rm = TRUE),
    pred = mean(pred, na.rm = TRUE)
  )



# Exploratory analysis ---------------------------------------------------------

# Metrics separation in T:
simulations_data |>
  filter(
    rgp %in% c("r2_markov_symm_high", "r2_sbreak_mid", "r2_threshold_x_0", "r2_stransition_l0"),
  ) |>
  group_by(sgp, rgp) |>
  summarise(
    avg = metrics$series_average(y, r) |> metrics$diff_p() |> round(2),
    acf = metrics$series_autocorr(y, r) |> metrics$diff_p() |> round(2),
    vol = metrics$series_volatility(y, r) |> metrics$diff_p() |> round(2)
  ) |>
  relocate(rgp, sgp) |>
  arrange(rgp, sgp) |>
  mutate(
    big_rn = c("big", "small")[grepl("2$", sgp) + 1],
    rgp = str_replace_all(rgp, c(
      "r2_markov_symm_high" = "MS, symm.",
      "r2_markov_symm_low" = "MS, asymm.",
      "r2_threshold_x_0" = "SET, τ = 0",
      "r2_threshold_x_05" = "SET, τ = 0.5",
      "r2_stransition_l0" = "ST, τ = 0",
      "r2_stransition_l05" = "ST, τ = 0.5",
      "r2_sbreak_mid" = "SB, mid",
      "r2_sbreak_end" = "SB, end"
    )),
    sgp = str_replace_all(sgp, c(
      "r2_ar1_mu1" = "μ",
      "r2_ar1_mu2" = "μ",
      "r2_ar1_rho1" = "ρ",
      "r2_ar1_rho2" = "ρ",
      "r2_ar1_vol1" = "σ",
      "r2_ar1_vol2" = "σ"
    ))
  ) |>
  pivot_wider(names_from = big_rn, values_from = c(avg, acf, vol)) |>
  gt(rowname_col = c("rgp", "sgp"), groupname_col = NULL) |>
  tab_spanner(label = "avg", columns = c("avg_small", "avg_big")) |>
  tab_spanner(label = "acf", columns = c("acf_small", "acf_big")) |>
  tab_spanner(label = "vol", columns = c("vol_small", "vol_big")) |>
  cols_label(
    rgp = "RGP",
    sgp = "SGP"
  ) |>
  gtsave("outputs/table_expl.tex")



stats <- function(y, r) {
  c(
    avg = metrics$series_average(y, r, na.rm = TRUE) |> metrics$diff_p(),
    acf = metrics$series_autocorr(y, r, use = "complete.obs") |> metrics$diff_p(),
    vol = metrics$series_volatility(y, r, na.rm = TRUE) |> metrics$diff_p()
  )
}


# Metrics separation across t:
hi = simulations_data |>
  filter(sgp %in% c("r2_ar1_mu2", "r2_ar1_rho2", "r2_ar1_vol2")) |>
  filter(sim %in% sample(n_s, 8)) |>
  group_by(rgp, sgp, sim) |>
  reframe(
    map_dfr(1:n_t, \(tmax) stats(y = y[t <= tmax], r = r[t <= tmax])),
    t = 1:n_t
  )

hi2 = hi |>
  group_by(rgp, sgp, t) |>
  reframe(
    across(c(avg, acf, vol), list(avg = ~ mean(.x, na.rm = TRUE), sd = ~ sd(.x, na.rm = TRUE)))
  ) |>
  pivot_longer(matches("^avg|^acf|^vol"), names_to = c("stat", ".value"), values_to = "value", names_sep = "_") |>
  mutate(
    big_rn = c("big", "small")[grepl("2$", sgp) + 1],
    sym_rgp = c(
      "Symm.", "Asymm."
    )[rgp %in% c("r2_markov_symm_high", "r2_sbreak_mid", "r2_threshold_x_0", "r2_stransition_l0") + 1],
    sgp = str_replace_all(sgp, c(
      "r2_ar1_mu1" = "μ",
      "r2_ar1_mu2" = "μ",
      "r2_ar1_rho1" = "ρ",
      "r2_ar1_rho2" = "ρ",
      "r2_ar1_vol1" = "σ",
      "r2_ar1_vol2" = "σ"
    )) |>
      fct(c("μ", "ρ", "σ")),
    stat = str_replace_all(stat, c(
      "avg" = "RC average",
      "acf" = "RC ACF",
      "vol" = "RC volatility"
    )) |>
      fct(c("RC average", "RC ACF", "RC volatility"))
  )

ggplot(filter(hi2, rgp %in% c("r2_threshold_x_0", "r2_threshold_x_05")), aes(t, avg)) +
  geom_line(aes(color = sym_rgp)) +
  geom_ribbon(aes(ymin = avg - sd, ymax = avg + sd, fill = sym_rgp), alpha = 0.1) +
  geom_hline(yintercept = 0) +
  xlim(10, n_t) +
  labs(color = "DGP symmetry", fill = "DGP symmetry", x = "Time", y = "Moment's dispersion") +
  ggh4x::facet_grid2(vars(sgp), vars(stat), scales = "free_y") #, independent = "y"
ggsave2("outputs/metrics/set.png", 26, 20)

ggplot(filter(hi2, rgp %in% c("r2_sbreak_mid", "r2_sbreak_end")), aes(t, avg)) +
  geom_line(aes(color = sym_rgp)) +
  geom_ribbon(aes(ymin = avg - sd, ymax = avg + sd, fill = sym_rgp), alpha = 0.1) +
  geom_hline(yintercept = 0) +
  labs(color = "DGP symmetry", fill = "DGP symmetry", x = "Time", y = "Moment's dispersion") +
  xlim(10, n_t) +
  ggh4x::facet_grid2(vars(sgp), vars(stat), scales = "free_y") #, independent = "y"
ggsave2("outputs/metrics/sb.png", 26, 20)

ggplot(filter(hi2, rgp %in% c("r2_stransition_l0", "r2_stransition_l05")), aes(t, avg)) +
  geom_line(aes(color = sym_rgp)) +
  geom_ribbon(aes(ymin = avg - sd, ymax = avg + sd, fill = sym_rgp), alpha = 0.1) +
  geom_hline(yintercept = 0) +
  labs(color = "DGP symmetry", fill = "DGP symmetry", x = "Time", y = "Moment's dispersion") +
  xlim(10, n_t) +
  ggh4x::facet_grid2(vars(sgp), vars(stat), scales = "free_y") #, independent = "y"
ggsave2("outputs/metrics/st.png", 26, 20)

ggplot(filter(hi2, rgp %in% c("r2_markov_symm_high", "r2_markov_symm_low")), aes(t, avg)) +
  geom_line(aes(color = sym_rgp)) +
  geom_ribbon(aes(ymin = avg - sd, ymax = avg + sd, fill = sym_rgp), alpha = 0.1) +
  geom_hline(yintercept = 0) +
  labs(color = "DGP symmetry", fill = "DGP symmetry", x = "Time", y = "Moment's dispersion") +
  xlim(10, n_t) +
  ggh4x::facet_grid2(vars(sgp), vars(stat), scales = "free_y") #, independent = "y"
ggsave2("outputs/metrics/ms.png", 26, 20)


# Forecasting erros and regimes:
hello = estimations_data |>
  filter(
    t >= t - n_h,
    rgp %in% c("r2_markov_symm_high", "r2_sbreak_mid", "r2_threshold_x_0", "r2_stransition_l0"),
    sgp %in% c("r2_ar1_mu2", "r2_ar1_rho2", "r2_ar1_vol2"),
  ) |>
  left_join(
    simulations_data,
    by = c("sgp", "rgp", "sim", "t"), suffix = c("_est", "_sim")
  ) |>
  mutate(
    error = y_est - y_sim,
    correct_r = c("Correct", "Incorrect")[(r_est == r_sim) + 1],
    sgp = str_replace_all(sgp, c(
      "r2_ar1_mu1" = "μ",
      "r2_ar1_mu2" = "μ",
      "r2_ar1_rho1" = "ρ",
      "r2_ar1_rho2" = "ρ",
      "r2_ar1_vol1" = "σ",
      "r2_ar1_vol2" = "σ"
    )) |>
      fct(c("μ", "ρ", "σ")),
    rgp = str_replace_all(rgp, c(
      "r2_markov_symm_high" = "MS, symm.",
      "r2_markov_symm_low" = "MS, asymm.",
      "r2_threshold_x_0" = "SET, τ = 0",
      "r2_threshold_x_05" = "SET, τ = 0.5",
      "r2_stransition_l0" = "ST, τ = 0",
      "r2_stransition_l05" = "ST, τ = 0.5",
      "r2_sbreak_mid" = "SB, mid",
      "r2_sbreak_end" = "SB, end"
    ))
  ) |>
  group_by(sgp, rgp) |>
  filter(
    error >= quantile(error, 0.0005, na.rm = TRUE),
    error <= quantile(error, 0.9995, na.rm = TRUE)
  )

ggplot(filter(hello, model == "r2_markov"), aes(x = error)) +
  geom_density(aes(color = correct_r)) +
  ggh4x::facet_grid2(vars(sgp), vars(rgp), scales = "free", independent = "all") +
  labs(y = "Density", x = "Forecasting error", color = "Regime ID")
ggsave2("outputs/metrics/forecast_regime_ms.png", 28, 20)
ggplot(filter(hello, model == "r2_threshold_x"), aes(x = error)) +
  geom_density(aes(color = correct_r)) +
  ggh4x::facet_grid2(vars(sgp), vars(rgp), scales = "free", independent = "all") +
  labs(y = "Density", x = "Forecasting error", color = "Regime ID")
ggsave2("outputs/metrics/forecast_regime_threshold.png", 28, 20)
ggplot(filter(hello, model == "r2_stransition"), aes(x = error)) +
  geom_density(aes(color = correct_r)) +
  ggh4x::facet_grid2(vars(sgp), vars(rgp), scales = "free", independent = "all") +
  labs(y = "Density", x = "Forecasting error", color = "Regime ID")
ggsave2("outputs/metrics/forecast_regime_stransition.png", 28, 20)
ggplot(filter(hello, model == "r2_sbreak"), aes(x = error)) +
  geom_density(aes(color = correct_r)) +
  ggh4x::facet_grid2(vars(sgp), vars(rgp), scales = "free", independent = "all") +
  labs(y = "Density", x = "Forecasting error", color = "Regime ID")
ggsave2("outputs/metrics/forecast_regime_sbreak.png", 28, 20)


ola = left_join(
  estimations_meta, simulations_meta,
  by = c("sgp", "rgp", "sim"), suffix = c("_est", "_sim")
) |>
  rowwise() |>
  mutate(
    error = list(rowMeans(abs(meta_est$coefs - meta_sim$coefs)))
  ) |>
  unnest_wider(error, names_sep = "_")


ola2 <- ola |>
  left_join(
    select(data_models_final, sgp, rgp, sim, rmse),
    by = c("sgp", "rgp", "sim")
  ) |>
  pivot_longer(matches("^error_"), names_to = "coef", values_to = "error") |>
  filter(
    rgp %in% c("r2_markov_symm_high", "r2_sbreak_mid", "r2_threshold_x_0", "r2_stransition_l0"),
    sgp %in% c("r2_ar1_mu2", "r2_ar1_rho2", "r2_ar1_vol2"),
  )

ola3 <- ola2 |>
  filter(
    row_number() %in% sample(n(), 10000, replace = TRUE),
    error <= 10,
    rmse <= 10
  ) |>
  ungroup() |>
  mutate(
    sgp = str_replace_all(sgp, c(
      "r2_ar1_mu1" = "RN: μ",
      "r2_ar1_mu2" = "RN: μ",
      "r2_ar1_rho1" = "RN: ρ",
      "r2_ar1_rho2" = "RN: ρ",
      "r2_ar1_vol1" = "RN: σ",
      "r2_ar1_vol2" = "RN: σ"
    )) |>
      fct(c("RN: μ", "RN: ρ", "RN: σ")),
    rgp = str_replace_all(rgp, c(
      "r2_markov_symm_high" = "MS, symm.",
      "r2_markov_symm_low" = "MS, asymm.",
      "r2_threshold_x_0" = "SET, τ = 0",
      "r2_threshold_x_05" = "SET, τ = 0.5",
      "r2_stransition_l0" = "ST, τ = 0",
      "r2_stransition_l05" = "ST, τ = 0.5",
      "r2_sbreak_mid" = "SB, mid",
      "r2_sbreak_end" = "SB, end"
    )),
    coef = str_replace_all(coef, c(
      "error_mu" = "Coef.: μ",
      "error_rho" = "Coef.: ρ",
      "error_vol" = "Coef.: σ"
    ))
  )

ggplot(filter(ola3, model == "r2_markov"), aes(error, rmse)) +
  geom_point(aes(color = rgp), alpha = 0.3) +
  ggh4x::facet_grid2(vars(coef), vars(sgp), scales = "free", independent = "all") +
  labs(y = "RMSE", x = "Average absolute error in coefficients", color = "RGP")
ggsave2("outputs/metrics/scatter_markov.png", 28, 20)
ggplot(filter(ola3, model == "r2_threshold_x"), aes(error, rmse)) +
  geom_point(aes(color = rgp), alpha = 0.3) +
  ggh4x::facet_grid2(vars(coef), vars(sgp), scales = "free", independent = "all") +
  labs(y = "RMSE", x = "Average absolute error in coefficients", color = "RGP")
ggsave2("outputs/metrics/scatter_threshold.png", 28, 20)
ggplot(filter(ola3, model == "r2_sbreak"), aes(error, rmse)) +
  geom_point(aes(color = rgp), alpha = 0.3) +
  ggh4x::facet_grid2(vars(coef), vars(sgp), scales = "free", independent = "all") +
  labs(y = "RMSE", x = "Average absolute error in coefficients", color = "RGP")
ggsave2("outputs/metrics/scatter_sbreak.png", 28, 20)
ggplot(filter(ola3, model == "r2_stransition"), aes(error, rmse)) +
  geom_point(aes(color = rgp), alpha = 0.3) +
  ggh4x::facet_grid2(vars(coef), vars(sgp), scales = "free", independent = "all") +
  labs(y = "RMSE", x = "Average absolute error in coefficients", color = "RGP")
ggsave2("outputs/metrics/scatter_stransition.png", 28, 20)



# Regressions ------------------------------------------------------------------

box::use(gtsummary[...])

trimmed_sd <- function(x, trim = 0.01) {
  idx <- x >= quantile(x, trim, na.rm = TRUE) & x <= quantile(x, 1 - trim, na.rm = TRUE)
  sd(x[idx], na.rm = TRUE)
}

data_reg <- data_models_final |>
  mutate(across(where(is.numeric), ~ ifelse(is.finite(.x), .x, NA_real_))) |>
  mutate(across(where(is.numeric), ~ ifelse(abs(.x) <= 30 * trimmed_sd(.x), .x, NA_real_))) |>
  mutate(across(where(is.numeric), ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)))

abs(data_reg$rmse) |> cut(breaks = c(0, 0.5, 1, 5, 10, 20, Inf)) |> table()


## Taking a step back:

# Diagnosticos:
lm(rmse ~ sim, data_reg) |> stargazer::stargazer()
#lm(mape ~ sim, data_reg) |> summary()


# Models FE:
lm(rmse ~ model - 1, data_reg) |> stargazer::stargazer(single.row = TRUE)
#lm(mape ~ model - 1, data_reg) |> summary()


# Models misspecifications:
reg1 <- lm(
  rmse ~ model*rgp - 1,
  filter(data_reg, rgp %in% c("r2_markov_symm_high", "r2_sbreak_mid", "r2_threshold_x_0", "r2_stransition_l0"))
) |> summary()

reg12 <- lm( # Adding assymmetry to the mix:
  rmse ~ model*rgp - 1,
  filter(data_reg, rgp %in% c("r2_markov_symm_low", "r2_sbreak_end", "r2_threshold_x_05", "r2_stransition_l05"))
) |> summary()
# Esses dois deveriam ser matrizes, mas uma coisa de cada vez

reg1$coefficients[8:16, 1] - reg12$coefficients[8:16, 1]

glue("{round(reg1$coefficients[8:16, 1], 3)} ({round(reg1$coefficients[8:16, 1], 3)})") |>
  matrix(3, 3) %>%
  cbind(c("SET", "ST", "MS"), .) %>%
  stargazer::stargazer()
glue("{round(reg12$coefficients[8:16, 1], 3)} ({round(reg12$coefficients[8:16, 1], 3)})") |>
  matrix(3, 3) %>%
  cbind(c("SET", "ST", "MS"), .) %>%
  stargazer::stargazer()



reg13 <- lm( # Adding RN to the mix:
  rmse ~ model*rgp - 1,
  filter(data_reg,
    rgp %in% c("r2_markov_symm_high", "r2_sbreak_mid", "r2_threshold_x_0", "r2_stransition_l0"),
    grepl("2$", sgp)
  )
) |> summary()

reg1$coefficients[8:16, 1] - reg13$coefficients[8:16, 1]
glue("{round(reg13$coefficients[8:16, 1], 3)} ({round(reg13$coefficients[8:16, 1], 3)})") |>
  matrix(3, 3) %>%
  cbind(c("SET", "ST", "MS"), .) %>%
  stargazer::stargazer()


# Now with the metrics instead of RGP:
reg2 <- lm(
  rmse ~ model*(avg_est + acf_est + vol_est) - 1,
  filter(data_reg, rgp %in% c("r2_markov_symm_high", "r2_sbreak_mid", "r2_threshold_x_0", "r2_stransition_l0"))
) |> summary()
# And also with cross metric interactions, but later

reg2
glue("{round(reg2$coefficients[8:16, 1], 3)} ({round(reg2$coefficients[8:16, 1], 3)})") |>
  matrix(3, 3) %>%
  cbind(c("SET", "ST", "MS"), .) %>%
  stargazer::stargazer()


reg22 <- lm(
  rmse ~ model*(avg_est * acf_est * vol_est) - 1,
  filter(data_reg, rgp %in% c("r2_markov_symm_high", "r2_sbreak_mid", "r2_threshold_x_0", "r2_stransition_l0"))
) |> summary()
# And also with cross metric interactions, but later
glue("{round(reg22$coefficients[8:16, 1], 3)} ({round(reg22$coefficients[8:16, 1], 3)})") |>
  matrix(3, 3) %>%
  cbind(c("SET", "ST", "MS"), .) %>%
  stargazer::stargazer()


# What is best to match?

reg3 <- lm(
  rmse ~ r2 + regimes_me +
    #switches_diff + duration_diff +
    #avg_diff + acf_diff + vol_diff +
    mu_diff + rho1_diff + sigma_diff +
    NULL,
  data = data_reg
)

reg3 |> stargazer::stargazer(single.row = TRUE)

# TODO: regime_me only with predictions

reg4 <- lm(
  rmse ~
    model:(avg_diff + acf_diff + vol_diff) - 1 - model +
    #mu_diff + rho1_diff + sigma_diff +
    NULL,
  data = data_reg
) |> summary()

glue("{round(reg4$coefficients[1:12, 1], 3)} ({round(reg4$coefficients[1:12, 1], 3)})") |>
  matrix(3, 4) %>%
  cbind(c("SET", "ST", "MS"), .) %>%
  stargazer::stargazer()
reg4 |> stargazer::stargazer(single.row = TRUE)

# TODO: add interactions between metrics as control

# Check:
reg1_cor <- cor(
  select(data_models_final, rmse, r2, regimes_me,
  switches_diff, duration_diff,
  avg_diff, acf_diff, vol_diff, mu_diff, rho1_diff, sigma_diff),
  use = "complete.obs"
)

reg1_cor[upper.tri(reg1_cor, diag = TRUE)] <- 0
reg1_cor >= 0.8
