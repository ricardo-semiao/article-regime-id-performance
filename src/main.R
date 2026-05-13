
# Setup: Modules and Parameters ------------------------------------------------

# Debug:
if (FALSE) {
  #save.image("data/workspace.RData")
  load("data/workspace.RData")
}


# Modules:
box::purge_cache()
box::use(
  src/utils[...],
  src/options[dicts, params, options, groups],
  src/diagnostics,
  src/results,
  src/parameters[...]
)
box::use(
  RcppParallel[setThreadOptions],
  rTRNG[rnorm_trng]
)


# RNG:
set.seed(10126271)
filter_sim_i <- sample(n_i, 20)
filter_sim_i2 <- sample(n_i, 100)



# Simulation: Errors -----------------------------------------------------------

# Generation:
setThreadOptions(numThreads = 7L)
sim_errors_raw <- rnorm_trng(n_t * n_s, parallelGrain = 100L)

if (FALSE) {
  #write_rds2(sim_errors_raw, "data/errors_raw.rds")
  sim_errors_raw <- read_rds("data/errors_raw.rds")
}

sim_errors <- sim_errors_raw |>
  matrix(nrow = n_t, ncol = n_s) |>
  `colnames<-`(menu$sims$dgp_sim)


# Diagnostics:
diags <- list()

diags$g_error_dep <- diagnostics$error_dependence(sim_errors_raw, grain = 100L)
if (FALSE) {
  ggsave2("outputs/diagnostics/error_dependence.pdf", 7, 0.9, plot = diags$g_error_dep)
}

diags$g_error_dist <- diagnostics$error_distribution_sim(sim_errors_raw)
if (FALSE) {
  ggsave2("outputs/diagnostics/error_distribution.pdf", 12, 0.4, plot = diags$g_error_dist)
}



# Simulation: Series -----------------------------------------------------------

# Simulation inputs and function:
sim_inputs <- pmap(menu$sims, \(sgp, rgp, dgp, sim, dgp_sim) {
  list(
    sgp = options$sgps[[sgp]],
    rgp = options$rgps[[rgp]],
    errors = sim_errors[, dgp_sim]
  )
}) |>
  set_names(menu$sims$dgp_sim)

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

  list(r = max.col(r), y = y)
}


# Running simulations:
safe <- TRUE
if (FALSE) sim_inputs <- sim_inputs %>% .[str_split_i(names(.), "-", 3) %in% filter_sim_i]
simulations <- map_parallel(
  sim_inputs, simulate_serie,
  parallel = TRUE, safe = safe,
  setup_data = list(n_t = n_t)
)

# Saving results:
if (FALSE) {
  #write_rds2(simulations, "data/simulations.rds")
  simulations <- read_rds("data/simulations.rds")
}

# Checking errors:
if (safe) {
  compact(map(simulations, "error")) |> cli_alert_items()
  simulations <- map(simulations, "result")
}


# Collecting results: (result order is scrambled because of parallelization)
simulations_meta <- params$sgps[unique(menu$dgps$sgp)] |>
  map(\(xsgp) {
    map(xsgp$args, \(x) {
      x$mu <- x$mu %||% 0
      x$sigma <- x$sigma %||% 1
      unlist(x)
    }) |>
      do.call(rbind, args = _) |>
      `rownames<-`(paste0("R", seq_len(xsgp$n_r)))
  }) |>
  map(~ list(coefs = .x)) %>%
  {tibble(sgp = names(.), meta = .)} |>
  full_join(select(menu$dgps, sgp, rgp), by = "sgp") |>
  relocate(sgp, rgp, meta) |>
  arrange(sgp, rgp)

simulations_data <- simulations |>
  imap_dfr(~ c(sim_name = .y, .x[])) |>
  mutate( # Faster than separate
    sim_name = str_split_fixed(sim_name, "-", 3),
    sgp = sim_name[, 1], rgp = sim_name[, 2], sim = as.integer(sim_name[, 3]),
    sim_name = NULL
  ) |>
  group_by(sgp, rgp, sim) |>
  mutate(t = 1:n_t) |>
  ungroup() |>
  relocate(sgp, rgp, sim, t, r, y) |>
  arrange(sgp, rgp, sim, t)



# Diagnostics: Simulations -----------------------------------------------------

# NAs:
with(simulations_data, {
  print(glue("NAs in r: {any(is.na(r))}\nNAs in y: {any(is.na(y))}"))
})
# = Shouldn't be any


# Check sims with only 1 r:
simulations_data |>
  filter(t > n_b) |>
  group_by(sgp, rgp, sim) |>
  summarise(
    n_r_rgp = as.integer(gsub("r([0-9])+_.+", "\\1", rgp[1])),
    n_r_sim = length(unique(r))
  ) |>
  filter(n_r_sim != n_r_rgp)
# = Can exist, not a problem


# Proportions of regimes:
diags$t_regimes_sim <- diagnostics$regimes_proportions_sim(simulations_data)

if (FALSE) {
  gtsave2(diags$t_regimes_sim, "outputs/diagnostics/regimes_sim.tex")
}



# Simulation: Estimations ------------------------------------------------------

# Estimation inputs and function:
est_inputs <- map2(
  simulations,
  get_varying_param(names(simulations)),
  \(sim, rn_par) list(y = sim$y, rn_par = rn_par)
)

n_l_max <- map_dbl(options$models, ~ max(fn_env(.x)$n_l, fn_env(.x)$n_l_r)) |> max()

estimate_models <- function(input) {
  data <- data_lags(data.frame(y = input$y), n_l = n_l_max)

  results <- vector("list", n_m)
  names(results) <- names(mods)

  for (mod_name in names(mods)) {
    results[[mod_name]] <- mods[[mod_name]](data, n_t, n_b, n_h, rn_par = input$rn_par)
  }

  results
}


# Running estimations:
est_models <- options$models[c(unique(menu$ests$model))]
safe <- TRUE
if (safe) est_models <- map(est_models, safely_modify)
if (FALSE) est_inputs <- est_inputs %>% .[str_split_i(names(.), "-", 3) %in% filter_sim_i]

estimations <- map_parallel(
  est_inputs, estimate_models,
  parallel = TRUE, safe = FALSE,
  setup_packages = c("tsDyn", "MSwM", "stats", "randomForest"),
  setup_data = list(
    mods = est_models, data_lags = data_lags,
    n_b = n_b, n_h = n_h, n_t = n_t, n_l_max = n_l_max, n_m = n_m
  )
)

# Saving results:
if (FALSE) {
  #write_rds2(estimations, "data/estimations.rds")
  estimations <- read_rds("data/estimations.rds")
}

# Checking errors:
if (safe) {
  compact(map(estimations, ~ compact(map(.x, "error")))) |>
    cli_alert_items("outputs/diagnostics/estimation_errors.md", flatten = TRUE)
  estimations <- map(estimations, ~ compact(map(.x, "result")))
}


# Collecting results:
estimations_flat <- list_flatten(estimations, name_spec = "{outer}-{inner}")

estimations_meta <- estimations_flat |>
  imap_dfr(~ c(dgp_sim_model = .y, meta = list(list(.x$meta)))) |>
  mutate(
    dgp_sim_model = str_split_fixed(dgp_sim_model, "-", 4),
    sgp = dgp_sim_model[, 1], rgp = dgp_sim_model[, 2],
    sim = as.integer(dgp_sim_model[, 3]), model = dgp_sim_model[, 4],
    dgp_sim_model = NULL
  ) |>
  left_join( # * More memory usage, but avoids repeating the join
    simulations_meta,
    by = c("sgp", "rgp"), suffix = c("_est", "_sim"),
    na_matches = "never", relationship = "many-to-one", unmatched = "error"
  ) |>
  relocate(sgp, rgp, sim, model, meta_sim, meta_est) |>
  arrange(sgp, rgp, sim)

estimations_data <- estimations_flat |>
  imap_dfr(~ c(dgp_sim_model = .y, t = list(1:n_t), .x[c("y", "r")][])) |>
  mutate(
    dgp_sim_model = str_split_fixed(dgp_sim_model, "-", 4),
    sgp = dgp_sim_model[, 1], rgp = dgp_sim_model[, 2],
    sim = as.integer(dgp_sim_model[, 3]), model = dgp_sim_model[, 4],
    dgp_sim_model = NULL
  ) |>
  left_join( # * More memory usage, but avoids repeating the join
    simulations_data,
    by = c("sgp", "rgp", "sim", "t"), suffix = c("_est", "_sim"),
    na_matches = "never", relationship = "many-to-one", unmatched = "error"
  ) |>
  mutate(y_err = y_est - y_sim, r_err = r_sim != r_est) |>
  relocate(sgp, rgp, sim, model, t, r_sim, y_sim, r_est, y_est) |>
  arrange(sgp, rgp, sim, model, t)



# Diagnostics: Estimations -----------------------------------------------------

diags$obs_remove <- list()

# NAs:
diagnostics$nas_on_fit(estimations_data)
# = Shouldn't be any

diags$na_coef <- diagnostics$nas_on_coefs(estimations_meta, estimations_data)

diags$obs_remove$na_coefs <- diags$na_coef |>
  filter(model != "r1_rf", na_mu | na_rho1 | na_sigma) |>
  with(paste(sgp, rgp, sim, model, sep = "-"))
# = Can happen on n_r_est = 1 or regimes with only one obs.
# TODO: check if that's the case (see diagnostics/estimations.R)


# Fit and forecasting errors distributions:
diagnostics$erros_distribution_est(
  estimations_data,
  residuals = TRUE, lims = c(x = 20, y = 0.07), bins = 80
)
diags$high_rmse <- diagnostics$erros_distribution_est(
  estimations_data,
  residuals = FALSE, lims = c(x = 20, y = 0.03), bins = 80, cut_n = 5
)
# = Huge errors are considered estimation issues and are removed as below

diags$obs_remove$high_error <- diag_high_rmse |>
  filter(outlier) |>
  with(paste(sgp, rgp, sim, model, sep = "-"))

if (FALSE) {
  ggsave2(
    "outputs/diagnostics/residuals_distribution.pdf", 8, 0.7,
    plot = diags$g_residuals <- last_plot()
  )
  ggsave2(
    "outputs/diagnostics/forecast_errors_distribution.pdf", 8, 0.7,
    plot = diags$g_forecast <- last_plot()
  )
}


# Regimes proportions:
diags$regimes_est <- diagnostics$regimes_proportions_est(
  filter(estimations_data, ! model %in% c("r1_nors", "r1_rf")), n_l = n_l_max
)
# = Must remove obs. with n_r_est = 1 and n_smallest = 1

diags$obs_remove$few_r_obs <- diags$regimes_est |>
  filter(n_rare %in% c(0, 1)) |>
  with(paste(sgp, rgp, sim, model, sep = "-"))

if (FALSE) {
  ggsave2(
    "outputs/diagnostics/regimes_est.pdf", 8, 0.7,
    plot = diags$g_regimes <- last_plot()
  )
}


# Parameters distribution:
diagnostics$parameters_distribution(
  filter(estimations_meta, model != "r1_rf"), estimations_data, rmv_out = FALSE
)
diags$param_data <- diagnostics$parameters_distribution(
  filter(estimations_meta, model != "r1_rf"), estimations_data,
  q = 0.90, k = 20, rmv_out = TRUE
)
# = Removed (mu / rho1/ sd): 414, 195 / 190, 0 / 0, 0

diags$obs_remove$high_params <- diags$param_data |>
  filter(out) |>
  distinct(sgp, rgp, sim, model) |>
  with(paste(sgp, rgp, sim, model, sep = "-"))

if (FALSE) {
  ggsave2(
    "outputs/diagnostics/parameters_distribution.pdf", 9, 0.8,
    plot =  diags$g_params <- last_plot()
  )
}


# Other metadata distribution:
diagnostics$meta_distribution(estimations_meta)
# = Ok

# RF inportance:
estimations_meta |>
  filter(model == "r1_rf") |>
  unnest_wider(meta_est) |>
  unnest_wider(importance) |>
  summarise(across(y_l1:sd, mean))
# y_l1  y_l2  y_l3  y_l4   avg   acf    sd
# 883.  775.  697.  648.  699.  38.2  80.0

if (FALSE) {
  ggsave2(
    "outputs/diagnostics/metadata_distribution.pdf", 8, 0.7,
    plot = diags$g_meta <- last_plot()
  )
}


# Removing issues:
diags$t_obs_remove <- diagnostics$save_obs_removed(diags$obs_remove)
diags$t_obs_remove

if (FALSE) {
  gtsave2(diags$t_obs_remove, "outputs/diagnostics/estimation_issues.tex")
}

estimations_meta <- estimations_meta |>
  filter(! paste(sgp, rgp, sim, model, sep = "-") %in% reduce(diags$obs_remove, union))
estimations_data <- estimations_data |>
  filter(! paste(sgp, rgp, sim, model, sep = "-") %in% reduce(diags$obs_remove, union))


# Coefficients and true values:
diags$t_coefs <- diagnostics$coefs_table(estimations_meta,
  rgp %in% groups$rgp_sym,
  sgp %in% groups$sgp_big,
  (rgp == "r2_ms_symm_high" & model == "r2_ms") |
    (rgp == "r2_set_symm_x" & model == "r2_set") |
    (rgp == "r2_st_symm_l" & model == "r2_st")
)
# * Currently only for symmetric RGPs and big SGPs, and 'correct' model-RGP pairs

if (FALSE) {
  gtsave2(diags$t_coefs, "outputs/diagnostics/coefs_table.tex")
}


# Metrics and true values:
diags$t_metrics <- diagnostics$metrics_table(
  simulations_data, simulations_meta,
  rgp %in% groups$rgp_sym, sgp %in% groups$sgp_big
)
# * Currently only for symmetric RGPs and big SGPs

if (FALSE) {
  gtsave2(diags$t_metrics, "outputs/diagnostics/metrics_table.tex")
}


# Independence of simulation index
diags$i_independence <- estimations_data |>
  group_by(sgp, rgp, sim, model) |>
  summarise(rmse = sqrt(mean(y_err^2, na.rm = TRUE))) |>
  lm(rmse ~ poly(sim, 3) + log(sim), data = _) |> # Also consider lm(rmse) and MAPE
  print_summary()

if (FALSE) {
  results$format_reg_table(
    diags$i_independence, "outputs/diagnostics/i_independence.tex",
    single.row = TRUE, df = FALSE
  )
}



# Simulation: Metrics ----------------------------------------------------------

metrics_data <- options$metrics(estimations_data, estimations_meta)

if (FALSE) {
  #write_rds2(metrics_data, "data/metrics_data.rds")
  metrics_data <- read_rds("data/metrics_data.rds")
}



# Results: Exploratory Analysis ------------------------------------------------

res <- list()

# Metrics separation in T:
res$t_metrics_sep <- results$metrics_sep_table(simulations_data,
  sim %in% filter_sim_i2, rgp %in% groups$rgp_sym
)
# * Currently only for symmetric RGPs

if (FALSE) {
  gtsave2(res$t_metrics_sep, "outputs/exploratory/metrics_sep_t.tex")
}


# Metrics separation across t:
res$g_metrics_sep <- results$metrics_sep_graphs(simulations_data,
  sim %in% filter_sim_i,
  sgp %in% groups$sgp_big, rgp != "r1_nors"
)
# * Currently only for big SGPs and for 100 obs.

if (FALSE) {
  iwalk(res$g_metrics_sep, \(graph, name) {
    ggsave2(glue("outputs/exploratory/metrics_sep_{name}.pdf"), 8, 0.65, plot = graph)
  })
}


# Forecasting errors and regimes:
res$g_regimes_forecast <- results$regimes_rmse_graphs(
  estimations_data, simulations_data,
  sim %in% filter_sim_i2, model != "r1_rf",
  rgp %in% c("r1_nors", groups$rgp_sym), sgp %in% groups$sgp_big
)
# * Currently only for big SGPs and symmetric RGPs

if (FALSE) {
  iwalk(res$g_regimes_forecast, \(graph, name) {
    ggsave2(glue("outputs/exploratory/rmse_regimes_{name}.pdf"), 8, 0.65, plot = graph)
  })
}


# Metrics diff:
res$g_metrics_diff <- results$metrics_diff_graph(metrics_data)

if (FALSE) {
  ggsave2(
    glue("outputs/exploratory/metrics_diff.pdf"), 8, 0.65,
    plot = res$g_metrics_diff
  )
}



# Results: Systematic Analysis -------------------------------------------------

# Data:
imap_dfr(select(metrics_data, -(sgp:model)), \(col, col_name) {
  c(
    col = col_name,
    class = class(col),
    nas = sum(is_na(col) & ! is.nan(col)),
    nans = sum(is.nan(col)),
    infs = sum(is.infinite(col)),
    outliers = ifelse(grepl("_sim$", col_name), 0,
      sum(abs(col - median(col, na.rm = TRUE)) > 20 * mad(col, na.rm = TRUE), na.rm = TRUE)
    ),
    mean = round(mean(col, na.rm = TRUE), 2),
    median = round(median(col, na.rm = TRUE), 2),
    sd = round(sd(col, na.rm = TRUE), 3),
    mad = round(mad(col, na.rm = TRUE), 3)
  )
}) |>
  print(n = Inf)

reg_data <- metrics_data |>
  mutate(
    across(
      -(sgp:model),
      ~ ifelse(!is.finite(.x), NA_real_, .x)
    ),
    across(
      -c(sgp:model, -mape, ends_with("_sim")),
      ~ ifelse(abs(.x - median(.x, na.rm = TRUE)) <= 20 * mad(.x, na.rm = TRUE), .x, NA_real_)
    ),
    across(
      -c(sgp:model, rmse, mape, ends_with("_sim")), # r2 is only used as control
      ~ (.x - median(.x, na.rm = TRUE)) / mad(.x, na.rm = TRUE)
      #~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)
    ),
    is_mis = as.integer(
      str_replace(rgp, "(r[0-9]+_[^_]+)_*.*", "\\1") !=
        str_replace(as.character(model), "(r[0-9]+_[^_]+)_*.*", "\\1")
    ),
    model_r = str_replace(model, "r([0-9]+)_.+", "\\1") |> as.integer(),
    dgp_r = str_replace(rgp, "r([0-9]+)_.+", "\\1") |> as.integer(),
    model = fct(model, names(dicts$model[[1]]) %>% .[. %in% unique(menu$ests$model)]),
    rgp = fct(rgp, names(dicts$rgp[[1]]) %>% .[. %in% unique(menu$ests$rgp)]),
    sgp = fct(sgp, names(dicts$sgp[[1]]) %>% .[. %in% unique(menu$ests$sgp)])
  )

# Correlations:
select(reg_data,
  rmse, r2, regimes_bme,
  switches_diff, duration_diff,
  avg_diff, acf_diff, sd_diff, mu_diff, rho1_diff, sigma_diff
) |>
  cor(use = "na.or.complete") %>%
  `[<-`(upper.tri(., diag = TRUE), 0) |>
  round(2)


# Models FE - base:
res$models_fe <- list()

res$models_fe$none <- results$lm_clumped(
  model - 1, reg_data, c("r1_nors", "r1_rf")
)
res$models_fe$all <- results$lm_clumped(
  model - 1 + r2 + regimes_bme + switches_diff + duration_diff +
    mu_diff + rho1_diff + sigma_diff + avg_diff + acf_diff + sd_diff,
  reg_data, c("r1_nors", "r1_rf")
)
res$models_fe$r2 <- results$lm_clumped(
  model - 1 + regimes_bme + switches_diff + duration_diff +
    mu_diff + rho1_diff + sigma_diff + avg_diff + acf_diff + sd_diff,
  reg_data, c("r1_nors", "r1_rf")
)
res$models_fe$regimes <- results$lm_clumped(
  model - 1 + r2 + mu_diff + rho1_diff + sigma_diff + avg_diff + acf_diff + sd_diff,
  reg_data, c("r1_nors", "r1_rf")
)
res$models_fe$params <- results$lm_clumped(
  model - 1 + r2 + regimes_bme + switches_diff + duration_diff +
    avg_diff + acf_diff + sd_diff,
  reg_data, c("r1_nors", "r1_rf")
)
res$models_fe$metrics <- results$lm_clumped(
  model - 1 + r2 + regimes_bme + switches_diff + duration_diff +
    mu_diff + rho1_diff + sigma_diff,
  reg_data, c("r1_nors", "r1_rf")
)

if (FALSE) {
  results$format_reg_table(
    res$models_fe, "outputs/systematic/fe_base.tex",
    single.row = FALSE, no.space = TRUE, df = FALSE, omit.stat = c("f")
  )
}


# Model FE - stratified
res$model_fe_strat <- list()

res$model_fe_strat$none <- results$lm_clumped(
  model - 1, reg_data, "r1_nors"
)
res$model_fe_strat$asym <- results$lm_clumped(
  model - 1, filter(reg_data, rgp == "asymm"), "r1_nors", c("var", "var")
)
res$model_fe_strat$small <- results$lm_clumped(
  model - 1, reg_data, filter(reg_data, sgp == "1"), "r1_nors", c("var", "var")
)
res$model_fe_strat$mu <- results$lm_clumped(
  model - 1, reg_data, filter(reg_data, sgp == "r2_ar1_mu"),
  "r1_nors", c("var", "fam")
)
res$model_fe_strat$rho <- results$lm_clumped(
  model - 1, reg_data, filter(reg_data, sgp == "r2_ar1_rho"),
  "r1_nors", c("var", "fam")
)
res$model_fe_strat$sigma <- results$lm_clumped(
  model - 1, reg_data, filter(reg_data, sgp == "r2_ar1_sigma"),
  "r1_nors", c("var", "fam")
)

if (FALSE) {
  results$format_reg_table(
    res$model_fe_strat, "outputs/systematic/fe_strat.tex",
    single.row = FALSE, no.space = TRUE, df = FALSE, omit.stat = c("f")
  )
}

# How much each coef changes:
map_dfc(res$model_fe_strat, ~ .x$coefficients[1:5]) |>
  apply(1, \(x) mean(dist(x)) |> round(3))


# Misspecification - base:
res$mis_is <- list()

res$mis_is$is <- results$lm_clumped(is_mis, reg_data, c("r1_nors", "r1_rf", "r2_km"))
res$mis_is$mod <- results$lm_clumped(
  is_mis:model, reg_data, c("r1_nors", "r1_rf", "r2_km")
)
res$mis_is$rgpfam <- results$lm_clumped(
  is_mis:rgp, reg_data, c("r1_nors", "r1_rf", "r2_km"), c("fam", "all")
)
res$mis_is$sym <- results$lm_clumped(
  is_mis:rgp, filter(reg_data, rgp != "r1_nors"),
  c("r1_nors", "r1_rf", "r2_km"), c("var", "all")
)
res$mis_is$sgpfam <- results$lm_clumped(
  is_mis:sgp, reg_data, c("r1_nors", "r1_rf", "r2_km"), c("all", "fam")
)
res$mis_is$size <- results$lm_clumped(
  is_mis:sgp, reg_data, c("r1_nors", "r1_rf", "r2_km"), c("all", "var")
)

if (FALSE) {
  results$format_reg_table(
    res$mis_is, out = "outputs/systematic/mis_is.tex",
    single.row = FALSE, df = FALSE, no.space = TRUE
  )
}


# Mispecification - RGPs:
res$mis_rgp <- list()

res$mis_rgp$rgp <- results$lm_clumped(
  model * rgp - 1, reg_data, "r1_rf", c("fam", "fam")
)

if (FALSE) {
  results$format_reg_matrix(
    res$mis_rgp$rgp, out = "outputs/systematic/mis_rgp.tex",
    "^model.+:rgp", parts = c(Model = "model", RGP = "rgp")
  )
}

res$mis_rgp$full <- results$lm_clumped(
  (sgp:rgp) * model - 1 + sgp:rgp + sgp + rgp, reg_data, NULL, c("fam", "fam")
)

if (FALSE) {
  results$format_reg_matrix(
    res$mis_rgp$full, out = "outputs/systematic/mis_rgp_full.tex",
    "^sgp.+:rgp.+:model", parts = c(SGP = "sgp", RGP = "rgp", Model = "model"),
    rows = c(3, 7, 11)
  )
}


# Mispecification - metrics:
res$mis_met <- list()

res$mis_met$sim_noint <- results$lm_clumped(
  model * (avg_sim + acf_sim + sd_sim) - 1,
  reg_data, c("r1_rf"), c("fam", "fam")
)

if (FALSE) {
  results$format_reg_matrix(
    res$mis_met$sim_noint, out = "outputs/systematic/mis_metrics_sim_noint.tex",
    "^model.+:(avg|acf|sd)", parts = c(Model = "model", Metric = "metric"),
    order = c(2, 1)
  )
}

res$mis_met$sim_int <- results$lm_clumped(
  model * (avg_sim * acf_sim * sd_sim) - 1,
  reg_data, c("r1_rf"), c("fam", "fam")
)

if (FALSE) {
  results$format_reg_matrix(
    res$mis_met$sim_int, out = "outputs/systematic/mis_metrics_sim.tex",
    rows = 12:23 - 2, dim = c(4, 3),
    dimnames = list(Model = c("MS", "SET", "ST", "KM"), Metric = dicts$metrics$disp_gt)
  )
}

reg_data_est <- reg_data |>
  mutate(
    avg_est = case_when(model == "r1_nors" ~ avg_sim, TRUE ~ avg_est),
    acf_est = case_when(model == "r1_nors" ~ acf_sim, TRUE ~ acf_est),
    sd_est = case_when(model == "r1_nors" ~ sd_sim, TRUE ~ sd_est)
  )

res$mis_met$est_noint <- results$lm_clumped(
  model * (avg_est + acf_est + sd_est) - 1,
  reg_data_est, c("r1_rf"), c("fam", "fam")
)

if (FALSE) {
  results$format_reg_matrix(
    res$mis_met$est_noint, out = "outputs/systematic/mis_metrics_est_noint.tex",
    "^model.+:(avg|acf|sd)", parts = c(Model = "model", Metric = "metric"),
    order = c(2, 1)
  )
}

res$mis_met$est_int <- results$lm_clumped(
  model * (avg_est * acf_est * sd_est),
  reg_data_est, "r1_rf"
)

if (FALSE) {
  results$format_reg_matrix(
    res$mis_met$est_int, out = "outputs/systematic/mis_metrics_est.tex",
    "^model.+:(avg|acf|sd)", parts = c(Model = "model", Metric = "metric"),
    order = c(2, 1)
  )
}

res$mis_met$est_int2 <- results$lm_clumped(
  model * (avg_est * acf_est * sd_est) - 1 +
    model:(avg_est + acf_est + sd_est):sgp -
    (avg_est + acf_est + sd_est):sgp - model:sgp,
  reg_data_est, "r1_rf", c("all", "fam")
)

if (FALSE) {
  results$format_reg_matrix(
    res$mis_met$est_int2, out = "outputs/systematic/mis_metrics_est_strat.tex",
    "^model.+:(avg|acf|sd).+:sgp", parts = c(Model = "model", Metric = "metric", SGP = "sgp"),
    order = c(1, 2, 3), rows = c(2, 5)
  )
}


# Identification - base:
res$match <- list()

res$match$base <- results$lm_clumped(
  r2 + regimes_bme + switches_diff + duration_diff, reg_data
)
res$match$coefs <- results$lm_clumped(
  r2 + regimes_bme + switches_diff + duration_diff + mu_diff + rho1_diff + sigma_diff,
  reg_data
)
res$match$metrics <- results$lm_clumped(
  r2 + regimes_bme + switches_diff + duration_diff + avg_diff + acf_diff + sd_diff,
  reg_data
)

if (FALSE) {
  results$format_reg_table(
    res$match[c("base", "coefs", "metrics")], out = "outputs/systematic/match.tex",
    single.row = TRUE, df = FALSE
  )
}

res$match$r2_models <- results$lm_clumped(model:r2 - 1, reg_data)

res$match$rbme_models <- results$lm_clumped(model:regimes_bme - 1, reg_data)

if (FALSE) {
  results$format_reg_table(
    res$match[c("r2_models", "rbme_models")], out = "outputs/systematic/match_r2.tex",
    single.row = TRUE, df = FALSE
  )
}


# Identification - metrics:
res$match$metrics_models <- results$lm_clumped(
  model:(avg_diff + acf_diff + sd_diff) - 1, reg_data, c("r1_rf")
)

if (FALSE) {
  results$format_reg_matrix(
    res$match$metrics_models, out = "outputs/systematic/match_metrics.tex",
    ".+", parts = c(Model = "model", Metric = "metric")
  )
}

res$match$metrics_models_int <- results$lm_clumped(
  model * (avg_diff * acf_diff * sd_diff) - 1 +
    model:(avg_diff + acf_diff + sd_diff):sgp -
    (avg_diff + acf_diff + sd_diff):sgp - model:sgp,
  reg_data_est, "r1_rf", c("all", "fam")
)

if (FALSE) {
  results$format_reg_matrix(
    res$match$metrics_models_int, out = "outputs/systematic/match_metrics_strat.tex",
    ".+:model.+:(avg|acf|sd).+:sgp", parts = c(Model = "model", Metric = "metric", SGP = "sgp"),
    order = c(1, 2, 3), rows = c(2, 5)
  )
}


# Number of regimes:
res$regimes <- list()

res$regimes$each <- results$lm_clumped(
  (model_r < dgp_r) + (model_r > dgp_r)  +
    (model + rgp + sgp):(avg_sim + acf_sim + sd_sim),
  reg_data, c("r1_rf", "r2_st"), c("fam", "fam")
)
res$regimes$more <- results$lm_clumped(
  (model_r > dgp_r) * (avg_sim + acf_sim + sd_sim) +
    (model + rgp + sgp):(avg_sim + acf_sim + sd_sim),
  reg_data, c("r1_rf", "r2_st"), c("fam", "fam")
)
res$regimes$less <- results$lm_clumped(
  (model_r < dgp_r) * (avg_sim + acf_sim + sd_sim) +
    (model + rgp + sgp):(avg_sim + acf_sim + sd_sim),
  reg_data, c("r1_rf", "r2_st"), c("fam", "fam")
)
res$regimes$less_nokm <- results$lm_clumped(
  (model_r < dgp_r) * (avg_sim + acf_sim + sd_sim) +
    (model + rgp + sgp):(avg_sim + acf_sim + sd_sim),
  reg_data, c("r1_rf", "r2_st", "r2_km"), c("fam", "fam")
)

if (FALSE) {
  results$format_reg_table(
    res$regimes, out = "outputs/systematic/regimes.tex",
    single.row = TRUE, df = FALSE, keep = c("dgp_rTRUE", "Constant")
  )
}

res$regimes_models <- list()

res$regimes_models$more <- results$lm_clumped(
  as.integer(model_r > dgp_r):model +
    (model + rgp + sgp):(avg_sim + acf_sim + sd_sim),
  reg_data, c("r1_rf"), c("fam", "fam")
)

if (FALSE) {
  results$format_reg_table(
    res$regimes_models, out = "outputs/systematic/regimes_models.tex",
    single.row = TRUE, df = FALSE, keep = c("as.integer", "Constant")
  )
}
