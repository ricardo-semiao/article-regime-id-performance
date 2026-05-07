
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
  src/metrics,
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
setThreadOptions(numThreads = 6L)
sim_errors_raw <- rnorm_trng(n_t * n_s, parallelGrain = 100L)

if (FALSE) {
  #write_rds2(sim_errors_raw, "data/errors_raw.rds")
  sim_errors_raw <- read_rds("data/errors_raw.rds")
}

sim_errors <- sim_errors_raw |>
  matrix(nrow = n_t, ncol = n_s) |>
  `colnames<-`(menu$sims$dgp_sim)


# Diagnostics:
diag_g_error_dep <- diagnostics$error_dependence(sim_errors_raw)
if (FALSE) {
  ggsave2("outputs/diagnostics/error_dependence.pdf", 7, 0.9, plot = diag_g_error_dep)
}

diag_g_error_dist <- diagnostics$error_distribution_sim(sim_errors_raw)
if (FALSE) {
  ggsave2("outputs/diagnostics/error_distribution.pdf", 12, 0.4, plot = diag_g_error_dist)
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
      `rownames<-`(c("R1", "R2"))
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



# Diagnostics: Simulations ----------------------------------------------------------

diag_obs_remove <- list()

# NAs:
with(simulations_data, {
  print(glue("NAs in r: {any(is.na(r))}\nNAs in y: {any(is.na(y))}"))
})


# Remove sims with only 1 r:
simulations_data |>
  filter(t > n_b) |>
  group_by(sgp, rgp, sim) |>
  summarise(
    n_r_rgp = as.integer(gsub("r([0-9])+_.+", "\\1", rgp[1])),
    n_r_sim = length(unique(r))
  ) |>
  filter(n_r_sim != n_r_rgp)
# Currently they are not removed


# Proportions of regimes:
diag_t_regimes_sim <- diagnostics$regimes_proportions_sim(simulations_data)
diag_t_regimes_sim

if (FALSE) {
  gtsave2(diag_t_regimes_sim, "outputs/diagnostics/regimes_sim.tex")
}




# Simulation: Estimations ------------------------------------------------------

# Estimation inputs and function:
est_inputs <- map2(
  simulations,
  get_varying_param(names(simulations)),
  \(sim, rn_par) list(y = sim$y, rn_par = rn_par)
)

n_l_max <- map_dbl(options$models, ~ fn_env(.x)$n_l) |> max()

estimate_models <- function(input) {
  data <- data_lags(data.frame(y = input$y), n_l = n_l_max)

  results <- vector("list", n_m)
  names(results) <- names(mods)

  for (mod_name in names(mods)) {
    results[[mod_name]] <- mods[[mod_name]](data, n_t, n_h, n_b, rn_par = input$rn_par)
  }

  results
}


# Running estimations:
est_models <- options$models[unique(menu$ests$model)]
safe <- TRUE
if (safe) est_models <- map(est_models, safely_modify)
if (FALSE) est_inputs <- est_inputs[as.integer(str_split_i(names(est_inputs), "-", 3)) %in% filter_sim_i]

estimations <- map_parallel(
  est_inputs, estimate_models,
  parallel = TRUE, safe = FALSE,
  setup_packages = c("tsDyn", "MSwM", "stats"), # Packages used in estimation functions
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

# NAs:
diagnostics$nas_on_fit(estimations_data)
# Personal diagnostic: Ok

diag_na_coef <- diagnostics$nas_on_coefs(estimations_meta, estimations_data)
diag_obs_remove$na_coefs <- diag_na_coef |>
  filter(na_mu | na_rho1 | na_sigma) |>
  with(paste(sgp, rgp, sim, model, sep = "-"))
# Personal diagnostic: Can happen on n_r_est = 1 or regimes with only one obs.
# Must investigate if it is the case (see diagnostics/estimations.R)


# Fit and forecasting errors distributions:
diagnostics$erros_distribution_est(
  filter(estimations_data, sim %in% filter_sim_i2),
  residuals = TRUE, lims = c(x = 20, y = 0.05), bins = 80
)
diag_g_residuals <- last_plot()

if (FALSE) {
  ggsave2(
    "outputs/diagnostics/residuals_distribution.pdf", 8, 0.7,
    plot = diag_g_residuals
  )
}

diag_high_rmse <- diagnostics$erros_distribution_est(
  estimations_data,
  residuals = FALSE, lims = c(x = 20, y = 0.05), bins = 80, cut_n = 5
)
diag_g_forecast <- last_plot()
# Personal diagnostic: currently ~10k obs. outside the bounds. Hugh errors
# are considered estimation issues and are removed as below

diag_obs_remove$high_error <- diag_high_rmse |>
  filter(outlier) |>
  with(paste(sgp, rgp, sim, model, sep = "-"))

if (FALSE) {
  ggsave2(
    "outputs/diagnostics/forecast_errors_distribution.pdf", 8, 0.7,
    plot = diag_g_forecast
  )
}


# Regimes proportions:
diag_regimes_est <- diagnostics$regimes_proportions_est(
  filter(estimations_data, model != "r1_nors"), n_l = n_l_max
)
diag_g_regimes <- last_plot()
# Personal diagnostic: Must remove obs. with n_r_est = 1 and n_smallest = 1

diag_obs_remove$few_r_obs <- diag_regimes_est |>
  filter(n_rare %in% c(0, 1)) |>
  with(paste(sgp, rgp, sim, model, sep = "-"))

if (FALSE) {
  ggsave2("outputs/diagnostics/regimes_est.pdf", 8, 0.5, plot = diag_g_regimes)
}


# Parameters distribution:
diagnostics$parameters_distribution(
  estimations_meta, estimations_data, rmv_out = FALSE
)
diag_param_data <- diagnostics$parameters_distribution(
  estimations_meta, estimations_data,
  q = 0.90, k = 20, rmv_out = TRUE
)
diag_g_params <- last_plot()
# Personal diagnostic: removed (mu / rho1/ sd): 414, 195 / 190, 0 / 0, 0

diag_obs_remove$high_params <- diag_param_data |>
  filter(out) |>
  distinct(sgp, rgp, sim, model) |>
  with(paste(sgp, rgp, sim, model, sep = "-"))

if (FALSE) {
  ggsave2(
    "outputs/diagnostics/parameters_distribution.pdf", 9, 0.8,
    out =  diag_g_params
  )
}


# Other metadata distribution:
diagnostics$meta_distribution(estimations_meta)
diag_g_meta <- last_plot()
# Personal diagnostic: Ok

if (FALSE) {
  ggsave2("outputs/diagnostics/metadata_distribution.pdf", 8, 0.7, plot = diag_g_meta)
}

# Removing issues:
diag_obs_remove_table <- diagnostics$save_obs_removed(diag_obs_remove)
diag_obs_remove_table

if (FALSE) {
  gtsave2(diag_obs_remove_table, "outputs/diagnostics/estimation_issues.tex")
}

estimations_meta <- estimations_meta |>
  filter(! paste(sgp, rgp, sim, model, sep = "-") %in% reduce(diag_obs_remove, union))
estimations_data <- estimations_data |>
  filter(! paste(sgp, rgp, sim, model, sep = "-") %in% reduce(diag_obs_remove, union))



# Diagnostics: Tests -----------------------------------------------------------

# Metrics' analytical test:
diag_metrics_table <- diagnostics$metrics_table(
  simulations_data, simulations_meta,
  rgp %in% groups$rgp_sym, sgp %in% groups$sgp_big
)
diag_metrics_table
# * Currently only for symmetric RGPs and big SGPs

if (FALSE) {
  gtsave2(diag_metrics_table, "outputs/diagnostics/metrics_table.tex")
}


# Coefficients' analytical test:
diag_coefs_table <- diagnostics$coefs_table(estimations_meta,
  rgp %in% groups$rgp_sym,
  sgp %in% groups$sgp_big,
  (rgp == "r2_ms_symm_high" & model == "r2_ms") |
    (rgp == "r2_set_symm_x" & model == "r2_set_x") |
    (rgp == "r2_st_symm_l" & model == "r2_st")
)
diag_coefs_table
# * Currently only for symmetric RGPs and big SGPs, and 'correct' model-RGP pairs

if (FALSE) {
  gtsave2(diag_coefs_table, "outputs/diagnostics/coefs_table.tex")
}


# Independence of simulation index
diag_i_independence <- estimations_data |>
  group_by(sgp, rgp, sim, model) |>
  summarise(rmse = sqrt(mean(y_err^2, na.rm = TRUE))) |>
  lm(rmse ~ poly(sim, 3) + log(sim), data = _) |> # Also consider lm(rmse) and MAPE
  print_summary()

if (FALSE) {
  results$format_reg_table(
    diag_i_independence, "outputs/diagnostics/i_independence.tex",
    single.row = TRUE, df = FALSE
  )
}



# Results: Exploratory Analysis ------------------------------------------------

# Metrics separation in T:
exp_met_table <- results$metrics_sep_table(simulations_data,
  sim %in% filter_sim_i2, rgp %in% groups$rgp_sym
)
exp_met_table
# * Currently only for symmetric RGPs

if (FALSE) {
  gtsave2(exp_met_table, "outputs/exploratory/metrics_sep_t.tex")
}


# Metrics separation across t:
exp_met_graphs <- results$metrics_sep_graphs(simulations_data,
  sim %in% filter_sim_i,
  sgp %in% groups$sgp_big, rgp != "r1_nors"
)
# * Currently only for big SGPs
exp_met_graphs[[2]]

if (FALSE) {
  iwalk(exp_met_graphs, \(graph, name) {
    ggsave2(glue("outputs/exploratory/metrics_sep_{name}.pdf"), 8, 0.65, plot = graph)
  })
}


# Forecasting errors and regimes:
exp_regimes_graphs <- results$regimes_rmse_graphs(
  estimations_data, simulations_data,
  sim %in% filter_sim_i2,
  rgp %in% c("r1_nors", groups$rgp_sym), sgp %in% groups$sgp_big
)
# * Currently only for big SGPs and symmetric RGPs
exp_regimes_graphs[[2]]

if (FALSE) {
  iwalk(exp_regimes_graphs, \(graph, name) {
    ggsave2(glue("outputs/exploratory/rmse_regimes_{name}.pdf"), 8, 0.65, plot = graph)
  })
}



# Results: Systematic Analysis -------------------------------------------------

# Data:
metrics_data <- metrics$get_metrics_data(estimations_data, estimations_meta)

if (FALSE) {
  #write_rds2(metrics_data, "data/metrics_data.rds")
  metrics_data <- read_rds("data/metrics_data.rds")
}

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

sys_data <- metrics_data |>
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
    model = fct(model, unique(menu$ests$model)),
    rgp = fct(rgp, unique(menu$ests$rgp)),
    sgp = fct(sgp, unique(menu$ests$sgp))
  )

#na.omit(sys_data) |> nrow()
#with(sys_data, hist(avg_est, breaks = 40))

# Correlations:
sys_cor_mat <- select(sys_data,
  rmse, r2, regimes_bme,
  switches_diff, duration_diff,
  avg_diff, acf_diff, sd_diff, mu_diff, rho1_diff, sigma_diff
) |>
  cor(use = "na.or.complete")

sys_cor_mat[upper.tri(sys_cor_mat, diag = TRUE)] <- 0
which(sys_cor_mat >= 0.2, arr.ind = TRUE) %>%
  {cbind(
    row = rownames(sys_cor_mat)[.[, "row"]],
    col = colnames(sys_cor_mat)[.[, "col"]]
  )}


# Models FE - Base:
sys_model_fe <- list()

sys_model_fe$none <- lm(
  rmse ~ model - 1,
  filter(sys_data, model != "r1_nors")
)
sys_model_fe$all <- lm(
  rmse ~ model - 1 + r2 + regimes_bme + switches_diff + duration_diff +
    mu_diff + rho1_diff + sigma_diff + avg_diff + acf_diff + sd_diff,
  filter(sys_data, model != "r1_nors")
)
sys_model_fe$r2 <- lm(
  rmse ~ model - 1 + regimes_bme + switches_diff + duration_diff +
    mu_diff + rho1_diff + sigma_diff + avg_diff + acf_diff + sd_diff,
  filter(sys_data, model != "r1_nors")
)
sys_model_fe$regimes <- lm(
  rmse ~ model - 1 + r2 +
    mu_diff + rho1_diff + sigma_diff + avg_diff + acf_diff + sd_diff,
  filter(sys_data, model != "r1_nors")
)
sys_model_fe$params <- lm(
  rmse ~ model - 1 + r2 + regimes_bme + switches_diff + duration_diff +
    avg_diff + acf_diff + sd_diff,
  filter(sys_data, model != "r1_nors")
)
sys_model_fe$metrics <- lm(
  rmse ~ model - 1 + r2 + regimes_bme + switches_diff + duration_diff +
    mu_diff + rho1_diff + sigma_diff,
  filter(sys_data, model != "r1_nors")
)

if (FALSE) {
  results$format_reg_table(
    sys_model_fe, "outputs/systematic/fe_base.tex",
    single.row = FALSE, no.space = TRUE, df = FALSE, omit.stat = c("f")
  )
}

# sys_model_fe$rgp <- lm(
#   rmse ~ model * rgp - 1,
#   sys_data |> clump_dgps()
# ) |> print_summary()

# if (FALSE) {
#   results$format_reg_matrix(
#     sys_model_fe$rgp, "outputs/systematic/fe_rgp.tex", FALSE
#   )
# }


# Model FE - stratified
# Models FE - Base:
sys_model_fe_strat <- list()

sys_model_fe_strat$none <- lm(
  rmse ~ model - 1,
  filter(sys_data, model != "r1_nors")
)
sys_model_fe_strat$asym <- lm(
  rmse ~ model - 1,
  sys_data |> clump_dgps("var", "var") |> filter(rgp == "asymm", model != "r1_nors")
)
sys_model_fe_strat$small <- lm(
  rmse ~ model - 1,
  sys_data |> clump_dgps("var", "var") |> filter(sgp == "1", model != "r1_nors")
)
sys_model_fe_strat$mu <- lm(
  rmse ~ model - 1,
  sys_data |> clump_dgps("var", "fam") |> filter(sgp == "r2_ar1_mu", model != "r1_nors")
)
sys_model_fe_strat$rho <- lm(
  rmse ~ model - 1,
  sys_data |> clump_dgps("var", "fam") |> filter(sgp == "r2_ar1_mu", model != "r1_nors")
)
sys_model_fe_strat$sigma <- lm(
  rmse ~ model - 1,
  sys_data |> clump_dgps("var", "fam") |> filter(sgp == "r2_ar1_sigma", model != "r1_nors")
)

if (FALSE) {
  results$format_reg_table(
    sys_model_fe_strat, "outputs/systematic/fe_strat.tex",
    single.row = FALSE, no.space = TRUE, df = FALSE, omit.stat = c("f")
  )
}

# Models FE - appendix:
#...


# Misspecification - base:
sys_mis_is <- list()

sys_mis_is$is <- lm(
  rmse ~ is_mis,
  sys_data
) |> print_summary()
sys_mis_is$mod <- lm(
  rmse ~ is_mis : model,
  sys_data
) |> print_summary()
sys_mis_is$rgpfam <- lm(
  rmse ~ is_mis:rgp,
  sys_data |> clump_dgps("fam", "all")
) |> print_summary()
sys_mis_is$sym <- lm(
  rmse ~ is_mis:rgp,
  sys_data |> filter(rgp != "r1_nors") |> clump_dgps("var", "all")
) |> print_summary()
sys_mis_is$sgpfam <- lm(
  rmse ~ is_mis:sgp,
  sys_data |> clump_dgps("all", "fam")
) |> print_summary()
sys_mis_is$size <- lm(
  rmse ~ is_mis:sgp,
  sys_data |> clump_dgps("all", "var")
) |> print_summary()

if (FALSE) {
  results$format_reg_table(
    sys_mis_is, out = "outputs/systematic/mis_is_sgp.tex",
    single.row = FALSE, df = FALSE, no.space = TRUE
  )
}
# if (FALSE) {
#   results$format_reg_table(
#     sys_mis_is[c("mod", "rgpfam", "sym")], out = "outputs/systematic/mis_is_rgp.tex",
#     single.row = FALSE, df = FALSE, no.space = TRUE
#   )
# }



# Mispecification - RGPs:
sys_mis_rgp <- list()

sys_mis_rgp$rgp <- lm(
  rmse ~ model * rgp - 1,
  sys_data |> clump_dgps()
) |> print_summary()

if (FALSE) {
  results$format_reg_matrix(
    sys_mis_rgp$rgp, out = "outputs/systematic/mis_rgp.tex"
  )
}

sys_mis_rgp$full <- lm(
  rmse ~ (sgp:rgp) * model - 1 + sgp:rgp + sgp + rgp,
  sys_data |> clump_dgps() #|> filter(! (rgp == "r1_nors" & model != "r2_st"))
) |> print_summary()

if (FALSE) {
  results$format_reg_matrix2(
    sys_mis_rgp$full, out = "outputs/systematic/mis_rgp_full.tex"
  )
}


# Mispecification - metrics:
sys_mis_met <- list()

# sys_mis$metrics_sim <- lm(
#   rmse ~ model * (avg_sim + acf_sim + sd_sim) - 1,
#   sys_data |> clump_dgps()
# ) |> print_summary()

# if (FALSE) {
#   results$format_reg_matrix(
#     sys_mis$metrics_sim, out = "outputs/systematic/mis_metrics_sim.tex",
#     dimnames = list(Model = c("SET", "ST", "MS"), Metric = c("Avg", "ACF", "SD"))
#   )
# }

sys_mis_met$sim_int <- lm(
  rmse ~ model * (avg_sim * acf_sim * sd_sim) - 1,
  sys_data |> clump_dgps()
) |> print_summary()
#with(sys_data |> clump_dgps(), table(avg_sim, acf_sim))

if (FALSE) {
  results$format_reg_matrix(
    sys_mis_met$sim_int, out = "outputs/systematic/mis_metrics_sim.tex",
    dimnames = list(Model = c("SET", "ST", "MS"), Metric = c("Avg", "ACF", "SD"))
  )
}

sys_data_est <- sys_data |>
  mutate(
    avg_est = case_when(model == "r1_nors" ~ avg_sim, TRUE ~ avg_est),
    acf_est = case_when(model == "r1_nors" ~ acf_sim, TRUE ~ acf_est),
    sd_est = case_when(model == "r1_nors" ~ sd_sim, TRUE ~ sd_est)
  )

# sys_mis$metrics <- lm(
#   rmse ~ model * (avg_est + acf_est + sd_est) - 1,
#   sys_data_est |> clump_dgps()
# ) |> print_summary()

# if (FALSE) {
#   results$format_reg_matrix(
#     sys_mis$metrics, out = "outputs/systematic/mis_metrics.tex",
#     dimnames = list(Model = c("SET", "ST", "MS"), Metric = c("Avg", "ACF", "SD"))
#   )
# }

sys_mis_met$est_int <- lm(
  rmse ~ model * (avg_est * acf_est * sd_est) - 1,
  sys_data_est
) |> print_summary()

if (FALSE) {
  results$format_reg_matrix(
    sys_mis_met$est_int, out = "outputs/systematic/mis_metrics_est.tex",
    dimnames = list(Model = c("SET", "ST", "MS"), Metric = c("Avg", "ACF", "SD"))
  )
}

# sys_mis_met$est_int <- lm(
#   rmse ~ model * (avg_est * acf_est * sd_est) - 1 + sgp + rgp,
#   sys_data_est |> clump_dgps("var", "all")
# ) |> print_summary()


# Misspecification - Appendix:
#...


# Identification - base:
sys_match <- list()
sys_match$r2 <- lm(rmse ~ r2 - 1, sys_data)
sys_match$regimes <- lm(rmse ~ regimes_bme + switches_diff + duration_diff - 1, sys_data)
sys_match$coefs <- lm(rmse ~ mu_diff + rho1_diff + sigma_diff - 1, sys_data)
sys_match$metrics <- lm(rmse ~ avg_diff + acf_diff + sd_diff - 1, sys_data)

sys_match$base <- lm(rmse ~ r2 + regimes_bme + switches_diff + duration_diff, sys_data)
sys_match$coefs <- lm(
  rmse ~ r2 + regimes_bme + switches_diff + duration_diff +
    mu_diff + rho1_diff + sigma_diff,
  sys_data
)
sys_match$coefs <- lm(
  rmse ~ r2 + regimes_bme + switches_diff + duration_diff +
    avg_diff + acf_diff + sd_diff,
  sys_data
)

if (FALSE) {
  results$format_reg_table(
    sys_match, out = "outputs/systematic/match.tex",
    single.row = TRUE, df = FALSE
  )
}

sys_match$r2_models <- lm(
  rmse ~ model:r2 - 1,
  data = sys_data
) |> print_summary()

if (FALSE) {
  results$format_reg_table(
    sys_match$r2_models, out = "outputs/systematic/match_r2.tex",
    single.row = TRUE, df = FALSE
  )
}

sys_match$metrics_models <- lm(
  rmse ~ model:(avg_diff + acf_diff + sd_diff) - 1,
  data = sys_data
) |> print_summary()

if (FALSE) {
  results$format_reg_matrix(
    sys_match$metrics_models, out = "outputs/systematic/match_metrics.tex",
    rows = 1:9
  )
}
