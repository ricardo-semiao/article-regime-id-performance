
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
  src/utils2[...],
  src/options[dicts, params, options],
  src/diagnostics,
  src/metrics,
  src/results,
  RcppParallel[setThreadOptions], rTRNG[rnorm_trng],
  gt[gtsave],
  stargazer[stargazer],
  ggplot2[...]
)


# Simulation parameters:
n_s <- 400L # Number of simulations
n_b <- 4L # Burn-in periods
n_h <- 10L # Number of periods to predict
n_t <- 100L + n_b + n_h # Number of time periods



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
    "r2_ar1_sigma1", "r2_ar1_sigma2",
    NULL  # To correct trailing comma
  ),
  rgp = c(
    "r1_no_rs",
    #"r2_multinomial_equal", "r2_multinomial_reg1",
    "r2_markov_symm_high", "r2_markov_asymm_high",
    #"r2_markov_symm_low", "r2_markov_asymm_low",
    #"r2_sbreak_mid", "r2_sbreak_end",
    "r2_threshold_x_0", "r2_threshold_x_05",
    #"r2_threshold_abs_05", "r2_threshold_abs_2",
    #"r2_threshold_diff_05", "r2_threshold_diff_2",
    "r2_stransition_l0", "r2_stransition_l05",
    #"r2_stransition_e0", "r2_stransition_e05",
    NULL
  )
) |>
  mutate(dgp = str_c(sgp, "-", rgp))

n_p <- nrow(dgp_names)

sim_names <- expand_grid(dgp_names, sim = 1:n_s) |>
  mutate(dgp_sim = str_c(dgp, "-", sim))

simulations_meta <- params$sgps[unique(dgp_names$sgp)] |>
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
  full_join(select(dgp_names, sgp, rgp), by = "sgp")



# Simulation: Errors -----------------------------------------------------------

# Generation:
setThreadOptions(numThreads = 6L)
errors_raw <- rnorm_trng(n_t * n_p * n_s, parallelGrain = 100L)

errors <- errors_raw |>
  matrix(nrow = n_t, ncol = n_p * n_s) |>
  `colnames<-`(sim_names$dgp_sim)


# Diagnostics:
diagnostics$error_dependence(errors_raw)
if (FALSE) {
  ggsave2("outputs/diagnostics/error_dependence.pdf", 10, 0.9)
}

diagnostics$error_distribution(errors_raw)
if (FALSE) {
  ggsave2("outputs/diagnostics/error_distribution.pdf", 15, 0.4)
}



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
# Example: `input <- sim_inputs[["r2_ar1_sigma2-r2_lstar_05-14"]]`
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

  list(r = r, y = y)
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
  #write_rds2(simulations, "data/simulations.rds")
  simulations <- read_rds("data/simulations.rds")
}

# Checking errors:
if (safe) {
  compact(map(simulations, "error")) |> cli_alert_items()
  simulations <- map(simulations, "result")
}


# Collecting results:
simulations_data <- bind_rows(simulations) |>
  mutate(
    str_split(sim_names$dgp_sim, "-", n = 3) |>
      rep(each = n_t) |>
      do.call(rbind, args = _) |>
      `colnames<-`(c("sgp", "rgp", "sim")) |>
      as.data.frame(),
    r = max.col(r),
    t = rep(1:n_t, n_p * n_s)
  ) |>
  mutate(
    across(c(sgp, rgp), fct),
    across(c(sim, r), as.integer)
  ) |>
  select(sgp, rgp, sim, t, y, r)



# Estimation: Models -----------------------------------------------------------

# Used models:
dput(names(options$models))

model_names <- expand_grid(
  sim_names,
  model = c(
    "r1_no_rs",
    #"r2_sbreak",
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
est_inputs <- map2(
  simulations,
  get_varying_param(names(simulations)),
  \(sim, rn_par) list(y = sim$y, rn_par = rn_par)
)


# Estimation function:
# Example: `input <- est_inputs[["r2_ar1_rho2-r2_threshold_x_0-2"]]`
estimate_models <- function(input) {
  data <- data_lags(data.frame(y = input$y), n_l = n_l)

  results <- vector("list", n_m)
  names(results) <- names(mods)

  for (mod_name in names(mods)) {
    results[[mod_name]] <- mods[[mod_name]](data, n_t, n_h, n_b, rn_par = input$rn_par)
  }

  results
}

# Running estimations:
considered_models <- options$models[unique(model_names$model)]
safe <- TRUE
if (safe) considered_models <- map(considered_models, safely_modify)

estimations <- map_parallel(
  sample(est_inputs, 1000), estimate_models,
  mods = considered_models, data_lags = data_lags,
  n_b = n_b, n_h = n_h, n_t = n_t, n_l = n_l, n_m = n_m,
  parallel = TRUE, safe = FALSE
)

# Saving results:
if (FALSE) {
  #write_rds2(estimations, "data/estimations.rds")
  estimations <- read_rds("data/estimations.rds")
}

# Checking errors:
if (safe) {
  compact(map(estimations, ~ compact(map(.x, "error")))) |>
    cli_alert_items(flatten = TRUE)
  estimations <- map(estimations, ~ compact(map(.x, "result")))
}

# Checking regimes:
.regimes_per_model <- imap_dfr(estimations, \(sim, name) {
  c(name = name, map(sim, \(model) length(tabulate(model$r)))[])
}) |>
  pivot_longer(-name, names_to = "model") |>
  filter(
    as.integer(gsub("r([0-9]+)_.+", "\\1", model)) != value
  )

table(.regimes_per_model$model, .regimes_per_model$value)
for (l in split(.regimes_per_model, seq_len(nrow(.regimes_per_model)))) {
  estimations[[l$name]][[l$model]] <- NULL
}
estimations <- map(estimations, compact)


# Collecting results:
estimations_flat <- list_flatten(estimations, name_spec = "{outer}-{inner}")

estimations_meta <- estimations_flat |>
  purrr::imap(~ c(dgp_sim_model = .y, meta = list(list(.x$meta)))) |>
  bind_rows() |>
  separate_wider_delim(
    dgp_sim_model, delim = "-",
    names = c("sgp", "rgp", "sim", "model")
  ) |>
  mutate(
    across(c(sgp, rgp, model), fct),
    across(c(sim), as.integer)
  ) |>
  left_join(
    simulations_meta, by = c("sgp", "rgp"), suffix = c("_est", "_sim")
  )

estimations_data <- estimations_flat |>
  purrr::imap(~ c(dgp_sim_model = .y, t = list(1:n_t), .x[c("y", "r")][])) |>
  bind_rows() |>
  separate_wider_delim(
    dgp_sim_model, delim = "-",
    names = c("sgp", "rgp", "sim", "model")
  ) |>
  mutate(
    across(c(sgp, rgp, model), fct),
    across(c(sim, r), as.integer)
  )



# Estimation: Metrics ----------------------------------------------------------

metrics_data <- metrics$get_metrics_data(
  simulations_data, estimations_data, estimations_meta,
  n_t = n_t, n_b = n_b + n_l + 1, n_h = n_h
)

if (FALSE) {
  #write_rds2(metrics_data, "data/metrics_data.rds")
  metrics_data <- read_rds("data/metrics_data.rds")
}



# Results: Diagnostics ---------------------------------------------------------

# Simulation metrics:
diag_sim <- diagnostics$moments_table(simulations_data, simulations_meta,
  rgp %in% c("r2_markov_symm_high", "r2_sbreak_mid", "r2_threshold_x_0", "r2_stransition_l0"),
  sgp %in% c("r2_ar1_mu2", "r2_ar1_rho2", "r2_ar1_sigma2"),
  test = TRUE
)
diag_sim

if (FALSE) {
  gtsave(diag_sim, "outputs/diagnostics/simulations_metrics.tex")
}


# Estimations coefficients:
diag_est <- diagnostics$coefs_table(estimations_meta,
  rgp %in% c("r2_markov_symm_high", "r2_sbreak_mid", "r2_threshold_x_0", "r2_stransition_l0"),
  sgp %in% c("r2_ar1_mu2", "r2_ar1_rho2", "r2_ar1_sigma2"),
  (rgp == "r2_markov_symm_high" & model == "r2_markov") |
    (rgp == "r2_sbreak_mid" & model == "r2_sbreak") |
    (rgp == "r2_threshold_x_0" & model == "r2_threshold_x") |
    (rgp == "r2_stransition_l0" & model == "r2_stransition"),
  test = TRUE
)
diag_est

if (FALSE) {
  gtsave(diag_est, "outputs/diagnostics/estimations_coefs.tex")
}


# Improbable counts:
diag_counts <- diagnostics$improbable_counts(
  estimations_data, simulations_data,
  n_b = n_b, n_t = n_t, n_h = n_h
)
diag_counts

if (FALSE) {
  writeLines(
    imap_chr(diag_counts, ~ glue("{.y}: {formatC(.x, 8, format = 'f')}")),
    "outputs/diagnostics/improbable_counts.txt"
  )
}



# Results: Exploratory Analysis ------------------------------------------------

# Metrics separation in T:
res_met_table <- results$metrics_sep_table(simulations_data,
  #row_number() %in% sample(n(), 1000),
  rgp %in% c("r2_markov_symm_high", "r2_sbreak_mid", "r2_threshold_x_0", "r2_stransition_l0")
)
res_met_table

if (FALSE) {
  gtsave(res_met_table, "outputs/exploratory/metrics_sep_t.tex")
}


# Metrics separation across t:
res_met_graphs <- results$metrics_sep_graphs(simulations_data,
  n_t = n_t,
  row_number() %in% sample(n(), 1000),
  rgp %in% c("r2_markov_symm_high", "r2_sbreak_mid", "r2_threshold_x_0", "r2_stransition_l0")
)
res_met_graphs[[1]]

if (FALSE) {
  iwalk(res_met_graphs, \(graph, name) {
    ggsave2(plot = graph, glue("outputs/exploratory/metrics_sep_{name}.pdf"), 8, 1)
  })
}


# Forecasting errors and regimes:
res_regimes_graphs <- results$regimes_rmse_graphs(
  estimations_data, simulations_data, n_t = n_t, n_h = n_h,
  #row_number() %in% sample(n(), 1000),
  rgp %in% c("r2_markov_symm_high", "r2_sbreak_mid", "r2_threshold_x_0", "r2_stransition_l0"),
  sgp %in% c("r2_ar1_mu2", "r2_ar1_rho2", "r2_ar1_sigma2"),
  models = c("r2_markov", "r2_sbreak", "r2_threshold_x", "r2_stransition")
)
res_regimes_graphs[[1]]

if (FALSE) {
  iwalk(res_regimes_graphs, \(graph, name) {
    ggsave2(plot = graph, glue("outputs/exploratory/rmse_regimes_{name}.pdf"), 8, 1)
  })
}



# Results: Systematic Analysis -------------------------------------------------

# Setup:
colnames(metrics_data)

imap_dfr(select(metrics_data, where(is.numeric)), \(col, col_name) {
  c(
    col = col_name,
    class = class(col),
    nas = sum(is_na(col) & ! is.nan(col)),
    nans = sum(is.nan(col)),
    infs = sum(is.infinite(col)),
    outliers = sum(abs(col) > 30 * trimmed_sd(col), na.rm = TRUE)
  )
}) |>
  print(n = Inf)

sys_data <- metrics_data |>
  mutate(
    across(where(is.numeric), ~ ifelse(!is.finite(.x), NA_real_, .x)),
    across(
      c(mu_est, rho1_est, sigma_est, mu_diff, rho1_diff, sigma_diff, rmse, mape, r2),
      ~ ifelse(abs(.x) <= 30 * trimmed_sd(.x), .x, NA_real_)
    ),
    across(
      c(where(is.numeric), -sim, -rmse, -mape, -r2),
      ~ (.x - mean(.x, na.rm = TRUE)) / sd(.x, na.rm = TRUE)
    ),
    model = fct(model, unique(model_names$model)),
    rgp = fct(rgp, unique(model_names$rgp)),
    is_mis = str_replace(rgp, "(r[0-9]+_[^_]+)_.+", "\\1") != as.character(model)
  )

abs(metrics_data$rmse) |> cut(breaks = c(0, 0.5, 1, 5, 10, 20, Inf)) |> table()
abs(sys_data$rmse) |> cut(breaks = c(0, 0.5, 1, 5, 10, 20, Inf)) |> table()


# Correlations:
cor_mat <- select(sys_data,
  rmse, r2, regimes_bme,
  switches_diff, duration_diff,
  avg_diff, acf_diff, sd_diff, mu_diff, rho1_diff, sigma_diff
) |>
  cor(use = "na.or.complete")

cor_mat[upper.tri(cor_mat, diag = TRUE)] <- 0
which(cor_mat >= 0.8, arr.ind = TRUE) %>%
  {cbind(row = rownames(cor_mat)[.[, "row"]], col = colnames(cor_mat)[.[, "col"]])}


# Diagnosticos:
lm(log(rmse) ~ poly(sim, 3) + log(sim), sys_data) |> print_summary()
sys_i <- lm(rmse ~ poly(sim, 3) + log(sim), sys_data) |> print_summary()

if (FALSE) {
  format_reg_table(
    sys_i, "outputs/systematic/i.tex",
    single.row = TRUE, df = FALSE
  )
}


# Models FE:
sys_model_fe <- list()
sys_model_fe[[1]] <- lm(mape ~ model - 1, sys_data)
sys_model_fe[[2]] <- lm(
  mape ~ model - 1 + regimes_bme + switches_diff + duration_diff,
  sys_data
)
sys_model_fe[[3]] <- lm(
  mape ~ model - 1 + regimes_bme + switches_diff + duration_diff + r2,
  sys_data
)
sys_model_fe[[4]] <- lm(
  mape ~ model - 1 + regimes_bme + switches_diff + duration_diff + r2 +
    mu_diff + rho1_diff + sigma_diff,
  sys_data
)
sys_model_fe[[5]] <- lm(
  mape ~ model - 1 + regimes_bme + switches_diff + duration_diff + r2 +
    avg_diff + acf_diff + sd_diff,
  sys_data
)

if (FALSE) {
  format_reg_table(
    sys_model_fe, "outputs/systematic/model_fe.tex",
    single.row = TRUE, df = FALSE, omit.stat = c("f")
  )
}


# Models misspecifications:
mods_groups <- list(
  sym = c("r2_markov_symm_high", "r2_sbreak_mid", "r2_threshold_x_0", "r2_stransition_l0"),
  asym = c("r2_markov_symm_low", "r2_sbreak_end", "r2_threshold_x_05", "r2_stransition_l05"),
  rn = c("r2_markov_symm_high", "r2_sbreak_mid", "r2_threshold_x_0", "r2_stransition_l0")
)

sys_mis <- list()

sys_mis$is <- lm(
  rmse ~ is_mis,
  filter(sys_data, rgp %in% mods_groups$sym)
) |> print_summary()

if (FALSE) {
  format_reg_table(
    sys_mis$is, out = "outputs/systematic/mis_is.tex",
    single.row = TRUE, df = FALSE
  )
}

sys_mis$sym <- lm(
  rmse ~ model * rgp - 1,
  filter(sys_data, rgp %in% mods_groups$sym)
) |> print_summary()

if (FALSE) {
  format_reg_matrix(
    sys_mis$sym, out = "outputs/systematic/mis_sym.tex"
  )
}


sys_mis$asym <- lm(
  rmse ~ model * rgp - 1,
  filter(sys_data, rgp %in% mods_groups$asym)
) |> print_summary()

if (FALSE) {
  format_reg_matrix(
    sys_mis$asym, out = "outputs/systematic/mis_asym.tex"
  )
}


sys_mis$rn <- lm(
  rmse ~ model * rgp - 1,
  filter(sys_data, rgp %in% mods_groups$rn, grepl("2$", sgp))
) |> print_summary()

if (FALSE) {
  format_reg_matrix(
    sys_mis$rn, out = "outputs/systematic/mis_rn.tex"
  )
}


# Model mis. with true metrics:
sys_mis$metrics_true <- lm(
  rmse ~ model * (avg_true + acf_true + sd_true) - 1,
  filter(sys_data, rgp %in% mods_groups$sym)
) |> print_summary()

if (FALSE) {
  format_reg_matrix(
    sys_mis$metrics_true, out = "outputs/systematic/mis_metrics_true.tex"
  )
}

sys_mis$metrics_true_int <- lm(
  rmse ~ model * (avg_true + acf_true + sd_true) - 1,
  filter(sys_data, rgp %in% mods_groups$sym)
) |> print_summary()

if (FALSE) {
  format_reg_matrix(
    sys_mis$metrics_true_int, out = "outputs/systematic/mis_metrics_true_int.tex"
  )
}


# Model mis. with estimated metrics:
sys_mis$metrics <- lm(
  rmse ~ model * (avg_est + acf_est + sd_est) - 1,
  filter(sys_data, rgp %in% mods_groups$sym)
) |> print_summary()

if (FALSE) {
  format_reg_matrix(
    sys_mis$metrics, out = "outputs/systematic/mis_metrics.tex"
  )
}

sys_mis$metrics_int <- lm(
  rmse ~ model * (avg_est * acf_est * sd_est) - 1,
  filter(sys_data, rgp %in% mods_groups$sym)
) |> print_summary()

if (FALSE) {
  format_reg_matrix(
    sys_mis$metrics_int, out = "outputs/systematic/mis_metrics_int.tex"
  )
}


# Comparisons:
sys_mis$sym$coefficients[8:16, 1] - sys_mis$asym$coefficients[8:16, 1]
sys_mis$sym$coefficients[8:16, 1] - sys_mis$rn$coefficients[8:16, 1]
sys_mis$metrics$coefficients[8:16, 1] - sys_mis$metrics_int$coefficients[8:16, 1]


# What is best to match?
sys_match <- list()
sys_match$regimes_bme <- lm(rmse ~ regimes_bme - 1, sys_data)
sys_match$regimes_info <- lm(rmse ~ switches_diff + duration_diff - 1, sys_data)
sys_match$r2 <- lm(rmse ~ r2 - 1, sys_data)
sys_match$coefs <- lm(rmse ~ mu_diff + rho1_diff + sigma_diff - 1, sys_data)
sys_match$metrics <- lm(rmse ~ avg_diff + acf_diff + sd_diff - 1, sys_data)

if (FALSE) {
  format_reg_table(
    sys_match, out = "outputs/systematic/match.tex", type = "text",
    single.row = TRUE, df = FALSE
  )
}


sys_match$metrics_int <- lm(
  rmse ~ model:(avg_diff + acf_diff + sd_diff) - 1,
  data = sys_data
) |> print_summary()

if (FALSE) {
  format_reg_matrix(
    sys_match$metrics_int, out = "outputs/systematic/match_metrics.tex", type = "text",
    rows = 1:9
  )
}
