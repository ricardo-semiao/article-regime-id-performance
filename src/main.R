
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
  rTRNG[rnorm_trng],
  gt[gtsave]
)


# Simulation parameters:
n_s <- 500L # Number of simulations
n_burn <- 10L # Burn-in periods
n_h <- 10L # Number of periods to predict
n_t <- 100L + n_burn + n_h # Number of time periods



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
    #"r2_multinomial_equal", "r2_multinomial_reg1",
    "r2_markov_symm_high", "r2_markov_asymm_high",
    #"r2_markov_symm_low", "r2_markov_asymm_low",
    "r2_sbreak_mid", "r2_sbreak_end",
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
errors_raw <- rnorm_trng(n_t * n_p * n_s, parallelGrain = 100)

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

considered_models <- options$models[unique(model_names$model)]

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
    results[[mod_name]] <- mods[[mod_name]](data, n_t, n_h, n_burn, rn_par = input$rn_par)
  }

  results
}

# Running estimations:
safe <- TRUE
if (safe) considered_models <- map(considered_models, safely_modify)

estimations <- map_parallel(
  est_inputs, estimate_models,
  mods = considered_models, data_lags = data_lags,
  n_burn = n_burn, n_h = n_h, n_t = n_t, n_l = n_l, n_m = n_m,
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
  #estimations$`r2_ar1_rho2-r2_threshold_x_0-415` |> map("error")
  estimations <- map(estimations, ~ compact(map(.x, "result")))
}

# Checking regimes:
check_n_regimes <- imap_dfr(estimations, \(sim, name) {
  c(name = name, map(sim, \(model) length(table(model$r)))[])
}) |>
  pivot_longer(-name, names_to = "model") |>
  filter(value != 2)

table(check_n_regimes$model, check_n_regimes$value)
for (l in split(check_n_regimes, 1:nrow(check_n_regimes))) {
  estimations[[l$name]][[l$model]] <- NULL
}
estimations <- map(estimations, compact)
rm(check_n_regimes, l)


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
  n_t = n_t, n_burn = n_burn + n_l + 1, n_h = n_h
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
  n_burn = n_burn, n_t = n_t, n_h = n_h
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

trimmed_sd <- function(x, trim = 0.01) {
  idx <- x >= quantile(x, trim, na.rm = TRUE) & x <= quantile(x, 1 - trim, na.rm = TRUE)
  sd(x[idx], na.rm = TRUE)
}

data_reg <- data_metrics |>
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
  rmse ~ model*(avg_est + acf_est + sd_est) - 1,
  filter(data_reg, rgp %in% c("r2_markov_symm_high", "r2_sbreak_mid", "r2_threshold_x_0", "r2_stransition_l0"))
) |> summary()
# And also with cross metric interactions, but later

reg2
glue("{round(reg2$coefficients[8:16, 1], 3)} ({round(reg2$coefficients[8:16, 1], 3)})") |>
  matrix(3, 3) %>%
  cbind(c("SET", "ST", "MS"), .) %>%
  stargazer::stargazer()


reg22 <- lm(
  rmse ~ model*(avg_est * acf_est * sd_est) - 1,
  filter(data_reg, rgp %in% c("r2_markov_symm_high", "r2_sbreak_mid", "r2_threshold_x_0", "r2_stransition_l0"))
) |> summary()
# And also with cross metric interactions, but later
glue("{round(reg22$coefficients[8:16, 1], 3)} ({round(reg22$coefficients[8:16, 1], 3)})") |>
  matrix(3, 3) %>%
  cbind(c("SET", "ST", "MS"), .) %>%
  stargazer::stargazer()


# What is best to match?

reg3 <- lm(
  rmse ~ r2 + regimes_bme +
    #switches_diff + duration_diff +
    #avg_diff + acf_diff + sd_diff +
    mu_diff + rho1_diff + sigma_diff +
    NULL,
  data = data_reg
)

reg3 |> stargazer::stargazer(single.row = TRUE)

reg4 <- lm(
  rmse ~
    model:(avg_diff + acf_diff + sd_diff) - 1 - model +
    #mu_diff + rho1_diff + sigma_diff +
    NULL,
  data = data_reg
) |> summary()

glue("{round(reg4$coefficients[1:12, 1], 3)} ({round(reg4$coefficients[1:12, 1], 3)})") |>
  matrix(3, 4) %>%
  cbind(c("SET", "ST", "MS"), .) %>%
  stargazer::stargazer()
reg4 |> stargazer::stargazer(single.row = TRUE)


# Check:
reg1_cor <- cor(
  select(data_metrics, rmse, r2, regimes_bme,
  switches_diff, duration_diff,
  avg_diff, acf_diff, sd_diff, mu_diff, rho1_diff, sigma_diff),
  use = "na.or.complete"
)

reg1_cor[upper.tri(reg1_cor, diag = TRUE)] <- 0
reg1_cor >= 0.8
