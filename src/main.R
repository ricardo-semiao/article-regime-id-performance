
# Setup: Modules and Parameters ------------------------------------------------

box::purge_cache()
box::use(
  src/utils[...],
  src/utils2[...],
  src/options[dicts, params, options],
  src/diagnostics,
  src/metrics/metrics,
  rTRNG[rnorm_trng]
)


# Simulation parameters:
n_s <- 30L # Number of simulations
n_burn <- 10L # Burn-in periods
n_h <- 10L # Number of periods to predict
n_t <- 100L + n_burn + n_h # Number of time periods


# Debug:
if (FALSE) {
  save.image("personal/workspace.RData")
  load("personal/workspace.RData")
}



# Setup: DGP options -----------------------------------------------------------

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
    "r2_markov_symm_high", "r2_markov_symm_low",
    #"r2_markov_asymm_high", "r2_markov_asymm_low",
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

# Checking errors:
if (safe) {
  names(compact(map(simulations, "error"))) |> cli_alert_items()
  simulations <- map(simulations, "result")
}

# Collecting and saving results:
if (FALSE) {
  write_rds(simulations, "data/simulations.rds") %>%
    {cli$cli_alert_success("Simulations saved to {.file data/simulations.rds}")}
  simulations <- read_rds("data/simulations.rds")
}

simulations_data <- imap(simulations, \(res, sim_name) {
  sim_opts <- str_split_1(sim_name, "-")
  tibble(
    sgp = fct(sim_opts[1]), rgp = fct(sim_opts[2]),
    sim = as.integer(sim_opts[3]),
    t = 1:n_t, y = res$y, r = max.col(res$r)
  )
}) |>
  bind_rows()



# Simulation: Diagnostics ------------------------------------------------------

# Setup
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
    regime_aligned = !grepl("threshold", rgp), lims = c(0, 2)
  )
  ggsave2("outputs/simulations/stats_sgp-{rgp}.png", 28, 14)

  plot_sgps_sim(
    simulations_data, diagnostics$simulations$panel_stats,
    unique(dgp_names_main$sgp), rgp,
    dimension = "rgp", option = rgp, sims = sims_sample[1:5], n_burn = n_burn,
    regime_aligned = !grepl("threshold", rgp), lims = c(0, 15)
  )
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

# Checking errors:
if (safe) {
  names(compact(map(estimations, ~ compact(map(.x, "error"))))) |>
    cli_alert_items()
  estimations <- map(estimations, ~ map(.x, "result"))
}



# Estimations: Checks ----------------------------------------------------------

# Checking regimes:
check_n_regimes <- imap_dfr(estimations, \(sim, name) {
  c(name = name, map(sim, \(model) length(table(model$r)))[])
}) |>
  pivot_longer(-name, names_to = "model") |>
  filter(value != 2)

for (l in split(check_n_regimes, 1:nrow(check_n_regimes))) {
  estimations[[l$name]][[l$model]] <- NULL
}
estimations <- map(estimations, compact)


# Collecting and saving results:
if (FALSE) {
  write_rds(estimations, "data/estimations.rds") %>%
    {cli$cli_alert_success("Estimations saved to {.file data/estimations.rds}")}
  estimations <- read_rds("data/estimations.rds")
}

estimations_data <- list_flatten(estimations, name_spec = "{outer}-{inner}") |>
  imap(\(res, sim_name) {
    sim_opts <- str_split_1(sim_name, "-")
    meta <- res$meta
    ord <- regimes_order(meta$coefs, sim_opts[1])
    tibble(
      sgp = fct(sim_opts[1]), rgp = fct(sim_opts[2]),
      sim = as.integer(sim_opts[3]), model = fct(sim_opts[4]),
      t = 1:n_t, y = res$y, r = ord[res$r]
    )
  }) |>
  bind_rows()

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

    tibble(
      sgp = fct(sim_opts[1]), rgp = fct(sim_opts[2]),
      sim = as.integer(sim_opts[3]), model = fct(sim_opts[4]),
      meta = list(meta)
    )
  }) |>
  bind_rows()



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
  sgp <- "r2_ar1_mu2"; rgp <- "r2_markov_symm_high"; model <- "r2_markov"
  diagnostics$estimations$panel_estimations(
    subset_results(simulations_data, sgps = sgp, rgps = rgp, model = model),
    subset_results(estimations_data, sgps = sgp, rgps = rgp, model = model),
    n_burn = n_burn, n_t = n_t, n_h = n_h,
    title = glue("SGP: {dicts$sgps[sgp]}\nRGP: {dicts$rgps[rgp]}\nModel: {dicts$models[model]}")
  )
}


# Residuals panel for each RGP-Model pair (each with all SGPs within):
pwalk(distinct(model_names_main, rgp, model), \(rgp, model, ...) {
  plot_all_sgps2(
    estimations_data, simulations_data, diagnostics$estimations$panel_residuals,
    sgps = unique(model_names_main$sgp), n_burn = n_burn, n_t = n_t, n_h = n_h,
    regime_aligned = FALSE, hline = 0, title = NULL, lims = c(0, 0.4)
  ) +
    plot_annotation(
      title = glue("RGP: {dicts$rgps[rgp]};  Model: {dicts$models[model]}")
    )
  ggsave2("outputs/estimations/residuals-{rgp}-{model}.png", 28, 14)
})


# Residuals panel for each RGP-Model pair (each with all SGPs within):
pwalk(distinct(model_names, rgp, sim, model), \(rgp, model, ...) {
  data <- subset_results(
    estimations_meta, dicts, rgps = rgp, model = model
  )
  diagnostics$estimations$coefs_distribution(
    data, params, model_names
  )
  if (FALSE) ggsave2("outputs/estimations/coefs-{rgp}-{model}.png", 28, 14)
})


# Table of statistics:
.tab <- diagnostics$estimations$table_residuals(
  simulations_data, estimations_data, TRUE,
  dgp_names_main$dgp, na.rm = TRUE, n_burn = n_burn
)
if (FALSE) cat(gt::as_latex(.tab), file = "outputs/estimations/table_residuals.tex")



# Metrics ----------------------------------------------------------------------

data_models_final <- get_data_final(
  estimations_data, simulations_data, estimations_meta, "models"
)
data_models_final

data_regimes_final <- get_data_final(
  estimations_data, simulations_data, estimations_meta, "regimes"
)
data_regimes_final



# Comparisons ------------------------------------------------------------------

# Do for both data_models_final and data_regimes_final. Regimes has an extra
# regime variable


# Adjusting factor levels:
map(data_models_final[c("rgp", "sgp", "model")], levels)
reg_data <- data_models_final |>
  mutate(
    sgp = str_remove(sgp, "[^_]$"),
    rgp = str_remove(rgp, "_[^_]+$"),
  ) %>%
  filter(sgp == "r2_ar1_mu")


# First step:
# Add regressions on the identification of regime variable and metrics
# themselves


# Basic regressions:
lm(rmse ~ sim, reg_data) |> summary()
lm(rmse ~ sgp * rgp, reg_data) |> summary()
lm(rmse ~ model*rgp + sgp, reg_data) |> summary()


# Metrics must be separated by the relevant factor (SGP metrics run separately
# for each SGP, same for RGP)
lm(rmse ~ model*rgp + sgp + sgp_metric_est, reg_data) |> summary()
lm(rmse ~ model*rgp + sgp + model*sgp_metric_est, reg_data) |> summary()


# Graphs:
reg_data |>
  filter(rmse < 100) |>
  ggplot(aes(sgp_metric_est, rmse)) +
  geom_point(aes(color = model, shape = model)) +
  ggh4x::facet_grid2(vars(rgp), vars(sgp), scales = "free", independent = "all")
  #scale_x_log10() + scale_y_log10() +
  #geom_smooth(method = "lm") +
