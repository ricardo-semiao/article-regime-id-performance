
# Setup: Modules and Parameters ------------------------------------------------

# box::purge_cache()
box::use(
  src/utils[...],
  src/options[sgps, rgps, models],
  src/diagnostics,
  src/others/metrics,
  rTRNG[rnorm_trng]
)


# Simulation parameters:
n_s <- 30L # Number of simulations
n_t <- 100L # Number of time periods
n_burn <- 20L # Burn-in periods
n_h <- 1L # Number of periods to predict


# Debug:
#load("personal/workspace.RData")



# Setup: DGP options -----------------------------------------------------------

# Used combinations:
walk(list(sgps$options, rgps$options), ~ dput(names(.x)))

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
    "r2_lstar_0", "r2_lstar_05",
      #"r2_estar_0", "r2_estar_05",
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

# Simulation input:
sim_inputs <- pmap(sim_names, \(sgp, rgp, dgp, sim, dgp_sim) {
  list(
    sgp = sgps$options[[sgp]],
    rgp = rgps$options[[rgp]],
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

  list(r = r, y = y) # Todo:summarize r into vector might be needed for memory
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
  map(simulations, "error") |> compact() |> names()
  map(simulations, "result") |> keep(~ inherits_any(.x, "try-error")) |> names()
  #simulations[["r2_ar1_vol2-r2_lstar_05-14"]]

  simulations <- map(simulations, "result")
}

# Collecting and saving results:
simulations_data <- imap(simulations, \(res, sim_name) {
  sim_opts <- str_split_1(sim_name, "-")
  tibble(
    sgp = fct(sim_opts[1]), rgp = fct(sim_opts[2]),
    sim = as.integer(sim_opts[3]),
    t = 1:n_t, y = res$y, r = max.col(res$r)
  )
}) |>
  bind_rows()

if (FALSE) write_rds(simulations, "data/simulations.rds")



# Simulation: Diagnostics ------------------------------------------------------

# Graphs:
walk(1:nrow(dgp_names), \(i) {
  sgp_name <- dgp_names$sgp[i]
  rgp_name <- dgp_names$rgp[i]
  sims_sample <- sample(n_s, 10)

  data <- diagnostics$simulations$subset_simulations(
    simulations_data, sgps = sgp_name, rgps = rgp_name
  )

  diagnostics$simulations$panel_simulations(data,  n_burn = n_burn)
  if (FALSE) ggsave2("outputs/simulations/values-{sgp}-{rgp}.png", 28, 14)

  diagnostics$simulations$panel_stats(
    data, "sgp", sgp_name, sims = sims_sample[1:5], n_burn = n_burn,
    regime_aligned = !grepl("threshold", rgp_name)
  )
  if (FALSE) ggsave2("outputs/simulations/stats_sgp-{sgp}-{rgp}.png", 28, 14)

  diagnostics$simulations$panel_stats(
    data, "rgp", rgp_name, sims = sims_sample[1:5], n_burn = n_burn,
    regime_aligned = !grepl("threshold", rgp_name)
  )
  if (FALSE) ggsave2("outputs/simulations/stats_rpg-{sgp}-{rgp}.png", 28, 14)
})


# Table of statistics:
.tab <- diagnostics$simulations$table_sgps(simulations_data)
if (FALSE) cat(gt::as_latex(.tab), file = "outputs/simulations/table_sgps.tex")

.tab <- diagnostics$simulations$table_rgps(simulations_data)
if (FALSE) cat(gt::as_latex(.tab), file = "outputs/simulations/table_rgps.tex")
rm(.tab)



# Estimation: Model Options ----------------------------------------------------

# Used models:
dput(names(models$options))

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

n_m <- length(models$options_names)

# Estimation function:
# Example: `y = simulations_ys[[1]]; models = models$options`
estimate_models <- function(y_name) {
  y <- simulations_ys[[y_name]]
  data <- data.frame(y = y, y_l1 = lag(y, 1L, default = NA))
  # Todo: generalize for n_p > 1

  results <- vector("list", n_m)
  names(results) <- names(models)

  for (mod_name in names(models)) {
    results[[mod_name]] <- models[[mod_name]](data, n_t, n_h)
  }

  results # Todo: transpose?
}


# Running estimations:
simulations_ys <- map(simulations, "y") %>%
  keep_at(\(sim_names) {
    sim_names %in% model_names$dgp_sim #& str_detect(sim_names, "mu_1")
  })
# Todo: burn in

estimations <- map_parallel(
  set_names(names(simulations_ys)), estimate_models,
  models = map(models$options[unique(model_names$model)], safely),
  lag = lag, simulations_ys = simulations_ys,
  n_m = n_m, n_t = n_t, n_h = n_h,
  parallel = TRUE, safe = FALSE
)

# Checking errors:
map(estimations, ~ compact(map(.x, "error"))) |> compact()
map(estimations, "result") |> keep(~ inherits_any(.x, "try-error")) |> names()
#estimations[["r2_ar1_mu1-r2_markov_symm_high-1"]]


# Collecting and saving results:
if (FALSE) write_rds(estimations, "data/estimations.rds")

estimations_data <- list_flatten(estimations, name_spec = "{outer}-{inner}") |>
  map("result") |>
  imap(\(res, sim_name) {
    sim_opts <- str_split_1(sim_name, "-")
    tibble(
      sgp = fct(sim_opts[1]), rgp = fct(sim_opts[2]),
      sim = as.integer(sim_opts[3]), model = fct(sim_opts[4]),
      t = 1:n_t, y = res$y, r = res$r
    )
  }) |>
  bind_rows()

estimations_meta <- list_flatten(estimations, name_spec = "{outer}-{inner}") |>
  map("result") |>
  imap(\(res, sim_name) {
    sim_opts <- str_split_1(sim_name, "-")
    tibble(
      sgp = fct(sim_opts[1]), rgp = fct(sim_opts[2]),
      sim = as.integer(sim_opts[3]), model = fct(sim_opts[4]),
      meta = list(res$meta)
    )
  }) |>
  bind_rows()



# Estimation: Diagnostics ------------------------------------------------------

a = estimations_meta %>%
  #filter(str_detect(model, "^r2_")) %>%
  filter(str_detect(rgp, as.character(model))) %>%
  unnest_wider(meta) %>%
  rowwise() %>%
  mutate(coefs = list(asplit(coefs, 2, TRUE))) %>%
  ungroup() %>%
  unnest_wider(coefs, names_sep = "_") %>%
  unnest_wider(starts_with("coefs_"), names_sep = "_R")

correct <- tribble(
  ~ sgp, ~ coef, ~ regime, ~ value,
  "r2_ar1_mu1", "Intercept", "R1", 0,
  "r2_ar1_mu1", "Intercept", "R2", 0.5,
  "r2_ar1_mu2", "Intercept", "R1", 0,
  "r2_ar1_mu2", "Intercept", "R2", 2,
  "r2_ar1_mu1", "AR(1)", "R1", 0.5,
  "r2_ar1_mu1", "AR(1)", "R2", 0.5,
  "r2_ar1_mu2", "AR(1)", "R1", 0.5,
  "r2_ar1_mu2", "AR(1)", "R2", 0.5,
)

a %>%
  #filter(str_detect(sgp, "mu1")) %>%
  pivot_longer(
    starts_with("coefs_"),
    names_pattern = "coefs_(.+)_(R[0-9]+)",
    names_to = c("coef", "regime"),
    values_to = "value"
  ) %>%
  ggplot(aes(x = value, color = regime)) +
  geom_density() +
  #geom_histogram(aes(fill = regime), position = "identity", alpha = 0.3, bins = 20) +
  geom_vline(data = correct, aes(xintercept = value), linetype = "dashed") +
  ggh4x::facet_grid2(vars(sgp), vars(coef), scales = "free", independent = "y") +
  ggh4x::facetted_pos_scales(
    x = list(
      `AR(1)` = scale_x_continuous(limits = c(-0.5, 1.5)),
      `Intercept` = scale_x_continuous(limits = c(-4, 4))
    )
  )






estimations_data %>%
  filter(group == "r2_ar1_mu1-r2_markov_symm_high-1") %>%
  ggplot(aes(t, y, color = factor(r), group = NA)) +
  geom_line() +
  facet_wrap(vars(model))

  mutate(coefs = asplit(coefs, 2))
estimations_data$meta[[1]]
asplit



a <- list_flatten(estimations, name_spec = "{outer}-{inner}") |>
  map("result")

b <- map(a, \(x) {
  coe <- x$meta$coefs
  ord <- order(coe[, "Intercept"])
  x$meta$coefs <- coe[ord, ]
  x$r <- setNames(1:2, ord)[x$r]
  x
})


estimations_meta <- b |>
  imap(\(res, sim_name) {
    sim_opts <- str_split_1(sim_name, "-")
    tibble(
      group = fct(str_c(sim_opts[1:3], collapse = "-")),
      sgp = fct(sim_opts[1]), rgp = fct(sim_opts[2]),
      sim = as.integer(sim_opts[3]), model = fct(sim_opts[4]),
      meta = list(res$meta)
    )
  }) |>
  bind_rows() # Todo: do.call(rbind, args = _) for speed?

gdata = estimations_meta %>%
  #filter(str_detect(model, "^r2_")) %>%
  filter(str_detect(rgp, as.character(model))) %>%
  unnest_wider(meta) %>%
  rowwise() %>%
  mutate(coefs = list(asplit(coefs, 2, TRUE))) %>%
  ungroup() %>%
  unnest_wider(coefs, names_sep = "_") %>%
  unnest_wider(starts_with("coefs_"), names_sep = "_R")

correct <- tribble(
  ~ sgp, ~ coef, ~ regime, ~ value,
  "r2_ar1_mu1", "Intercept", "R1", 0,
  "r2_ar1_mu1", "Intercept", "R2", 0.5,
  "r2_ar1_mu2", "Intercept", "R1", 0,
  "r2_ar1_mu2", "Intercept", "R2", 2,
  "r2_ar1_mu1", "AR(1)", "R1", 0.5,
  "r2_ar1_mu1", "AR(1)", "R2", 0.5,
  "r2_ar1_mu2", "AR(1)", "R1", 0.5,
  "r2_ar1_mu2", "AR(1)", "R2", 0.5,
)

gdata %>%
  #filter(str_detect(sgp, "mu1")) %>%
  pivot_longer(
    starts_with("coefs_"),
    names_pattern = "coefs_(.+)_(R[0-9]+)",
    names_to = c("coef", "regime"),
    values_to = "value"
  ) %>%
  ggplot(aes(x = value, color = regime)) +
  geom_density() +
  #geom_histogram(aes(fill = regime), position = "identity", alpha = 0.3, bins = 20) +
  geom_vline(data = correct, aes(xintercept = value), linetype = "dashed") +
  ggh4x::facet_grid2(vars(sgp), vars(coef), scales = "free", independent = "y") +
  ggh4x::facetted_pos_scales(
    x = list(
      `AR(1)` = scale_x_continuous(limits = c(-0.5, 1.5)),
      `Intercept` = scale_x_continuous(limits = c(-4, 4))
    )
  )


a[[1]]
simulations_ys[["r2_ar1_mu1-r2_threshold_x_0-1"]]
n_r_hat <- 2

rmv_idxs <- is.na(a[[1]]$y) | is.na(a[[1]]$r) | (! a[[1]]$r %in% (1:n_r_hat))

y = a[[1]]$y[!rmv_idxs]
r = a[[1]]$r[!rmv_idxs]




map_dfr(a, function(x) {
  rmv_idxs <- is.na(x$y) | is.na(x$r) | (! x$r %in% (1:n_r_hat))
  y = x$y[!rmv_idxs]
  r = x$r[!rmv_idxs]

  c(
    average(y, r, n_r_hat) |> setNames(paste0("avg_r", 1:n_r_hat)),
    c(x$meta$coefs) |> setNames(paste0(
      c("Intercept", paste0("AR(", 1:(ncol(x$meta$coefs)), ")")),
      rep(paste0("_r", 1:n_r_hat), each = ncol(x$meta$coefs))
    ))
  )
})



# Metrics ----------------------------------------------------------------------


n_r_hat <- 2
sim_name <- names(simulations_ys)[[1]]
sim_results <- estimations[[sim_name]]
model_result <- estimations[[sim_name]]$r2_threshold_x

metrics_data <- imap(estimations, function(sim_results, sim_name) {
  y_true <- simulations_ys[[sim_name]]

  imap(sim_results, \(model_result, model_name) {
    x <- model_result$result

    rmv_idxs <- is.na(x$y) | is.na(x$r) | (! x$r %in% (1:n_r_hat))
    y <- x$y[!rmv_idxs]
    r <- x$r[!rmv_idxs]

    list(
      "dgp_sim" = sim_name,
      "model" = model_name,
      rmse = rmse(x$y, y_true, n_h, n_t),
      average_dist = average(y, r, n_r_hat) |> mean_pairwise_dist(),
      average_sd = average(y, r, n_r_hat) |> sd()
    )
  })
}) |>
  list_flatten() |>
  bind_rows()

metrics_data <- left_join(
  metrics_data,
  select(sim_names, dgp_sim, sgp, rgp, sim),
  by = "dgp_sim"
)



# Comparisons ------------------------------------------------------------------

map(metrics_data[c("rgp", "sgp", "model")], unique)

lm(rmse ~ sim, metrics_data) |> summary()
lm(rmse ~ sgp, metrics_data) |> summary()
lm(rmse ~ rgp, metrics_data) |> summary()
lm(rmse ~ sgp * rgp, metrics_data) |> summary()

lm(rmse ~ model, metrics_data) |> summary()
lm(rmse ~ model + sgp + rgp, metrics_data) |> summary()
lm(rmse ~ model*rgp + sgp, metrics_data) |> summary()
#lm(rmse ~ model*rgp*sgp, metrics_data) |> summary()

lm(rmse ~ average_sd, metrics_data) |> summary()
lm(rmse ~ model*rgp + sgp + average_sd, metrics_data) |> summary()
lm(rmse ~ model*rgp + sgp + model*average_sd, metrics_data) |> summary()

ggplot(metrics_data, aes(average, rmse, color = model)) +
  geom_point() #+
  #geom_smooth(method = "lm") +
  #ggh4x::facet_grid2(vars(rgp), vars(sgp), scales = "free", independent = "all")
