
# Setup: Modules and Parameters ------------------------------------------------

# box::purge_cache()
box::use(
  src/utils[...],
  src/options[...],
  diagnose = src/diagnostics
)


# Simulation parameters:
n_s <- 40L # Number of simulations
n_t <- 200L # Number of time periods
n_burn <- 20L # Burn-in periods
n_h <- 1L # Number of periods to predict



# Simulation: DGP options ------------------------------------------------------

# Used combinations:
walk(list(sgps$options, rgps$options), ~ dput(names(.x)))

dgp_names <- expand_grid(
  sgp = c(
    "r2_ar1_mu1", "r2_ar1_mu2"#,
    #"r2_ar1_rho1", "r2_ar1_rho2",
      #"r2_ar1_sign1", "r2_ar1_sign2",
      #"r2_ar2_pos1", "r2_ar2_pos2",
      #"r2_ar2_neg1", "r2_ar2_neg2",
    #"r2_ar1_vol1", "r2_ar1_vol2"
  ),
  rgp = c(
      #"r2_multinomial_equal", "r2_multinomial_reg1",
    #"r2_markov_symm_high", "r2_markov_symm_low",
    #"r2_markov_asymm_high", "r2_markov_asymm_low",
    #"r2_sbreak_mid", "r2_sbreak_end",
    "r2_threshold_x_0", "r2_threshold_x_05"#,
    #"r2_threshold_abs_05", "r2_threshold_abs_2",
    #"r2_threshold_diff_05", "r2_threshold_diff_2",
    #"r2_lstar_0", "r2_lstar_05"#,
      #"r2_estar_0", "r2_estar_05"
  )
) |>
  mutate(dgp = str_c(sgp, "-", rgp))

n_p <- nrow(dgp_names)

sim_names <- expand_grid(dgp_names, sim = 1:n_s) |>
  mutate(dgp_sim = str_c(dgp, "-", sim))



# Simulation: Errors -----------------------------------------------------------

# Error generation:
errors_raw <- rTRNG::rnorm_trng(n_t * n_p * n_s, parallelGrain = 100)

diagnose$error_dependence(errors_raw) # Todo: triangular matrix
if (FALSE) ggsave2("figures/diag_errors_dependence.png", 20, 15)

diagnose$error_distribution(errors_raw)
if (FALSE) ggsave2("figures/diag_errors_distribution.png", 20, 15)

errors <- errors_raw |>
  matrix(nrow = n_t, ncol = n_p * n_s) |>
  `colnames<-`(sim_names$dgp_sim)



# Simulation: Series -----------------------------------------------------------

# Simulation input:
# Each iteration must receive all inputs (for better parallelization)
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
simulations <- get_results(
  sim_inputs, simulate_serie,
  n_t = n_t,
  parallel = FALSE, safely = TRUE
)
# Todo: tentar fazer parallel passando apenas referencias

# Checking errors:
map(simulations, "error") |> compact() |> names()
map(simulations, "result") |> keep(~ inherits_any(.x, "try-error")) |> names()
#simulations[["r2_ar1_vol2-r2_lstar_05-14"]]



# Simulation: Diagnostics ------------------------------------------------------

# Collecting results:
simulations_data <- imap(map(simulations, "result"), \(res, sim_name) {
  sim_opts <- str_split_1(sim_name, "-")
  tibble(
    group = fct(sim_name), sgp = fct(sim_opts[1]), rgp = fct(sim_opts[2]),
    sim = as.integer(sim_opts[3]),
    t = 1:n_t, y = res$y, r = max.col(res$r)
  )
}) |>
  bind_rows() # Todo: do.call(rbind, args = _) for speed?

simulations_gdata <- diagnose$subset_simulations(
  simulations_data,
  sgps = c("r2_ar1_rho1", "r2_ar1_rho2"),
  rgps = c("r2_sbreak_mid", "r2_threshold_0")
)


diagnose$series_values(simulations_gdata, sims = 1, n_burn = n_burn)
if (FALSE) ggsave2("figures/diag_series_one.png", 20, 15)

diagnose$series_values(simulations_gdata, sims = sample(n_s, 7), n_burn = n_burn)
if (FALSE) ggsave2("figures/diag_series_mult.png", 20, 15)

diagnose$series_paths(simulations_gdata, t_max = 50, n_burn = n_burn)
if (FALSE) ggsave2("figures/diag_series_paths.png", 20, 15)

diagnose$series_distribution(simulations_gdata)
if (FALSE) ggsave2("figures/diag_series_distribution.png", 20, 15)

diagnose$regimes_values(simulations_gdata, n_burn = n_burn)
if (FALSE) ggsave2("figures/diag_regimes.png", 20, 15)

diagnose$series_stats(simulations_gdata, n_burn = n_burn, \(x) {
  c("mean" = mean(x), "vol" = sd(x))
})
if (FALSE) ggsave2("figures/diag_stat_mean.png", 20, 15)

diagnose$regimes_stats(simulations_gdata, n_burn = n_burn, \(x) {
  estimate_transmat(x, 2)[rbind(c(1, 1), c(2, 2))] |> `names<-`(c("P11", "P22"))
}) # Todo: ver como exportar estimate_transmat
if (FALSE) ggsave2("figures/diag_stat_transmat.png", 20, 15)

diagnose$regimes_stats(simulations_gdata, n_burn = n_burn, \(x) {
  table(x) %>% `names<-`(glue("N° R{names(.)}"))
})
if (FALSE) ggsave2("figures/diag_stat_nobs.png", 20, 15)



# Estimation: Model Options ----------------------------------------------------

# Used models:
dput(names(models$options))

model_names <- expand_grid(
  sim_names,
  model = c(
    "r2_sbreak",
    "r2_threshold_x"#,
    #"r2_threshold_abs",
    #"r2_threshold_diff",
    #"r2_smooth_threshold",
    #"r2_markov"
  )
) |>
  mutate(dgp_sim_model = str_c(dgp_sim, "-", model))

n_m <- length(unique(model_names$model))


# Estimation function:
# Example: `y = simulations_ys[[1]]; models = models$options`
estimate_models <- function(y_name) {
  y <- simulations_ys[[y_name]]
  data <- data.frame(y = y, y_l1 = lag(y, 1L, default = NA))
  results <- vector("list", n_m)
  names(results) <- names(models)

  for (mod_name in names(models)) {
    results[[mod_name]] <- models[[mod_name]](data, n_t, n_h)
  }

  results # Todo: transpose?
}


# Running estimations:
simulations_ys <- map(simulations, c("result", "y")) %>%
  keep_at(\(sim_names) {
    sim_names %in% model_names$dgp_sim #& str_detect(sim_names, "mu_1")
  })
# Todo: burn in

estimations <- get_results(
  set_names(names(simulations_ys)), estimate_models,
  models = map(models$options[unique(model_names$model)], safely),
  lag = lag, simulations_ys = simulations_ys,
  n_m = n_m, n_t = n_t, n_h = n_h,
  parallel = FALSE, safely = FALSE
)

# Checking errors:
map(estimations, ~ compact(map(.x, "error"))) |> compact()
map(estimations, "result") |> keep(~ inherits_any(.x, "try-error")) |> names()
#estimations[["r2_ar1_mu1-r2_markov_symm_high-1"]]



# Estimation: Diagnostics ------------------------------------------------------


# ...
estimations_data <- list_flatten(estimations, name_spec = "{outer}-{inner}") |>
  map("result") |>
  imap(\(res, sim_name) {
    sim_opts <- str_split_1(sim_name, "-")
    tibble(
      group = fct(str_c(sim_opts[1:3], collapse = "-")),
      sgp = fct(sim_opts[1]), rgp = fct(sim_opts[2]),
      sim = as.integer(sim_opts[3]), model = fct(sim_opts[4]),
      t = 1:n_t, y = res$y, r = res$r
    )
  }) |>
  bind_rows() # Todo: do.call(rbind, args = _) for speed?

estimations_meta <- list_flatten(estimations, name_spec = "{outer}-{inner}") |>
  map("result") |>
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
