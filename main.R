
# Setup: Packages and Functions ------------------------------------------------

# box::purge_cache()
box::use(
  mirai[...],
  src/utils[...],
  src/options,
  diagnose = src/diagnostics
)



# Setup: Parameters and DGPs ---------------------------------------------------

# Simulation parameters:
n_s <- 30L # Number of simulations
n_t <- 100L # Number of time periods
n_burn <- 20L # Burn-in periods
n_h <- 1L # Number of periods to predict


# Used combinations:
map(list(options$sgps$options, options$rgps$options), names)

dgp_names <- expand_grid(
  sgp = c("r2_ar1_rho1", "r2_ar1_rho2", "r2_ar1_mu1", "r2_ar1_mu2"),
  rgp = names(options$rgps$options)
) |>
  mutate(dgp = str_c(sgp, "-", rgp))

n_p <- nrow(dgp_names)

sim_names <- expand_grid(dgp = dgp_names$dgp, sim = 1:n_s) |>
  pmap_chr(~ str_c(..1, "-", ..2))



# Simulation: Errors -----------------------------------------------------------

# Error generation:
errors_raw <- rTRNG::rnorm_trng(n_t * n_p * n_s, parallelGrain = 100)

diagnose$error_dependence(errors_raw)
if (FALSE) ggsave2("figures/diag_errors_dependence.png", 20, 15)

diagnose$error_distribution(errors_raw)
if (FALSE) ggsave2("figures/diag_errors_distribution.png", 20, 15)

errors <- errors_raw |>
  matrix(nrow = n_t, ncol = n_p * n_s) |>
  `colnames<-`(sim_names)



# Simulation: Series -----------------------------------------------------------

# Simulation input:
# Each iteration must receive all inputs (for better parallelization)
sim_inputs <- map(set_names(sim_names), \(opts) {
  opts_split <- str_split_1(opts, "-")
  list(
    sgp = options$sgps$options[[opts_split[1]]],
    rgp = options$rgps$options[[opts_split[2]]],
    errors = errors[, opts]
  )
})


# Simulation function:
simulate_serie <- function(data) {
  sfun <- data$sgp$fun
  rfun <- data$rgp$fun

  n_r <- data$rgp$n_r
  t_start <- data$sgp$t_cut + 1

  y <- data$errors

  r <- matrix(0, nrow = n_t, ncol = n_r)
  r_start <- eval_tidy(data$rgp$r_start, list(y = y, r = r, t_start = t_start))
  r[seq_len(t_start - 1), r_start] <- 1

  for (t in t_start:n_t) {
    r[t, ] <- rfun(y, r, t)
    y[t] <- sfun(y, r, t)
  }

  list(r = r, y = y) # Todo:summarize r into vector might be needed for memory
}


# Running simulations:
results <- get_results(
  sim_inputs, simulate_serie,
  n_t = n_t,
  parallel = FALSE, safely = TRUE
)

#keep(results, ~ inherits_any(.x, "try-error")) |> names()
map(results, "error") |> compact() |> names()
map(results, "result") |> keep(~ inherits_any(.x, "try-error")) |> names()


# Collecting results:
results_data <- imap(map(results, "result"), \(res, sim_name) {
  sim_opts <- str_split_1(sim_name, "-")
  tibble(
    group = fct(sim_name), sgp = fct(sim_opts[1]), rgp = fct(sim_opts[2]),
    sim = as.integer(sim_opts[3]),
    t = 1:n_t, y = res$y, r = max.col(res$r)
  )
}) |>
  bind_rows()



# Series Diagnostics -----------------------------------------------------------

gdata <- diagnose$subset_simulations(
  results_data,
  sgps = c("r2_ar1_rho1", "r2_ar1_rho2"),
  rgps = c("sbreak_mid", "threshold_0")
)


diagnose$series_values(gdata, sims = 1, n_burn = n_burn)
if (FALSE) ggsave2("figures/diag_series_one.png", 20, 15)

diagnose$series_values(gdata, sims = sample(n_s, 7), n_burn = n_burn)
if (FALSE) ggsave2("figures/diag_series_mult.png", 20, 15)

diagnose$series_paths(gdata, t_max = 50, n_burn = n_burn)
if (FALSE) ggsave2("figures/diag_series_paths.png", 20, 15)

diagnose$series_density(gdata)
if (FALSE) ggsave2("figures/diag_series_density.png", 20, 15)

diagnose$regimes_values(gdata, n_burn = n_burn)
if (FALSE) ggsave2("figures/diag_regimes.png", 20, 15)

diagnose$series_stats(gdata, n_burn = n_burn, \(x) {
  c("mean" = mean(x), "vol" = sd(x))
})
if (FALSE) ggsave2("figures/diag_stat_mean.png", 20, 15)

diagnose$regimes_stats(gdata, n_burn = n_burn, \(x) {
  estimate_transmat(x, 2)[rbind(c(1, 1), c(2, 2))] |> `names<-`(c("P11", "P22"))
})
if (FALSE) ggsave2("figures/diag_stat_transmat.png", 20, 15)

diagnose$regimes_stats(gdata, n_burn = n_burn, \(x) {
  table(x) %>% `names<-`(glue("N° R{names(.)}"))
})
if (FALSE) ggsave2("figures/diag_stat_nobs.png", 20, 15)
