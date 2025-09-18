
# Setup ------------------------------------------------------------------------

library(mirai)

source("utils.R")
source("codes/diagnostics.R")
source("codes/creators_sgp.R")
source("codes/creators_rgp.R")



# Parameters and DGPs ----------------------------------------------------------

# Simulation parameters:
n_s <- 30L # Number of simulations
n_t <- 100L # Number of time periods
n_burn <- 20L # Burn-in periods
n_h <- 1L # Number of periods to predict


# DGP options:
source("codes/options_sgp.R")
source("codes/options_rgp.R")

map(list(options_sgp, options_rgp), names)


# Used combinations:
dgp_names <- expand_grid(
  sgp = c("r2_ar1_rho1", "r2_ar1_rho2", "r2_ar1_mu1", "r2_ar1_mu2"),
  rgp = names(options_rgp)
) |>
  mutate(dgp = str_c(sgp, "-", rgp))

n_p <- nrow(dgp_names)

sim_names <- expand_grid(dgp = dgp_names$dgp, sim = 1:n_s) |>
  pmap_chr(~ str_c(..1, "-", ..2))



# Series Simulation ------------------------------------------------------------

# Error generation:
errors <- rTRNG::rnorm_trng(n_t * n_p * n_s, parallelGrain = 100)

diagnose_dependence(errors)
if (FALSE) ggsave2("figures/diag_errors_dependence.png", 20, 15)

diagnose_errors(errors)
if (FALSE) ggsave2("figures/diag_errors_distribution.png", 20, 15)

errors <- errors |>
  matrix(nrow = n_t, ncol = n_p * n_s) |>
  `colnames<-`(sim_names)


# Simulation data:
# Each iteration must receive all data (for better parallelization)
sim_data <- map(set_names(sim_names), \(opts) {
  opts_split <- str_split_1(opts, "-")
  list(
    sgp = options_sgp[[opts_split[1]]],
    rgp = options_rgp[[opts_split[2]]],
    errors = errors[, opts]
  )
})


# Simulation function:
simulate_serie <- function(data) {
  sfun <- data$sgp$fun
  rfun <- data$rgp$fun

  n_r <- data$rgp$n_r
  t_start <- data$sgp$t_cut + 1

  r <- matrix(0, nrow = n_t, ncol = n_r)
  r[t_start - 1, sample(1:n_r, 1)] <- 1

  y <- data$errors

  for (t in t_start:n_t) {
    r[t, ] <- rfun(y, r, t)
    y[t] <- sfun(y, r, t)
  }

  list(r = r, y = y) # Summarize r into vector?
}


# Running and collecting results:
results <- get_results(
  sim_data, simulate_serie,
  n_t = n_t,
  parallel = FALSE, safely = TRUE
)

#keep(results, ~ inherits_any(.x, "try-error")) |> names()
map(results, "error") |> compact() |> names()
map(results, "result") |> keep(~ inherits_any(.x, "try-error")) |> names()

results_pass <- map(results, "result")
# results_pass[["r2_ar1_mu1-lstar_05-1"]]$r

# Diagnostics:
results_data <- imap(results_pass, \(res, sim_name) {
  sim_opts <- str_split_1(sim_name, "-")
  tibble(
    group = sim_name, sgp = sim_opts[1], rgp = sim_opts[2], sim = sim_opts[3],
    t = 1:n_t, y = res$y, r = max.col(res$r)
  )
}) |>
  bind_rows()


gdata <- results_data %>%
  filter(
    sgp %in% c("r2_ar1_mu1", "r2_ar1_mu2") &
      rgp %in% c("markov_symm_high", "markov_asymm_high") & #c("sbreak_mid", "threshold_0")
      sim %in% c(1, sample(2:n_s, 6))
  ) %>%
  mutate(
    sgp = options_sgp_names[sgp],
    rgp = options_rgp_names[rgp]
  )

diagnose_series(gdata, only_one = TRUE)
if (FALSE) ggsave2("figures/diag_series_one.png", 20, 15)

diagnose_series(gdata)
if (FALSE) ggsave2("figures/diag_series_mult.png", 20, 15)

diagnose_paths(gdata, 50)
if (FALSE) ggsave2("figures/diag_series_paths.png", 20, 15)

diagnose_regimes(gdata)
if (FALSE) ggsave2("figures/diag_regimes.png", 20, 15)

diagnose_stat_y(gdata, \(x) c("mean" = mean(x), "vol" = sd(x)), "mean")
if (FALSE) ggsave2("figures/diag_stat_mean.png", 20, 15)

diagnose_stat_r(gdata, stat_name = "transition matrix", \(x) {
  estimate_transmat(x, 2)[rbind(c(1, 1), c(2, 2))] |> `names<-`(c("P11", "P22"))
})
if (FALSE) ggsave2("figures/diag_stat_transmat.png", 20, 15)

diagnose_stat_r(gdata, stat_name = "transition matrix", \(x) {
  table(x) %>% `names<-`(glue("N° R{names(.)}"))
})
if (FALSE) ggsave2("figures/diag_stat_nobs.png", 20, 15)
# diagnose_stat_r(gdata, stat_name = "transition matrix", \(x) {
#   n_r <- length(unique(x))
#   counts <- matrix(0, n_r, n_r)

#   r_diff <- c(0, diff(x))
#   counts <- table(x, factor(r_diff != 0, c(TRUE, FALSE)))[, "TRUE"]

#   res <- table(x) / counts
#   ifelse(is.infinite(res), NA, res) %>%
#     `names<-`(glue("Avg. Duration R{names(.)}"))
# })
#if (FALSE) ggsave2("figures/diag_stat_transmat.png", 20, 15)



results_data %>%
  filter(
    sgp %in% c("r2_ar1_sign2") &
      rgp %in% c("sbreak_mid", "threshold_0")
  ) %>%
  ggplot(aes(x = y, color = as.factor(r))) +
  geom_density() +
  facet_grid(sgp ~ rgp, scales = "free")


results_data %>%
  filter(str_detect(sgp, "_mu(1|2)$")) %>%
  group_by(sgp, rgp) %>%
  summarise(
    map_dfc(unique(r) %>% set_names(glue("R{.}")), \(reg) mean(y[r == reg]))
  ) %>% print(n = Inf)
