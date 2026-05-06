
# Setup ------------------------------------------------------------------------

box::use(
  src/utils[...]
)



# Simulation parameters --------------------------------------------------------

n_i <- 500L # Number of simulations
n_b <- 4L # Burn-in periods
n_h <- 10L # Number of periods to predict
n_t <- 100L + n_b + n_h # Number of time periods

menu <- list()



# Simulation Menu --------------------------------------------------------------

menu$dgps <- expand_grid(
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
    #"r2_multinomial_symm", "r2_multinomial_asymm",
    "r2_markov_symm_high", "r2_markov_asymm_high",
    #"r2_markov_symm_low", "r2_markov_asymm_low",
    #"r2_sbreak_symm", "r2_sbreak_asymm",
    "r2_threshold_symm_x", "r2_threshold_asymm_x",
    #"r2_threshold_symm_abs", "r2_threshold_asymm_abs",
    #"r2_threshold_symm_diff", "r2_threshold_asymm_diff",
    "r2_stransition_symm_l", "r2_stransition_asymm_l",
    #"r2_stransition_symm_e", "r2_stransition_asymm_e",
    NULL
  )
) |>
  mutate(dgp = str_c(sgp, "-", rgp))

menu$sims <- expand_grid(menu$dgps, sim = 1:n_i) |>
  mutate(dgp_sim = str_c(dgp, "-", sim))

n_s <- nrow(menu$sims)



# Estimation Menu --------------------------------------------------------------

menu$ests <- expand_grid(
  menu$sims,
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

n_m <- length(unique(menu$ests$model))
n_e <- nrow(menu$ests)



box::export(
  n_i, n_b, n_h, n_t,
  n_s, n_m, n_e,
  menu
)
