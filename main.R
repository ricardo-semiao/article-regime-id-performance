
# Setup ------------------------------------------------------------------------

library(mirai)

source("utils.R")
source("codes/sgp.R")
source("codes/rgp.R")



# ------------------------------------------------------------------------------

n_r <- 2
n_t <- 1000


sgp_opts <- list(
  rw_half = list(
    funs = list(
      rw = serie_ar(mu = 0, rho1 = 1.5),
      half = serie_ar(mu = 0, rho1 = 0.5)
    ),
    n_rho = 1
  )
)


rgp_opts <- list(
  binom = regime_binom(n_r, rep(1 / n_r, n_r)),
  markov = regime_markov(n_r, markov_simmat(0.9, n_r))
)


dgp_opts <- tidyr::expand_grid(sgp = names(sgp_opts), rgp = names(rgp_opts))
dgp_opts <- set_names(
  pmap(dgp_opts, ~ list(...)),
  pmap(dgp_opts, ~ paste0(c(...), collapse = "."))
)


errors <- map(dgp_opts, \(x) rnorm(n_t, 0, 1))


# Loop -------------------------------------------------------------------------

# dgp_id <- names(dgp_opts)[[1]]
main_loop <- function(dgp_id) {
  dgp <- dgp_opts[[dgp_id]]
  sgp <- sgp_opts[[dgp$sgp]]
  rgp <- rgp_opts[[dgp$rgp]]

  r <- rgp(n_t)
  y <- errors[[dgp_id]]

  for (t in sgp$n_rho:n_t) {
    y[t] <- sgp$funs[[r[t]]](y, t)
  }

  cbind(r = r, y = y)
}

results <- map(names(dgp_opts), safely(main_loop))
results <- map(results, "result")

ggplot(as.data.frame(results[[2]]), aes(x = 1:n_t, y = y)) +
  geom_line(aes(color = r, group = NA))




cl <- makeClusterPSOCK(2)
print(cl)

results <- parLapply(cl, X = names(dgp_opts), fun = safely(main_loop))

parallel::stopCluster(cl)


# Theta in Theta_opts is a n_r sized list with Theta_r being the params of the current regime
# A more general version would consider different dgps in each regime