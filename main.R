
# Setup ------------------------------------------------------------------------

library(mirai)

source("utils.R")
source("codes/creators_sgp.R")
source("codes/creators_rgp.R")



# Parameters -------------------------------------------------------------------

# Number of time periods
n_t <- 100L

# Number of periods to predict
n_h <- 1L


# DGP options:
source("codes/options_sgp.R")
source("codes/options_rgp.R")

map(list(options_sgp, options_rgp), names)



# Drafts -----------------------------------------------------------------------

errors <- map(dgp_opts, \(x) rnorm(n_t, 0, 1))


# Ex: `dgp_id = names(dgp_opts)[[1]]`
main_loop <- function(dgp_id) {
  dgp <- dgp_opts[[dgp_id]]
  sgp <- sgp_opts[[dgp$sgp]]
  rgp <- rgp_opts[[dgp$rgp]]

  sfun <- sgp$funs
  rfun <- rgp$fun

  r <- matrix(0, nrow = n_t, ncol = rgp$n_r)
  r[1, sample(1:rgp$n_r, 1)] <- 1

  y <- errors[[dgp_id]]

  for (t in (sgp$n_t_cut + 1):n_t) {
    r[t] <- rfun(y, r, t)
    y[t] <- sfun(y, r, t)
  }

  list(r = r, y = y) # Summarize r into vector?
}


results <- map(names(dgp_opts), safely(main_loop))
results <- map(results, "result")

ggplot(as.data.frame(results[[2]]), aes(x = 1:n_t, y = y)) +
  geom_line(aes(color = r, group = NA))


cl <- makeClusterPSOCK(2)
print(cl)

results <- parLapply(cl, X = names(dgp_opts), fun = safely(main_loop))

parallel::stopCluster(cl)
