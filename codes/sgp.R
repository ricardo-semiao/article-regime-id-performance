
serie_ar <- function(mu, ...) {
  rhos <- list2(...)

  conds <- c(
    grepl("^rho[0-9]+$", names(rhos)),
    map_lgl(rhos, ~ is_bare_numeric(.x, 1))
  )

  if (!all(conds)) {
    abort("Wrong arguments to AR process.")
  }

  rhos <- as.numeric(rhos)
  n_rho <- length(rhos)

  \(y, t) {
    mu + sum(rhos * y[t:(t - n_rho)]) + y[t]
  }
}

\(y, t) 0.1 + sum(c(0.5, 0.5) * y[t:(t - 2)]) + y[t]
