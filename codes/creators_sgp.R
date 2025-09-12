
# Creators ---------------------------------------------------------------------

create_sgp <- list()


#' SGP: AR
#'
#' @param mu [`double(1)`] Mean parameter.
#' @param ... [`double(1)`] AR coefficients.
#' @param vol [`double(1)`] Volatility parameter.
#'
#' @value [`expr()`] Expression for AR calculation.
create_sgp$ar <- function(mu, ..., vol = 1) {
  rhos <- list2(...)
  walk(list(mu, rhos, vol), force)

  test_conditions(
    #"{.arg ...} must be named as rho1, rho2, ..." = all(grepl("^rho[0-9]+$", names(rhos))),
    "All arguments must be bare numeric scalars and finite" = all(
      map_lgl(c(mu, rhos, vol), \(x) is_bare_numeric(x, 1) && is.finite(x))
    )
  )

  rhos <- as.numeric(rhos)
  n_rho <- length(rhos)

  expr({
    !!mu + sum(!!rhos * y[(t - 1):(t - !!n_rho)]) + !!vol * y[t]
  })
}
