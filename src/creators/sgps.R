
# Setup ------------------------------------------------------------------------

# Loading dependencies:
box::use(
  src/utils[...]
)



# Creators ---------------------------------------------------------------------

# All return an expression

#' SGP: AR
#'
#' @param mu [`double(1)`] Mean parameter.
#' @param ... [`double(1)` each] AR coefficients.
#' @param sigma [`double(1)`] Volatility parameter.
#'
#' @returns  [`expression()`] Expression for AR calculation.
#' @export
ar <- function(mu = 0, ..., sigma = 1) {
  rhos <- list2(...)
  walk(list(mu, rhos, sigma), force)

  test_conditions(
    "All arguments must be bare numeric scalars and finite" = all(
      map_lgl(c(mu, rhos, sigma), \(x) is_bare_numeric(x, 1) && is.finite(x))
    )
  )

  rhos <- as.numeric(rhos)
  n_rho <- length(rhos)

  expr({
    !!mu + sum(!!rhos * y[(t - 1):(t - !!n_rho)]) + !!sigma * y[t]
  })
}
