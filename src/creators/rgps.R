
# Setup ------------------------------------------------------------------------

# Loading dependencies
box::use(
  ../utils[...]
)



# Creators ---------------------------------------------------------------------

#' RGP: Markov
#'
#' @param probs [`matrix()`] Transition probability matrix.
#'
#' @returns [`function`] Markov regime generator function.
#' @export
markov <- function(probs) {
  walk(list(probs), force)

  test_conditions(
    "{.arg probs} must be a bare numeric matrix" = is_bare_double(probs) && is.matrix(probs),
    "All values in {.arg probs} must be non-negative" = all(probs >= 0),
    "Each row of {.arg probs} must sum to 1" = all(rowSums(probs) == 1)
  )

  n_r <- nrow(probs)

  new_function(
    args = pairlist2(y = , r = , t = ),
    body = expr({
      r[t, sample(1:!!n_r, 1, prob = (!!probs)[r[t - 1, ] == 1, ])] <- 1
      r[t, ]
    }),
    env = pkg_env("stats")
  )
}
# Todo: test n_r


#' RGP: Structural breaks
#'
#' @param breaks [`integer()`] Strictly increasing vector of break points
#' (>= 1). Breaks are closed on left.
#'
#' @returns  [`function`] Structural break regime generator function.
#' @export
sbreak <- function(breaks) {
  walk(list(breaks), force)

  test_conditions(
    "{.arg breaks} must be a bare integer vector" = is_bare_integer(breaks),
    "All values in {.arg breaks} must be >= 1" = all(breaks >= 1),
    "{.arg breaks} must be strictly increasing" = all(diff(breaks) > 0)
  )

  new_function(
    args = pairlist2(y = , r = , t = ),
    body = expr({
      r[t, .Internal(which(t < c(!!breaks, Inf)))[1]] <- 1
      r[t, ]
    }),
    env = pkg_env("stats")
  )
}
# Todo: test consistency with n_t and n_r


#' SRP: Threshold
#'
#' @param breaks [`numeric()`] Strictly increasing vector of thresholds. Breaks
#' are closed on left.
#' @param g [`function`] Function for `g(y[t]) < breaks`.
#'
#' @returns  [`function`] Threshold regime generator function.
#' @export
threshold <- function(breaks, g = \(x) x) {
  walk(list(breaks, g), force)

  test_conditions(
    "{.arg breaks} must be a bare numeric vector" = is_bare_numeric(breaks),
    "{.arg breaks} must be strictly increasing" = all(diff(breaks) > 0)
  )

  new_function(
    args = pairlist2(y = , r = , t = ),
    body = expr({
      r[t, .Internal(which(g(y[t]) < c(!!breaks, Inf)))[1]] <- 1
      r[t, ]
    }),
    env = new_environment(list(g = g), pkg_env("stats"))
  )
}
# Todo: implement left_closed argument


#' SRP: Smooth Transition.
#'
#' Only suitable for 2 regimes.
#'
#' @param breaks [`numeric()`] Strictly increasing vector of thresholds. Breaks
#' are closed on left.
#' @param g [`function`] Transition function.
#'
#' @returns  [`function`] Smooth threshold regime generator function.
#' @export
smooth_threshold <- function(breaks, g) {
  walk(list(breaks, g), force)

  test_conditions(
    "{.arg breaks} must be a bare numeric scalar" = is_bare_numeric(breaks, 1),
    "{.arg breaks} must be strictly increasing" = all(diff(breaks) > 0),
    "{.arg g} must be a function" = is_function(g)
  )

  new_function(
    args = pairlist2(y = , r = , t = ),
    body = expr({
      r[t, 1] <- g(y[t], !!breaks)
      r[t, 2] <- 1 - r[t, 1]
      r[t, ]
    }),
    env = new_environment(list(g = g), pkg_env("stats"))
  )
}
