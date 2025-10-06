
# Setup ------------------------------------------------------------------------

# Loading dependencies:
box::use(
  src/utils[...]
)
# Functions imported via `::`: mbreaks::dofix, tsDyn::setar, tsDyn::lstar,
# MSwM::msmFit

# Temporary example:
if (FALSE) {
  n_h <- 10; n_burn <- 10; n_t <- 100 + n_h + n_burn
  n_l <- 1; n_r <- 2
  min_r_size <- 0.1; tol <- 1e-5; max_iter <- 10
  g <- \(x) x; gamma <- NULL
  data <- data.frame(y = c(rnorm(50, 2), rnorm(30, 0), rnorm(n_t - 80, 1))) |>
    data_lags(n_l = 1)
}



# Helpers ----------------------------------------------------------------------

# Results are always a list with elements y, r, and meta
# - y: full series with n_burn + n_l + warmup NAs at start. Warmup NAs vary by
# model, but are usually 0 or 1
# - r: regimes, with n_burn + n_l + warmup NAs at start. This is already the
# summarized regime, the categorical column-vector, not the full matrix of e.g.
# probabilities
# - meta: list with model-specific information:abstol
#   - Always contains coefs, a matrix with each row being a regime and each
#   column a coefficient, in the order: intercept, lag 1, lag 2, ..., lag n_l.
#   - Also contains switches, the meta information about regime changes (e.g.
#   thresholds or transition matrix)

#' Internal: Get regimes from model info
get_results <- list()


#' Get results from mbreaks::dofix
#'
#' - Regimes: similar to cut(1:n_t, mod$dates). Prediction is straightforward
#' - Series: prediction using last regime's coefficients. n_l NAs at start
get_results$mbreaks_dofix <- function(data, mod, n_burn, n_h, n_t, n_r, n_l) {
  # Regimes:
  date1 <- n_burn + c(1 - n_burn, mod$date)
  date2 <- n_burn + c(mod$date - 1, n_t - n_burn)

  r <- integer(n_t)
  for (s in 1:n_r) {
    r[date1[s]:date2[s]] <- s
  }

  # Series:
  coefs_last_r <- mod$beta[((n_r - 1) * (n_l + 1) + 1):(n_r * (n_l + 1))]

  preds <- double(n_h)
  for (i in 1:n_h) {
    preds[i] <- sum(coefs_last_r * c(1, data[n_t - i + 1, -1]))
  }

  y <- c(rep(NA, n_burn + n_l), mod$fitted.values, preds)

  # Meta information:
  meta <- list(
    coefs = matrix(mod$beta, n_r, n_l + 1, byrow = FALSE),
    switches = c(mod$date)
  )

  list(y = y, r = r, meta = meta)
}


#' Get results from tsDyn::setar
#'
#' - Regimes: 1 initial NA and in-sample regimes are given. Predictions are the
#'  number of thresholds that the threshold variable exceeds plus 1
#' - Series: current regime's coefficient used at each moment. n_l+1 NAs at
#'  start
get_results$tsdyn_setar <- function(data, mod, n_burn, n_h, n_t, n_r, n_l, g) {
  thresholds <- mod$coefficients[grep("^th", names(mod$coefficients))]
  coefs <- matrix(
    mod$coefficients[grep("^[^th]", names(mod$coefficients))],
    n_r, n_l + 1, byrow = TRUE
  )

  # Regimes:
  r <- integer(n_t)
  r[1:(n_burn + 1)] <- NA_integer_ # tsDyn with m = 1 un-uses 1 more observation
  r[(n_burn + 2):(n_t - n_h)] <- mod$model.specific$regime

  for (i in n_h:1) {
    r[n_t - i + 1] <- sum(thresholds < g(data[, "y_l1"])[n_t - i + 1]) + 1
  }

  # Series:
  preds <- double(n_h)
  for (i in 1:n_h) {
    preds[i] <- sum(coefs[r[n_t - i + 1], ] * c(1, data[n_t - i + 1, -1]))
  }

  y <- c(rep(NA, n_burn + n_l + 1), mod$fitted.values, preds)

  # Meta information:
  meta <- list(
    coefs = coefs,
    switches = thresholds
  )

  list(y = y, r = r, meta = meta)
}


#' Get results from mswm::lstar
#'
#' Only works for 2 regimes
#' - Regimes: same as tsDyn::setar
#' - Series: use the current regime's value to weight the coefficients
get_results$tsdyn_lstar <- function(data, mod, n_burn, n_h, n_t, n_r, n_l) {
  threshold <- mod$coefficients["th"]
  gamma <- mod$coefficients["gamma"]
  coefs <- matrix(
    mod$coefficients[grep("const|phi", names(mod$coefficients))],
    2, n_l + 1, byrow = TRUE
  )

  # Regimes:
  r <- c(
    rep(NA_integer_, n_burn),
    1 / (1 + exp(- (data[(n_burn + 1):n_t, "y_l1"] - threshold) / gamma))
  )

  # Series:
  preds <- double(n_h)
  for (i in 1:n_h) {
    r_i <- r[n_t - i + 1]
    preds[i] <- sum(
      (coefs[1, ] * r_i + coefs[2, ] * (1 - r_i)) * c(1, data[n_t - i + 1, -1])
    )
  }

  y <- c(rep(NA, n_burn + n_l + 1), mod$fitted.values, preds)

  # Meta information:
  meta <- list(
    coefs = coefs,
    switches = threshold,
    gamma = gamma
  )

  list(y = y, r = (r <= 0.5) + 1, meta = meta)
}


#' Get results from mswm::msmFit
#'
#' Predictions overall are the expected values given the probabilities.
#' - Regimes: the marginal probabilities are the filtered ones. These are
#'  updated via the transition matrix. The final regime variable is the most
#'  likely regime.
#' - Series: average across regimes using the marginal probabilities
get_results$mswm_msmfit <- function(data, mod, n_burn, n_h, n_t, n_r, n_l) {
  coefs <- as.matrix(mod@Coef)

  # Regimes:
  r <- matrix(NA, n_t, n_r)
  r[(n_burn + n_l + 1):(n_t - n_h), ] <- mod@Fit@filtProb

  for (i in n_h:1) {
    r[(n_t - i + 1), ] <- mod@transMat %*% r[(n_t - i), ]
  }

  # Series:
  preds <- double(n_h)
  for (i in 1:n_h) {
    preds[i] <- sum(coefs %*% c(1, data[n_t - i + 1, -1]) * r[(n_t - i + 1), ])
  }

  y <- c(rep(NA, n_burn + n_l), mod@model$fitted.values, preds)

  # Meta information:
  meta <- list(
    coefs = coefs,
    switches = mod@transMat
  )

  list(y = y, r = max.col(r), meta = meta)
}


# Sanitizing enclosing environments
for (model_name in names(get_results)) {
  fn_env(get_results[[model_name]]) <- pkg_env("base")
}



# Creators ---------------------------------------------------------------------

# Parameters always include n_r and n_p. Often include optimization
# parameters such as min_r_size, tol, and max_iter
# All return a generator function enclosing a child of base env carrying the
# hyperparameters, model function, and methods for getting predictions and
# regimes
# Model functions are passed via usual `::`, as box might interact weirdly with
# parallelism

#' Structural breaks
#'
#' Comments on parameters:
#' - h set by eps1; model with intercept; no error treatments
#'
#' @export
sbreak <- function(
  n_r, n_l = 1,
  min_r_size = 0.1,
  tol = 1e-5, max_iter = 10
) {
  defaults <- c(
    as.list(current_env()), get_results = get_results$mbreaks_dofix
  )

  body <- expr({
    z_name <- grep("^y_l[0-9]+", colnames(data), value = TRUE)
    mod <- mbreaks::dofix(
      # Data:
      "y", z_name, x_name = NULL, data = data[(n_burn + n_l + 1):(n_t - n_h), ],
      # Hyperparameters:
      fixn = n_r - 1,
      # Optimization:
      eps = tol, eps1 = min_r_size, maxi = max_iter, fixb = 0, betaini = NULL,
      # Others:
      prewhit = 0, robust = 0, hetdat = 0, hetvar = 0, hetq = 0, hetomega = 0,
      h = NULL, const = 1
    )
    get_results(data, mod, n_burn, n_h, n_t, n_r, n_l)
  })

  new_function(
    args = exprs(data = , n_t = , n_h = , n_burn = ),
    body = body,
    env = new_environment(defaults, pkg_env("base"))
  )
}
# Todo: generalize for n_l > 1

#' Model: Threshold
#'
#' Comments on parameters:
#' - m, ML, MM, MH given by mL etc.; th missing (will be estimated)
#' - mTh, thDelay missing, given by thVar
#' - Model in levels and with constants; no threshold restrictions
#'
#' @param g [`function(y){}`] Transition function. Must be a closure (i.e.
#'  non-primitive), and will have its environment sanitized to base env.
#'
#' @export
threshold <- function(
  n_r, n_l = 1, g = \(y) y,
  min_r_size = 0.1,
  tol = 1e-5, max_iter = 10
) {
  g <- new_function(exprs(y = ), fn_body(g), pkg_env("base"))

  defaults <- c(
    as.list(current_env()), get_results = get_results$tsdyn_setar
  )

  body <- expr({
    mod <- tsDyn::setar(
      # Data:
      data[(n_burn + n_l + 1):(n_t - n_h), "y"], mL = n_l, mM = n_l, mH = n_l,
      thVar = g(data[, "y_l1"])[(n_burn + n_l + 1):(n_t - n_h)],
      # Hyperparameters:
      nthresh = n_r - 1,
      # Optimization:
      trim = min_r_size,
      # Others:
      d = 1, steps = 1,
      include = "const", common = "none", model = "TAR", type = "level",
      restriction = "none", trace = FALSE
    )
    get_results(data, mod, n_burn, n_h, n_t, n_r, n_l, g = g)
  })

  new_function(
    args = exprs(data = , n_t = , n_h = , n_burn = ),
    body = body,
    env = new_environment(defaults, pkg_env("base"))
  )
}

#' Model: Smooth transition
#'
#' Only works for 2 regimes. Comments on parameters:
#' - m, ML, MM, MH given by mL etc.; th missing (will be estimated)
#' - mTh, thDelay missing, given by thVar
#' - Model in levels and with constants; no threshold restrictions
#' - Also consider starting.control
#' - Old option: `thVar = data$y_l1[(1 + n_l):(n_t - n_h)]`
#'
#' @export
stransition <- function(
  n_r = 2, n_l = 1, gamma = NULL,
  min_r_size = 0.1,
  tol = 1e-5, max_iter = 10
) {
  defaults <- c(
    as.list(current_env()), get_results = get_results$tsdyn_lstar
  )

  body <- expr({
    gamma <- gamma %||% quote(expr = )
    mod <- tsDyn::lstar(
      # Data:
      data[(n_burn + 1):(n_t - n_h), "y"], mL = n_l, mH = n_l, thDelay = n_l,
      # Hyperparameters:
      gamma = gamma,
      # Optimization:
      control = list(maxit = max_iter, abstol = tol),
      # Others:
      d = 1, steps = 1, include = "const", trace = FALSE
    )
    get_results(data, mod, n_burn, n_h, n_t, n_r, n_l)
  })

  new_function(
    args = exprs(data = , n_t = , n_h = , n_burn = ),
    body = body,
    env = new_environment(defaults, pkg_env("base"))
  )
}

#' Model: Markov switching
#'
#' Comments on parameters:
#' - All coefficients switch between regimes, but not vol
#'
#' @export
markov <- function(
  n_r = 2, n_l = 1, gamma = NULL,
  min_r_size = 0.1,
  tol = 1e-5, max_iter = 10
) {
  defaults <- c(
    as.list(current_env()), get_results = get_results$mswm_msmfit
  )

  body <- expr({
    mod <- MSwM::msmFit(
      # Data:
      y ~ 1, k = n_r, p = n_l, data = as.data.frame(data[(n_burn + 1):(n_t - n_h), ]),
      # Optimization:
      control = list(maxiter = max_iter, tol = tol, parallelization = FALSE),
      # Others:
      sw = c(rep(TRUE, n_l + 1), FALSE)
    )
    get_results(data, mod, n_burn, n_h, n_t, n_r, n_l)
  })

  new_function(
    args = exprs(data = , n_t = , n_h = , n_burn = ),
    body = body,
    env = new_environment(defaults, pkg_env("base"))
  )
}
