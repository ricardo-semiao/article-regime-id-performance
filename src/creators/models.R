
# Setup ------------------------------------------------------------------------

# Loading dependencies
box::use(
  src/utils[...]
)
# Functions imported via `::`: mbreaks::dofix, tsDyn::setar, tsDyn::lstar,
# MSwM::msmFit

# Temporary example:
if (FALSE) {
  data <- tibble(
    y = c(rnorm(30, 2), rnorm(40, 0), rnorm(30, 1)),
    y_l1 = lag(y, 1L, default = NA)
  ) |> as.data.frame()

  n_p <- 1; n_t <- 100; n_h <- 1
  n_r <- 3; min_r_size <- 0.1; tol <- 1e-5; max_iter <- 10
  g <- \(x) x; gamma <- NULL
}



# Helpers ----------------------------------------------------------------------

#' Internal: Get regimes from model info
get_results <- list()


#' Get results from mbreaks::dofix
#'
#' - Regimes: similar to cut(1:n_t, mod$dates). Prediction is straightforward
#' - Series: prediction using last regime's coefficients. n_p NAs at start
get_results$mbreaks_dofix <- function(data, mod, n_t, n_h, n_r, n_p) {
  # Regimes:
  date1 <- c(1, mod$date)
  date2 <- c(mod$date - 1, n_t)

  r <- integer(n_t)
  for (s in 1:n_r) {
    r[date1[s]:date2[s]] <- s
  }

  # Series:
  coefs_last_r <- mod$beta[((n_r - 1) * (n_p + 1) + 1):(n_r * (n_p + 1))]

  preds <- double(n_h)
  for (i in 1:n_h) {
    preds[i] <- sum(coefs_last_r * c(1, data[n_t - i + 1, -1]))
  }

  y <- c(rep(NA, n_p), mod$fitted.values, preds)

  # Meta information:
  meta <- list(
    coefs = matrix(mod$beta, n_r, n_p + 1, byrow = FALSE),
    switches = c(mod$date)
  )

  list(y = y, r = r, meta = meta)
}


#' Get results from tsDyn::setar
#'
#' - Regimes: 1 initial NA and in-sample regimes are given. Predictions are the
#'  number of thresholds that the threshold variable exceeds plus 1
#' - Series: current regime's coefficient used at each moment. n_p+1 NAs at
#'  start
get_results$tsdyn_setar <- function(data, mod, n_t, n_h, n_r, n_p, g) {
  thresholds <- mod$coefficients[grep("const|phi", names(mod$coefficients))]
  coefs <- matrix(
    mod$coefficients[grep("^[^th]", names(mod$coefficients))],
    n_r, n_p + 1, byrow = TRUE
  )
  dimnames(coefs) <- list(
    paste0("Regime ", 1:n_r),
    c("Intercept", paste0("AR(", 1:n_p, ")"))
  )

  # Regimes:
  r <- integer(n_t)
  r[1] <- NA_integer_ # tsDyn with m = 1 un-uses 1 more observation
  r[2:(n_t - n_h)] <- mod$model.specific$regime

  for (i in n_h:1) {
    r[n_t - i + 1] <- sum(thresholds < g(data$y_l1)[n_t - i + 1]) + 1
  }

  # Series:
  preds <- double(n_h)
  for (i in 1:n_h) {
    r_i <- r[n_t - i + 1]
    preds[i] <- sum(coefs[r_i, ] * c(1, data[n_t - i + 1, -1]))
  }

  y <- c(rep(NA, n_p + 1), mod$fitted.values, preds)

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
get_results$tsdyn_lstar <- function(data, mod, n_t, n_h, n_r, n_p) {
  threshold <- mod$coefficients["th"]
  gamma <- mod$coefficients["gamma"]
  coefs <- matrix(
    mod$coefficients[grep("const|phi", names(mod$coefficients))],
    2, n_p + 1, byrow = TRUE
  )

  # Regimes:
  r <- 1 / (1 + exp(- (data$y_l1[1:n_t] - threshold) / gamma))

  # Series:
  preds <- double(n_h)
  for (i in 1:n_h) {
    r_i <- r[n_t - i + 1]
    preds[i] <- sum(
      (coefs[1, ] * r_i + coefs[2, ] * (1 - r_i)) * c(1, data[n_t - i + 1, -1])
    )
  }

  y <- c(rep(NA, n_p + 1), mod$fitted.values, preds)

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
get_results$mswm_msmfit <- function(data, mod, n_t, n_h, n_r, n_p) {
  coefs <- as.matrix(mod@Coef)

  # Regimes:
  r <- rbind(NA, NA, mod@Fit@filtProb, NA) # Todo: correct to n_p + 1 NAs

  for (i in n_h:1) {
    r[(n_t - i + 1), ] <- mod@transMat %*% r[(n_t - i), ]
  }

  # Series:
  preds <- double(n_h)
  for (i in 1:n_h) {
    preds[i] <- sum(coefs %*% c(1, data[n_t - i + 1, -1]) * r[(n_t - i + 1), ])
  }

  y <- c(rep(NA, n_p + 1), mod@model$fitted.values, preds)

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
  n_r, n_p = 1,
  min_r_size = 0.1,
  tol = 1e-5, max_iter = 10
) {
  defaults <- c(
    as.list(current_env()), get_results = get_results$mbreaks_dofix
  )

  body <- expr({
    mod <- mbreaks::dofix(
      # Data:
      "y", "y_l1", x_name = NULL, data = data[(1 + n_p):(n_t - n_h), ],
      # Hyperparameters:
      fixn = n_r - 1,
      # Optimization:
      eps = tol, eps1 = min_r_size, maxi = max_iter, fixb = 0, betaini = NULL,
      # Others:
      prewhit = 0, robust = 0, hetdat = 0, hetvar = 0, hetq = 0, hetomega = 0,
      h = NULL, const = 1
    )
    get_results(data, mod, n_t, n_h, n_r, n_p)
  })

  new_function(
    args = exprs(data = , n_t = , n_h = ),
    body = body,
    env = new_environment(defaults, pkg_env("base"))
  )
}
# Todo: generalize for n_p > 1

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
  n_r, n_p = 1, g = \(y) y,
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
      data$y[(1 + n_p):(n_t - n_h)], mL = n_p, mM = n_p, mH = n_p,
      thVar = g(data$y_l1)[(1 + n_p):(n_t - n_h)],
      # Hyperparameters:
      nthresh = n_r - 1,
      # Optimization:
      trim = min_r_size,
      # Others:
      d = 1, steps = 1,
      include = "const", common = "none", model = "TAR", type = "level",
      restriction = "none", trace = FALSE
    )
    get_results(data, mod, n_t, n_h, n_r, n_p, g = g)
  })

  new_function(
    args = exprs(data = , n_t = , n_h = ),
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
#' - Old option: `thVar = data$y_l1[(1 + n_p):(n_t - n_h)]`
#'
#' @export
stransition <- function(
  n_r = 2, n_p = 1, gamma = NULL,
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
      data$y[1:(n_t - n_h)], mL = n_p, mH = n_p, thDelay = n_p,
      # Hyperparameters:
      gamma = gamma,
      # Optimization:
      control = list(maxit = max_iter, abstol = tol),
      # Others:
      d = 1, steps = 1, include = "const", trace = FALSE
    )
    get_results(data, mod, n_t, n_h, n_r, n_p)
  })

  new_function(
    args = exprs(data = , n_t = , n_h = ),
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
  n_r = 2, n_p = 1, gamma = NULL,
  min_r_size = 0.1,
  tol = 1e-5, max_iter = 10
) {
  defaults <- c(
    as.list(current_env()), get_results = get_results$mswm_msmfit
  )

  body <- expr({
    mod <- MSwM::msmFit(
      # Data:
      y ~ 1, k = n_r, p = n_p, data = data[(1 + n_p):(n_t - n_h), ],
      # Optimization:
      control = list(maxiter = max_iter, tol = tol, parallelization = FALSE),
      # Others:
      sw = c(rep(TRUE, n_p + 1), FALSE)
    )
    get_results(data, mod, n_t, n_h, n_r, n_p)
  })

  new_function(
    args = exprs(data = , n_t = , n_h = ),
    body = body,
    env = new_environment(defaults, pkg_env("base"))
  )
}
