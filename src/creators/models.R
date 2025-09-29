
# Setup ------------------------------------------------------------------------

# Loading dependencies
box::use(
  ../utils[...],
  mbreaks[sbreak_mbreaks = dofix],
  tsDyn[threshold_tsdyn = setar, smooth_threshold_tsdyn = lstar],
  MSwM[markov_mswm = msmFit]
)



# Helpers ----------------------------------------------------------------------

# Get regimes from model info
get_r <- list()

get_r$mbreaks <- function(data, mod, n_t, n_r_hat) {
  date1 <- c(1, mod$date)
  date2 <- c(mod$date - 1, n_t)

  r <- integer(n_t)
  for (s in 1:n_r_hat) {
    r[date1[s]:date2[s]] <- s
  }

  r
}

get_r$tsdyn <- function(data, mod, thresholds, g, n_t, n_h) {
  r <- integer(n_t)

  r[1] <- NA_integer_ # tsDyn with m = 1 un-uses 1 more observation
  r[2:(n_t - n_h)] <- mod$model.specific$regime

  for (i in n_h:1) {
    r[n_t - i + 1] <- sum(thresholds < g(data$y_l1)[n_t - i + 1]) + 1
  }

  r
}

get_r$mswm <- function(data, mod, n_h, n_t) {
  r <- rbind(NA, NA, mod@Fit@filtProb, NA)

  for (i in n_h:1) {
    r[(n_t - i + 1), ] <- mod@transMat %*% r[(n_t - i), ]
  }

  r
}


# Predictions
get_pred <- list()

get_pred$mbreaks <- function(data, mod, n_r_hat, n_p, n_t, n_h) {
  coefs_last_r <- mod$beta[((n_r_hat - 1) * (n_p + 1) + 1):(n_r_hat * (n_p + 1))]

  preds <- double(n_h)
  for (i in 1:n_h) {
    preds[i] <- sum(coefs_last_r * c(1, data[n_t - i + 1, -1]))
  }

  preds
}

get_pred$tsdyn <- function(data, coefs, r, n_h, n_t) {
  preds <- double(n_h)
  for (i in 1:n_h) {
    r_i <- r[n_t - i + 1]
    preds[i] <- sum(
      (coefs[1, ] * r_i + coefs[2, ] * (1 - r_i)) * c(1, data[n_t - i + 1, -1])
    )
  }

  preds
}

get_pred$mswm <- function(data, coefs, r, n_h, n_t) {
  preds <- double(n_h)

  for (i in 1:n_h) {
    preds[i] <- sum(coefs %*% c(1, data[n_t - i + 1, -1]) * r[(n_t - i + 1), ])
  }

  preds
}


# Temporary example:
if (FALSE) {
  data <- tibble::tibble(
    y = c(rnorm(30, 2), rnorm(40, 0), rnorm(30, 1)),
    y_l1 = lag(y, 1L, default = NA)
  ) |> as.data.frame()

  n_p = 1; n_t = 100; n_h = 1
  n_r_hat = 3; min_r_size = 0.1; tol = 1e-5; max_iter = 10
  g = function(x) x; gamma = NULL
}



# Models -----------------------------------------------------------------------

# Structural breaks
sbreak <- function(
  n_r_hat, n_p = 1,
  min_r_size = 0.1,
  tol = 1e-5, max_iter = 10
) {
  defaults <- c(
    as.list(current_env()), model = sbreak_mbreaks,
    get_pred = get_pred$mbreaks, get_r = get_r$mbreaks
  )

  body <- expr({
    # Function: mbreaks::dofix
    mod <- mbreaks::dofix(
      # Data:
      "y", "y_l1", x_name = NULL, data = data[(1 + n_p):(n_t - n_h), ],
      # Hyperparameters:
      fixn = n_r_hat - 1,
      # Optimization:
      eps = tol, eps1 = min_r_size, maxi = max_iter, fixb = 0, betaini = NULL,
      # Others:
      prewhit = 0, robust = 0, hetdat = 0, hetvar = 0, hetq = 0, hetomega = 0,
      h = NULL, const = 1
      # h set by eps1; model with intercept; no error treatments
    )

    # Storing results:
    list(
      y = c(
        rep(NA, n_p), # Todo: data$y[1:n_p]?
        mod$fitted.values,
        get_pred(data, mod, n_r_hat, n_p, n_t, n_h)
      ),
      r = get_r(data, mod, n_t, n_r_hat),
      meta = list(
        coefs = matrix(mod$beta, n_r_hat, n_p + 1, byrow = FALSE),
        switches = c(mod$date)
      )
    )
  })

  new_function(
    args = exprs(data = , n_t = , n_h = ),
    body = body,
    env = new_environment(defaults, pkg_env("base"))
  )
}


# Threshold
threshold <- function(
  n_r_hat, n_p = 1, g = \(y) y,
  min_r_size = 0.1,
  tol = 1e-5, max_iter = 10
) {
  defaults <- c(
    as.list(current_env()), model = threshold_tsdyn,
    get_pred = get_pred$tsdyn, get_r = get_r$tsdyn
  )

  body <- expr({
    # Function: tsDyn::setar
    mod <- model(
      # Data:
      data$y[(1 + n_p):(n_t - n_h)], mL = 1, mM = 1, mH = 1,
      thVar = g(data$y_l1)[(1 + n_p):(n_t - n_h)],
      # Hyperparameters:
      nthresh = n_r_hat - 1,
      # Optimization:
      trim = min_r_size,
      # Others:
      d = 1, steps = 1,
      include = "const", common = "none", model = "TAR", type = "level",
      trace = FALSE,  restriction = "none"
      # m, ML, MM, MH given by mL etc.; th missing (will be estimated)
      # mTh, thDelay missing, given by thVar
      # model in levels and with constants; no threshold restrictions
    )

    # Storing results:
    thresholds <- mod$coefficients[grep("const|phi", names(mod$coefficients))]
    r <- get_r(data, mod, thresholds, g, n_t, n_h)
    coefs <- matrix(
      mod$coefficients[grep("^[^th]", names(mod$coefficients))],
      n_r_hat, n_p + 1, byrow = TRUE
    )
    dimnames(coefs) <- list(
      paste0("Regime ", 1:n_r_hat),
      c("Intercept", paste0("AR(", 1:n_p, ")"))
    )

    list(
      y = c(
        rep(NA, n_p + 1),
        mod$fitted.values,
        get_pred(data, coefs, r, n_h, n_t)
      ),
      r = r,
      meta = list(
        coefs = coefs,
        switches = thresholds
      )
    )
  })

  new_function(
    args = exprs(data = , n_t = , n_h = ),
    body = body,
    env = new_environment(defaults, pkg_env("base"))
  )
}


# Smooth transition:
smooth_threshold <- function(
  n_r_hat = 2, n_p = 1, gamma = NULL,
  min_r_size = 0.1,
  tol = 1e-5, max_iter = 10
) {
  defaults <- c(
    as.list(current_env()), model = smooth_threshold_tsdyn,
    get_pred = get_pred$tsdyn, get_r = get_r$tsdyn
  )

  body <- expr({
    gamma <- gamma %||% quote(expr = )
    # Function: tsDyn::lstar
    mod <- model(
      # Data:
      data$y[1:(n_t - n_h)], mL = 1, mH = 1, thDelay = 1,
      #thVar = data$y_l1[(1 + n_p):(n_t - n_h)],
      # Hyperparameters:
      gamma = gamma,
      # Optimization:
      control = list(maxit = max_iter, abstol = tol),
      # Also consider starting.control
      # Others:
      d = 1, steps = 1,
      include = "const", trace = FALSE
      # m, ML, MM, MH given by mL etc.; th missing (will be estimated)
      # mTh, thDelay missing, given by thVar
      # model in levels and with constants; no threshold restrictions
    )

    # Storing results:
    threshold <- mod$coefficients["th"]
    gamma <- mod$coefficients["gamma"]

    r <- 1 / (1 + exp(- (data$y_l1[1:n_t] - threshold) / gamma))
    coefs <- matrix(
      mod$coefficients[grep("const|phi", names(mod$coefficients))],
      2, n_p + 1, byrow = TRUE
    )

    list(
      y = c(
        rep(NA, n_p + 1),
        mod$fitted.values,
        get_pred(data, coefs, r, n_h, n_t)
      ),
      r = (r <= 0.5) + 1,
      meta = list(
        coefs = coefs,
        gamma = gamma,
        switches = threshold
      )
    )
  })

  new_function(
    args = exprs(data = , n_t = , n_h = ),
    body = body,
    env = new_environment(defaults, pkg_env("base"))
  )
}


# Markov
markov <- function(
  n_r_hat = 2, n_p = 1, gamma = NULL,
  min_r_size = 0.1,
  tol = 1e-5, max_iter = 10
) {
  defaults <- c(
    as.list(current_env()), model = markov_mswm,
    get_pred = get_pred$mswm, get_r = get_r$mswm
  )

  body <- expr({
    # Function: MSwM::msmFit
    mod <- model(
      # Data:
      y ~ 1, k = n_r_hat, p = n_p, data = data[(1 + n_p):(n_t - n_h), ],
      # Optimization:
      control = list(maxiter = max_iter, tol = tol, parallelization = FALSE),
      # Others:
      sw = c(rep(TRUE, n_p + 1), FALSE)
      # all coefficients switch between regimes, but not vol
    )

    # Storing results:
    coefs <- as.matrix(mod@Coef)
    r <- get_r(data, mod, n_h, n_t)

    list(
      y = c(
        rep(NA, n_p + 1),
        mod@model$fitted.values,
        get_pred(data, coefs, r, n_h, n_t)
      ),
      r = max.col(r),
      meta = list(
        coefs = coefs,
        switches = mod@transMat
      )
    )
  })

  new_function(
    args = exprs(data = , n_t = , n_h = ),
    body = body,
    env = new_environment(defaults, pkg_env("base"))
  )
}
