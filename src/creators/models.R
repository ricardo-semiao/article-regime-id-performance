
# Setup ------------------------------------------------------------------------

# Loading dependencies:
box::use(
  src/utils[...],
  src/metrics[series_sd]
)
# Functions imported via `::`: mbreaks::dofix, tsDyn::setar, tsDyn::lstar,
# MSwM::msmFit, stats::lm

#' Helper: Order regimes by a varying parameter
#' TODO: document and think where to put
regimes_order <- function(coefs, rn_par, dims) {
  order(coefs[, which(rn_par == dims$cols)], decreasing = FALSE)
}
fn_env(regimes_order) <- pkg_env("base")

bare_sd <- function(x, ...) {
  xna <- x[!is.na(x)]
  n <- length(xna)

  sqrt(sum((xna - sum(xna) / n)^2) / (n - 1))
}


# Temporary example:
if (FALSE) {
  n_h <- 10; n_b <- 4; n_t <- 100 + n_h + n_b
  n_l <- 1; n_r <- 2
  min_r_size <- 0.1; tol <- 1e-5; max_iter <- 10
  g <- \(x) x; gamma <- NULL; rn_par = "rho1"
  w_size = 5
  data <- data.frame(y = c(rnorm(50, 2), rnorm(30, 0), rnorm(n_t - 80, 1))) |>
    data_lags(n_l = 1)
}



# Helpers ----------------------------------------------------------------------

# Results are always a list with elements y, r, and meta
# - y: full series with n_b + n_l + warmup NAs at start. Warmup NAs vary by
# model, but are usually 0 or 1
# - r: regimes, with n_b + n_l + warmup NAs at start. This is already the
# summarized regime, the categorical column-vector, not the full matrix of e.g.
# probabilities
# - meta: list with model-specific information:abstol
#   - Always contains coefs, a matrix with each row being a regime and each
#   column a coefficient, in the order: intercept, lag 1, lag 2, ..., lag n_l.
#   - Also contains switches, the meta information about regime changes (e.g.
#   thresholds or transition matrix)

#' Internal: Get regimes from model info
get_results <- list()


#' Get results from stats::lm
#' n_r and rn_par are ignored, as there is only 1 regime and no varying
#'  parameters
get_results$stats_lm <- function(data, mod, n_b, n_h, n_t, n_r, n_l, rn_par) {
  dims <- list(
    rows = paste0("R", 1),
    cols = c("mu", paste0("rho", 1:n_l), "sigma")
  )

  # Regimes:
  r <- rep(1L, n_t)
  r[1:n_b] <- NA_integer_ # TODO optimize as is in RF

  # Series:
  preds <- double(n_h)
  for (i in 0:(n_h - 1)) {
    preds[i + 1] <- sum(mod$coefficients * c(1, data[n_t - i, -1]))
  }

  y <- c(rep(NA_real_, n_b + n_l), mod$fitted.values, preds)

  # Meta information:
  idx_fit <- (n_b + 1):(n_t - n_h)
  coefs <- cbind(
    matrix(mod$coefficients, 1, n_l + 1, byrow = TRUE),
    series_sd(data[idx_fit, 1] - y[idx_fit], r, 1, na.rm = TRUE)
  )

  meta <- list(
    coefs = `dimnames<-`(coefs, dims)
  )

  list(y = unname(y), r = r, meta = meta)
}


#' Get results from mbreaks::dofix
#'
#' - Regimes: similar to cut(1:n_t, mod$dates). Prediction is straightforward
#' - Series: prediction using last regime's coefficients. n_l NAs at start
get_results$mbreaks_dofix <- function(data, mod, n_b, n_h, n_t, n_r, n_l, rn_par) {
  dims <- list(
    rows = paste0("R", 1:n_r),
    cols = c("mu", paste0("rho", 1:n_l), "sigma")
  )

  # Regimes:
  date1 <- n_b + c(1, mod$date)
  date2 <- n_b + c(mod$date - 1, n_t - n_b)

  r <- integer(n_t)
  r[1:n_b] <- NA_integer_
  for (s in 1:n_r) {
    r[date1[s]:date2[s]] <- s
  }

  # Series:
  coefs_last_r <- mod$beta[((n_r - 1) * (n_l + 1) + 1):(n_r * (n_l + 1))] # TODO: Check

  preds <- double(n_h)
  for (i in 0:(n_h - 1)) {
    preds[i + 1] <- sum(coefs_last_r * c(1, data[n_t - i, -1]))
  }

  y <- c(rep(NA_real_, n_b + n_l), mod$fitted.values, preds)

  # Meta information:
  idx_fit <- (n_b + 1):(n_t - n_h)
  coefs <- cbind(
    matrix(mod$beta, n_r, n_l + 1, byrow = TRUE),
    series_sd(data[idx_fit, 1] - y[idx_fit], r, n_r, na.rm = TRUE)
  )
  ord <- regimes_order(coefs, rn_par, dims)

  meta <- list(
    coefs = `dimnames<-`(coefs[ord, ], dims),
    switches = c(mod$date)
  )

  list(y = y, r = ord[r], meta = meta)
}


#' Get results from tsDyn::setar
#'
#' - Regimes: 1 initial NA and in-sample regimes are given. Predictions are the
#'  number of thresholds that the threshold variable exceeds plus 1
#' - Series: current regime's coefficient used at each moment. n_l+1 NAs at
#'  start
get_results$tsdyn_setar <- function(data, mod, n_b, n_h, n_t, n_r, n_l, rn_par, g) {
  dims <- list(
    rows = paste0("R", 1:n_r),
    cols = c("mu", paste0("rho", 1:n_l), "sigma")
  )
  coefs_raw <- mod$coefficients

  thresholds <- coefs_raw[grep("^th", names(coefs_raw))]
  coefs <- matrix(
    coefs_raw[grep("^[^th]", names(coefs_raw))],
    n_r, n_l + 1, byrow = TRUE
  )

  # Regimes:
  r <- integer(n_t)
  r[1:(n_b + 1)] <- NA_integer_ # tsDyn with m = 1 un-uses 1 more observation
  r[(n_b + 2):(n_t - n_h)] <- mod$model.specific$regime

  for (i in (n_h - 1):0) {
    r[n_t - i] <- sum(thresholds < g(data[, "y_l1"])[n_t - i]) + 1
  }

  # Series:
  preds <- double(n_h)
  for (i in 0:(n_h - 1)) {
    preds[i + 1] <- sum(coefs[r[n_t - i], ] * c(1, data[n_t - i, -1]))
  }

  y <- c(rep(NA_real_, n_b + n_l + 1), mod$fitted.values, preds)

  # Meta information:
  idx_fit <- (n_b + 1):(n_t - n_h)
  coefs <- cbind(coefs, series_sd(data[idx_fit, 1] - y[idx_fit], r, n_r, na.rm = TRUE))
  ord <- regimes_order(coefs, rn_par, dims)

  meta <- list(
    coefs = `dimnames<-`(coefs[ord, ], dims),
    switches = thresholds
  )

  list(y = y, r = ord[r], meta = meta)
}


#' Get results from mswm::lstar
#'
#' Only works for 2 regimes
#' - Regimes: same as tsDyn::setar
#' - Series: use the current regime's value to weight the coefficients
get_results$tsdyn_lstar <- function(data, mod, n_b, n_h, n_t, n_r, n_l, rn_par) {
  dims <- list(
    rows = paste0("R", 1:n_r),
    cols = c("mu", paste0("rho", 1:n_l), "sigma")
  )

  threshold <- mod$coefficients["th"]
  gamma <- mod$coefficients["gamma"]
  coefs <- matrix(
    mod$coefficients[grep("const|phi", names(mod$coefficients))],
    2, n_l + 1, byrow = TRUE
  )

  # Regimes:
  r <- c(
    rep(NA_integer_, n_b),
    1 / (1 + exp(- (data[(n_b + 1):n_t, "y_l1"] - threshold) / gamma))
  )

  # Series:
  preds <- double(n_h)
  for (i in 0:(n_h - 1)) {
    r_i <- r[n_t - i]
    preds[i + 1] <- sum(
      (coefs[1, ] * r_i + coefs[2, ] * (1 - r_i)) * c(1, data[n_t - i, -1])
    )
  }

  y <- c(rep(NA_real_, n_b + n_l + 1), mod$fitted.values, preds)
  r_cat <- (r <= 0.5) + 1

  # Meta information:
  idx_fit <- (n_b + 1):(n_t - n_h)
  coefs <- cbind(coefs, series_sd(data[idx_fit, 1] - y[idx_fit], r_cat, n_r, na.rm = TRUE))
  ord <- regimes_order(coefs, rn_par, dims)

  meta <- list(
    coefs = `dimnames<-`(coefs[ord, ], dims),
    switches = threshold,
    gamma = gamma
  )

  list(y = y, r = ord[r_cat], meta = meta)
}


#' Get results from mswm::msmFit
#'
#' Predictions overall are the expected values given the probabilities.
#' - Regimes: the marginal probabilities are the filtered ones. These are
#'  updated via the transition matrix. The final regime variable is the most
#'  likely regime.
#' - Series: average across regimes using the marginal probabilities
get_results$mswm_msmfit <- function(data, mod, n_b, n_h, n_t, n_r, n_l, rn_par) {
  dims <- list(
    rows = paste0("R", 1:n_r),
    cols = c("mu", paste0("rho", 1:n_l), "sigma")
  )

  coefs <- as.matrix(mod@Coef)

  # Regimes:
  r <- matrix(NA, n_t, n_r)
  r[(n_b + n_l + 1):(n_t - n_h), ] <- mod@Fit@filtProb

  for (i in (n_h - 1):0) {
    r[(n_t - i), ] <- mod@transMat %*% r[(n_t - i - 1), ]
  }

  # Series:
  preds <- double(n_h)
  for (i in 0:(n_h - 1)) {
    preds[i + 1] <- sum(coefs %*% c(1, data[n_t - i, -1]) * r[(n_t - i), ])
  }

  y <- c(rep(NA_real_, n_b + n_l), mod@model$fitted.values, preds)
  r_cat <- max.col(r, ties.method = "first")

  # Meta information:
  idx_fit <- (n_b + 1):(n_t - n_h)
  coefs <- cbind(coefs, series_sd(data[idx_fit, 1] - y[idx_fit], r_cat, n_r, na.rm = TRUE))
  ord <- regimes_order(coefs, rn_par, dims)

  meta <- list(
    coefs = `dimnames<-`(coefs[ord, ], dims),
    switches = mod@transMat
  )

  list(y = unname(y), r = ord[r_cat], meta = meta)
}


#' Get results from stats::kmeans
get_results$stats_km <- function(data, mod, n_b, n_h, n_t, n_r, n_l, rn_par, w_size) {
  n_l <- 1 # TODO: reimplement for n_l > 1
  dims <- list(
    rows = paste0("R", 1:n_r),
    cols = c("mu", paste0("rho", 1:n_l), "sigma")
  )

  # Regimes:
  r_pred <- integer(n_h)
  for (i in (n_h - 1):0) {
    w_idx <- n_t - i
    w_data <- data[(w_idx - w_size):(w_idx - 1), "y_l1"]

    data[, "avg"][w_idx] <- mean(w_data)
    data[, "acf"][w_idx] <- acor(w_data)
    data[, "sd"][w_idx] <- bare_sd(w_data)

    r_pred[i + 1] <- which.min(apply(mod$centers, 1, \(c) sum((c - data[w_idx, ])^2)))
  }
  r <- c(
    rep(NA_integer_, n_b),
    mod$cluster,
    r_pred
  )

  # Series:
  idx_fit <- (n_b + 1):(n_t - n_h)
  ar <- stats::lm(
    y ~ 0 + r + y_l1:r - 1,
    data = cbind(data[idx_fit, ], r = factor(r[idx_fit]))
  )

  y <- c(
    rep(NA_real_, n_b),
    stats::fitted(ar),
    stats::predict(ar, data.frame(r = factor(r_pred), y_l1 = data[(n_t - n_h + 1):n_t, "y_l1"]))
  ) # TODO: reimplement natively

  # Meta information:
  coefs <- c(
    ar$coefficients,
    series_sd(data[idx_fit, "y"] - y[idx_fit], r[idx_fit], n_r)
  ) |>
    matrix(n_r, 3)
  ord <- regimes_order(coefs, rn_par, dims)

  meta <- list(
    coefs = `dimnames<-`(coefs[ord, ], dims),
    switches = mod$centers
  )

  list(y = unname(y), r = ord[r], meta = meta)
}


# Add to get_results:
get_results$stats_rf <- function(data, mod, n_b, n_h, n_t, n_r, n_l, rn_par, w_size) {
  dims <- list(
    rows = "R1",
    cols = c("mu", paste0("rho", 1:n_l), "sigma")
  )

  idx_fit <- (n_b + 1):(n_t - n_h)
  preds_oos <- stats::predict(mod, newdata = as.data.frame(data[(n_t - n_h + 1):n_t, , drop = FALSE]))
  fitted_in <- stats::predict(mod, newdata = as.data.frame(data[idx_fit, , drop = FALSE]))

  y <- c(rep(NA_real_, n_b), fitted_in, preds_oos)
  r <- c(rep(NA_integer_, n_b), rep(1, n_t - n_b))
  meta <- list(
    coefs = `dimnames`(matrix(NA_real_, 1, 2 + n_l), dims)#,
    #importance = randomForest::importance(mod)
  )

  list(y = unname(y), r = r, meta = meta)
}


# Sanitizing enclosing environments
for (model_name in names(get_results)) {
  fn_env(get_results[[model_name]]) <- new_environment(
    list(
      series_sd = series_sd, regimes_order = regimes_order,
      acor = acor, bare_sd = bare_sd
    ), # TODO customize for each
    pkg_env("base")
  )
}



# Creators ---------------------------------------------------------------------

# Parameters always include n_r and n_p. Often include optimization
# parameters such as min_r_size, tol, and max_iter
# All return a generator function enclosing a child of base env carrying the
# hyperparameters, model function, and methods for getting predictions and
# regimes
# Model functions are passed via usual `::`, as box might interact weirdly with
# parallelism

#' Standard AR model
#' @export
ar <- function(
  n_r, n_l = 1
) {
  defaults <- c(
    as.list(current_env()), results = get_results$stats_lm
  )

  if (n_l != 1) stop("Only n_l = 1 is currently supported for ar().")
  if (n_r != 1) stop("Only n_r = 1 is supported for ar().")

  body <- expr({
    mod <- stats::lm(
      data[(n_b + n_l + 1):(n_t - n_h), 1] ~ data[(n_b + n_l):(n_t - n_h - 1), 1]
      # TODO: consider data[(n_b + 1 + n_l):(n_t - n_h), 1] ~ data[(n_b + 1):(n_t - n_h - n_l), 1]
      # and similar in the other models
      # Use names of data
    )
    results(data, mod, n_b, n_h, n_t, n_r, n_l)
  })

  new_function(
    args = exprs(data = , n_t = , n_h = , n_b = , rn_par = ),
    body = body,
    env = new_environment(defaults, pkg_env("base"))
  )
}

#' Structural breaks
#'
#' Might only work for n_l = 1.
#'
#' Comments on parameters:
#' - h set by eps1; model with intercept; no error treatments
#'
#' @export
sbreak <- function(
  n_r, n_l = 1,
  min_r_size = 0.25,
  tol = 1e-5, max_iter = 10
) {
  defaults <- c(
    as.list(current_env()), results = get_results$mbreaks_dofix
  )

  body <- expr({
    z_name <- grep("^y_l[0-9]+", colnames(data), value = TRUE)
    mod <- mbreaks::dofix(
      # Data:
      "y", z_name, x_name = NULL, data = data[(n_b + n_l + 1):(n_t - n_h), ],
      # Hyperparameters:
      fixn = n_r - 1,
      # Optimization:
      eps = tol, eps1 = min_r_size, maxi = max_iter, fixb = 0, betaini = NULL,
      # Others:
      prewhit = 0, robust = 0, hetdat = 0, hetvar = 0, hetq = 0, hetomega = 0,
      h = NULL, const = 1
    )
    results(data, mod, n_b, n_h, n_t, n_r, n_l, rn_par)
  })

  new_function(
    args = exprs(data = , n_t = , n_h = , n_b = , rn_par = ),
    body = body,
    env = new_environment(defaults, pkg_env("base"))
  )
}


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
  fn_env(g) <- pkg_env("base")

  defaults <- c(
    as.list(current_env()), results = get_results$tsdyn_setar
  )

  body <- expr({
    mod <- tsDyn::setar(
      # Data:
      data[(n_b + n_l + 1):(n_t - n_h), "y"], mL = n_l, mM = n_l, mH = n_l,
      thVar = g(data[, "y_l1"])[(n_b + n_l + 1):(n_t - n_h)],
      # Hyperparameters:
      nthresh = n_r - 1,
      # Optimization:
      trim = min_r_size,
      # Others:
      d = 1, steps = 1,
      include = "const", common = "none", model = "TAR", type = "level",
      restriction = "none", trace = FALSE
    )
    results(data, mod, n_b, n_h, n_t, n_r, n_l, rn_par, g = g)
  })

  new_function(
    args = exprs(data = , n_t = , n_h = , n_b = , rn_par = ),
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
    as.list(current_env()), results = get_results$tsdyn_lstar
  )

  body <- expr({
    gamma <- gamma %||% quote(expr = )
    mod <- tsDyn::lstar(
      # Data:
      data[(n_b + 1):(n_t - n_h), "y"], mL = n_l, mH = n_l, thDelay = n_l,
      # Hyperparameters:
      gamma = gamma,
      # Optimization:
      control = list(maxit = max_iter, abstol = tol),
      # Others:
      d = 1, steps = 1, include = "const", trace = FALSE
    )
    results(data, mod, n_b, n_h, n_t, n_r, n_l, rn_par)
  })

  new_function(
    args = exprs(data = , n_t = , n_h = , n_b = , rn_par = ),
    body = body,
    env = new_environment(defaults, pkg_env("base"))
  )
}


#' Model: Markov switching
#'
#' Comments on parameters:
#' - All coefficients switch between regimes, but not sigma
#'
#' @export
markov <- function(
  n_r = 2, n_l = 1, gamma = NULL,
  min_r_size = 0.1,
  tol = 1e-5, max_iter = 10
) {
  defaults <- c(
    as.list(current_env()), results = get_results$mswm_msmfit
  )

  body <- expr({
    mod <- MSwM::msmFit(
      # Data:
      y ~ 1, k = n_r, p = n_l, data = as.data.frame(data[(n_b + 1):(n_t - n_h), ]),
      # Optimization:
      control = list(maxiter = max_iter, tol = tol, parallelization = FALSE),
      # Others:
      sw = c(rep(TRUE, n_l + 1), FALSE)
    )
    results(data, mod, n_b, n_h, n_t, n_r, n_l, rn_par)
  })

  new_function(
    args = exprs(data = , n_t = , n_h = , n_b = , rn_par = ),
    body = body,
    env = new_environment(defaults, pkg_env("base"))
  )
}


#' Model: K-means
#' @export
km <- function(
  n_r, n_l = 1, w_size = 5
) {
  defaults <- c(
    as.list(current_env()), results = get_results$stats_km,
    acor = acor, bare_sd = bare_sd
  )

  body <- expr({
    data <- cbind(data, avg = NA_real_, acf = NA_real_, sd = NA_real_)
    idx_fit <- (n_b + 1):(n_t - n_h)

    for (t in 1:(length(idx_fit) - w_size - 1)) {
      w_idx <- t + w_size + 1
      w_data <- data[(t + 1):(w_idx), "y_l1"]

      data[idx_fit, "avg"][w_idx] <- mean(w_data)
      data[idx_fit, "acf"][w_idx] <- acor(w_data)
      data[idx_fit, "sd"][w_idx] <- bare_sd(w_data)
    }
    data[idx_fit, "avg"][1:w_size] <- mean(data[(n_b + w_size + 1):(n_t - n_h), "avg"])
    data[idx_fit, "acf"][1:w_size] <- mean(data[(n_b + w_size + 1):(n_t - n_h), "acf"])
    data[idx_fit, "sd"][1:w_size] <- mean(data[(n_b + w_size + 1):(n_t - n_h), "sd"])
    data[is.na(data)] <- 0

    mod <- stats::kmeans(data[idx_fit, ], n_r) # TODO: shuold not include y_t?
    results(data, mod, n_b, n_h, n_t, n_r, n_l, rn_par, w_size)
  })

  new_function(
    args = exprs(data = , n_t = , n_h = , n_b = , rn_par = ),
    body = body,
    env = new_environment(defaults, pkg_env("base"))
  )
}


# Creator: random forest
#' @export
rf <- function(n_r = 1, n_l = 1, w_size = 5, ntree = 50, mtry = NULL) {
  defaults <- c(
    as.list(current_env()), results = get_results$stats_rf,
    acor = acor, bare_sd = bare_sd
  )

  body <- expr({
    model.frame <- stats::model.frame

    data <- cbind(data, avg = NA_real_, acf = NA_real_, sd = NA_real_)
    idx_fit <- (n_b + 1):(n_t - n_h)

    for (t in 1:(length(idx_fit) - w_size - 1)) {
      w_idx <- t + w_size + 1
      w_data <- data[(t + 1):(w_idx), "y_l1"]
      data[idx_fit, "avg"][w_idx] <- mean(w_data)
      data[idx_fit, "acf"][w_idx] <- acor(w_data)
      data[idx_fit, "sd"][w_idx] <- bare_sd(w_data)
    }
    data[idx_fit, "avg"][1:w_size] <- mean(data[(n_b + w_size + 1):(n_t - n_h), "avg"])
    data[idx_fit, "acf"][1:w_size] <- mean(data[(n_b + w_size + 1):(n_t - n_h), "acf"])
    data[idx_fit, "sd"][1:w_size] <- mean(data[(n_b + w_size + 1):(n_t - n_h), "sd"])
    data[is.na(data)] <- 0

    # Build training data: response + predictors (exclude 'y' from predictors when needed by other code)
    predictors <- setdiff(colnames(data), "y")
    train_df <- as.data.frame(cbind(y = data[idx_fit, "y"], data[idx_fit, predictors, drop = FALSE]))
    mod <- randomForest::randomForest(
      y ~ ., data = train_df, ntree = ntree,
      mtry = max(1, floor((ncol(train_df) - 1) / 4)),
      nodesize = 7, maxnodes = 100#,
      #importance = FALSE
    )

    # Ensure predict rows have same predictor columns
    results(data, mod, n_b, n_h, n_t, n_r, n_l, rn_par, w_size)
  })

  new_function(
    args = exprs(data = , n_t = , n_h = , n_b = , rn_par = ),
    body = body,
    env = new_environment(defaults, pkg_env("base"))
  )
}
