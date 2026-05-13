
# Setup ------------------------------------------------------------------------

# General modules:
box::use(
  src/utils[...],
  bench[mark]
)

# Models modules:
box::use(
  schange = strucchange,
  mbreaks = mbreaks,
  tsdyn = tsDyn,
  star = starvars,
  mstest = MSTest,
  mswm = MSwM
)


# n_r --------------------------------------------------------------------------

r = sample(c(1, 2), 100, replace = TRUE)

bench::mark(
  length(unique(r)),
  max(r)
)


# Dispersion metrics -----------------------------------------------------------

y = 1
x = c(1, 2.5, -7)

bench::mark(
  check = FALSE,
  x[1] - x[2],
  dist(x),
  mean(sapply(seq_along(x), \(i) abs(x[i] - x[-i]))),
  vapply(seq_along(x), \(i) abs(x[i] - x[-i]), double(2)),
  {
    n <- 1
    vapply(1:3, \(i) abs(x[i] - x[-i]), double(3 - 1))
  },
  y,
  dist(y)
)

n <- length(y) - 1
dists <- vapply(seq_along(y), \(i) abs(y[i] - y[-i]), double(n))

length(dists)


str(dists)

mean(dists)
i = 1
dist(x) |> mean()
x - rbind(x[-1], x[-2], x[-3])

mean(sapply(seq_along(x), \(i) abs(x[i] - x[-i])^k))

for (i in seq_along(x)) {
  abs(x[i] - x[-i])
}

n <- length(x)
seq(1, n^2, by = n)

expand.grid(x, x)[-c]


unclass(dist(y))


# AR model ---------------------------------------------------------------------

bench::mark(
  check = FALSE,
  arima1 = stats::arima(1:100, c(1, 0, 0)),
  arima2 = stats::arima(1:100, c(1, 0, 0), method = "CSS"),
  lm = stats::lm(1:100 ~ lag(1:100, default = 0))
)



# Dimnames ---------------------------------------------------------------------

x <- matrix(1:9, 3, 3)

f1 <- function() {
  colnames(x) <- c("a", "b", "c")
  x <- x[order(x[, "b"]), ]
  `rownames<-`(x, c("1", "2", "3"))
}

f2 <- function() {
  cols <- c("a", "b", "c")
  x <- x[order(x[, which("b" == cols)]), ]
  `dimnames<-`(x, list(c("1", "2", "3"), cols))
}

f3 <- function() {
  x <- x[order(x[, which("b" == c("a", "b", "c"))]), ]
  `dimnames<-`(x, list(c("1", "2", "3"), c("a", "b", "c")))
}

bench::mark(
  check = FALSE,
  f1(),
  f2(),
  f3()
)



# T index ----------------------------------------------------------------------

t1 <- 1:100
t2 <- 1:1000

bench::mark(
  t1 %in% (7:83),
  t1 == max(min(t1), 7):min(max(t1), 83),
  t2 %in% (7:83),
  t2 == max(min(t2), 7):min(max(t2), 83)
)



# Getting Metrics --------------------------------------------------------------

simulations_tmp <- read_rds("data/simulations.rds")
simulations_tmp <- map(simulations_tmp, "result")
n_t <- 120

data_tmp <- imap(simulations_tmp[sample(length(simulations_tmp), 5000)], \(res, sim_name) {
  sim_opts <- str_split_1(sim_name, "-")
  tibble(
    sgp = fct(sim_opts[1]), rgp = fct(sim_opts[2]),
    sim = as.integer(sim_opts[3]),
    t = 1:n_t, y = res$y, r = max.col(res$r, ties.method = "first")
  )
}) |>
  bind_rows()

mark(
  check = FALSE,
  dplyr = data_tmp |>
    group_by(sgp, rgp, sim) |>
    summarise(
      sgp_metric_est = metrics$sgp_metric(sgp[1], y, r) |> sd(),
      rgp_metric_est = metrics$rgp_metric(rgp[1], y, r) |> sd(),
      a = abs(sgp_metric_est - rgp_metric_est)
    ),
  dtplyr = dtplyr::lazy_dt(data_tmp) |>
    group_by(sgp, rgp, sim) |>
    summarise(
      sgp_metric_est = metrics$sgp_metric(sgp[1], y, r) |> sd(),
      rgp_metric_est = metrics$rgp_metric(rgp[1], y, r) |> sd()
    ) |>
    ungroup() |>
    mutate(a = abs(sgp_metric_est - rgp_metric_est)) |>
    as_tibble()
)



# Unnesting --------------------------------------------------------------------

x <- map(1:50, ~ list(1, 2, 3, 4))

bench::mark(
  manual = map_dfr(x, ~ list(a = .x[[1]], b = .x[[2]], c = .x[[3]], d = .x[[4]])),
  unnest = map_dfr(x, \(z) {
    names(z) <- c("a", "b", "c", "d")
    list(z = list(z))
  }) |>
    unnest_wider(z)
)



# Transmat ---------------------------------------------------------------------

r <- sample(3, 100, replace = TRUE)
n_r <- length(unique(r))

r_diff <- c(0, diff(r))
idxs <- which(r_diff != 0)

bench::mark(
  check = FALSE,
  {
    r_lead <- r[1:(n_t - 1)]
    a <- table(factor(paste0(r[1:(n_t - 1)], "_", r_lead + diff(r))))
    counts <- matrix(a, n_r, n_r, byrow = TRUE)
    counts
  },
  {
    r_lead <- r[1:(n_t - 1)]
    a <- tabulate(factor(sort(paste0(r[1:(n_t - 1)], "_", r_lead + diff(r)))))
    counts <- matrix(a, n_r, n_r, byrow = TRUE)
    counts
  }, # inconsistent when a regime does not appear
  {
    counts <- matrix(0, n_r, n_r)
    for (idx in 2:length(r)) {
      r_before <- r[idx - 1]
      r_after <- r_before + r_diff[idx]
      counts[r_before, r_after] <- counts[r_before, r_after] + 1
    }
    counts
  }
)



# Matrix indexing --------------------------------------------------------------

m <- matrix(1:3^4, 9, 9)

ind <- c(0, 1, 0)

mark(
  m[as.logical(ind), ],
  m[ind == 1, ],
  m[ind > 0, ],
  m[ind != 0, ]
)



# Multiplication ---------------------------------------------------------------

a <- 1:10
b <- matrix(a, 10, 1)
c <- matrix(a, 1, 10)

mark(
  check = FALSE,
  c %*% b,
  as.numeric(c %*% b),
  t(b) %*% b,
  sum(a * a)
)


# Lag --------------------------------------------------------------------------

lag <- function(x, n = 1L, default = NA) {
  c(rep(default, n), x[-(length(x) - seq_len(n) + 1)])
}

mark(
  lag(1:10, 3),
  dplyr::lag(1:10, 3)
)



# Mirai Behavior ---------------------------------------------------------------

# Mirai and loaded packages/variables:
g <- new_function(
  args = pairlist2(x = , y = ),
  body = expr({
    Sys.sleep(0.1)
    x + y
  }),
  env = pkg_env("graphics")
)

daemons(4)
res <- mirai_map(
  1:2,
  function(x) {
    stats::lm(rnorm(100) ~ rnorm(100))
    g(x, 10)
  },
  g = g
) |>
  collect_mirai()

res

# Mirai and closure env:
e1 <- new_environment(list(AAA = 1), pkg_env("base"))
f1 <- new_function(exprs(i = ), expr(i + AAA), env = e1)
e2 <- new_environment(list(AAA = 2), pkg_env("base"))
f2 <- new_function(exprs(i = ), expr(i + AAA), env = e2)

daemons(2)
mirai_map(1:2, f1, AAA = 10) |> collect_mirai()
mirai_map(1:2, f2, AAA = 10) |> collect_mirai()

f3 <- new_function(
  exprs(n = ), expr({
    list(
      fun = rlang::caller_fn(n),
      fun_env_contents = names(rlang::fn_env(rlang::caller_fn(n)))[1:10],
      env = rlang::caller_env(n),
      env_contents = names(rlang::caller_env(n))
    )
    #print(XXX)
  }), env = e2
)

mirai_map(0:4, f3, XXX = "aqui") |> collect_mirai()

# Attempt to debug env lookup:
# print_lookup_envs <- function(i, env = rlang::caller_env()) {
#   if (identical(env, baseenv())) {
#     print(env)
#   } else {
#     print(env)
#     print_lookup_envs(rlang::env_parent(env))
#   }
# }

# Conclusion: mirai probably puts the ... in some env and evaluates the function
# in it at some point unknown. Still, the env of the function is respected and
# passed to the workers, it being the first search path and having priority over
# values in ...

# Mirai and errors:
daemons(4)
mirai_map(exprs(1, stop("err")), eval) |> collect_mirai()
mirai_map(exprs(1, stop("err")), safely(eval)) |> collect_mirai()

daemons(4)
res <- mirai_map(c(1e-6, 1e6), Sys.sleep)
daemons(0)
collect_mirai(res)

daemons(4)
res <- mirai_map(c(1e-6, 1e6), safely(Sys.sleep))
daemons(0)
collect_mirai(res)

# Conclusion: connection errors are caught but not by safely



# Models -----------------------------------------------------------------------

set.seed(42)
data <- tibble(
  y = c(rnorm(30, 2), rnorm(40, 0), rnorm(30, 1)),
  ly = lag(y, default = 0)
)

n_p <- 1
n_t <- 100
n_h <- 1
n_coef <- n_p + 1

# Structural breaks:
models <- exprs(
  schange_bp = schange$breakpoints(
    y ~ ly, data = data[(1 + n_p):(n_t - n_h), ], breaks = 3, h = 0.15,
    tol = 1e-5, qr.tol = 1e-5
  ) |> coef(),
  # h!, start end, tol sqrt(.Machine$double.eps)/ncol(x), 1e-7
  # engine = "C"
  mbreaks_sequa = mbreaks$dosequa(
    "y", "ly", data = data[(1 + n_p):(n_t - n_h), ], m = 3, eps1 = 0.15,
    eps = 1e-5,
    prewhit = 0, robust = 0, hetdat = 0, hetvar = 0, hetq = 0, hetomega = 0,
  ) |> _[["beta"]] |> matrix(3, 2, byrow = TRUE),
  # eps = 1e-05 (tol), eps1 = h?
  mbreaks_repart = mbreaks$dorepart(
    "y", "ly", data = data[(1 + n_p):(n_t - n_h), ], m = 3, eps1 = 0.15,
    eps = 1e-5,
    prewhit = 0, robust = 0, hetdat = 0, hetvar = 0
  ) |> _[["beta"]] |> matrix(3, 2, byrow = TRUE),
  mbreaks_order = mbreaks$doorder(
    "y", "ly", data = data[(1 + n_p):(n_t - n_h), ], m = 3, eps1 = 0.15, ic = "BIC",
    eps = 1e-5,
    prewhit = 0, robust = 0, hetdat = 0, hetvar = 0, hetq = 0, hetomega = 0,
  ) |> _[["beta"]] |> matrix(3, 2, byrow = TRUE),
  mbreaks_fixed = mbreaks$dofix(
    "y", "ly", data = data[(1 + n_p):(n_t - n_h), ], fixn = 2,
    prewhit = 0, robust = 0, hetdat = 0, hetvar = 0, hetq = 0, hetomega = 0,
  ) |> _[["beta"]] |> matrix(3, 2, byrow = TRUE)
)

walk(models, ~ print(eval(.x)))

bench <- inject(mark(
  !!!models,
  check = FALSE, , min_iterations = 2
))
bench

# Threshold:
models <- exprs(
  tsdyn = tsdyn$setar(
    data$y[(1 + n_p):(n_t - n_h)], m = 1, nthresh = 2,
    thVar = data$ly[(1 + n_p):(n_t - n_h)],
    d = 1, steps = 1, trim = 0.15
  )
)

inject(mark(
  !!!models,
  check = FALSE, , min_iterations = 2
))

# Smooth transition:
models <- exprs(
  tsdyn = tsdyn$lstar(
    data$y[(1 + n_p):(n_t - n_h)], m = 1,
    thVar = data$ly[(1 + n_p):(n_t - n_h)],
    d = 1, steps = 1
  ),
  # only for 2
  #star = {
  #  y = data$y[(1 + n_p):(n_t - n_h)]
  #  ly = data$ly[(1 + n_p):(n_t - n_h)]
  #  stvalues <- star$startingVLSTAR(y, p = 1, n.combi = 3, singlecgamma = FALSE, st = ly, ncores = 1)
  #  star$VLSTAR(
  #    y, p = 1, m = 2,
  #    st = ly,
  #    ncores = 1, starting = stvalues
  #  )
  #}
)

walk(models, ~ print(eval(.x)))

inject(mark(
  !!!models,
  check = FALSE, , min_iterations = 2
))

# Markov switching:
models <- exprs(
  mswm = mswm$msmFit(
    y ~ 1, data = data[(1 + n_p):(n_t - n_h), ], k = 3, p = 1,
    sw = rep(TRUE, 3), control = list(tol = 1e-5, parallelization = FALSE)
  )
)

inject(mark(
  !!!models,
  check = FALSE, , min_iterations = 2
))



# Metrics ----------------------------------------------------------------------

mean_pairwise_dist <- function(x, ...) {
  mean(dist(x, ...))
}

diff_k_2 <- function(x, p = 1) {
  abs(x[1] - x[2])^p
}

mean_pairwise_dist2 <- function(x, k, ...) {
  mean(abs(dist(x, ...))^k)
}

mean_pairwise_dist3 <- function(x, k, ...) {
  if (k %% 2 == 0) {
    mean(dist(x, ...)^k)
  } else {
    mean(abs(dist(x, ...))^k)
  }
}

mark(
  check = FALSE,
  diff_k_2(1:5, p = 2),
  mean_pairwise_dist2(1:5, k = 2),
  mean_pairwise_dist3(1:5, k = 2)
)


a[[1]]
y <- a[[1]]$y
r <- ifelse(a[[1]]$r %in% c(1, 2), a[[1]]$r, NA)
n_r_hat <- 2

mark(
  check = FALSE,
  vapply(.Internal(split(y, as.factor(r))), mean, FUN.VALUE = numeric(1)),
  vapply(1:n_r_hat, \(i) mean(y[r == i], na.rm = TRUE), numeric(1))
)

mark(
  check = FALSE,
  vapply(1:n_r_hat, FUN.VALUE = numeric(1), FUN = \(s) {
    cor(y[r == s][-1], lag(y[r == s], 1)[-1])
  }),
  lapply(1:n_r_hat, \(s) {
    cor(y[r == s][-1], lag(y[r == s], 1)[-1])
  })
)

mark(
  check = FALSE,
  vapply(1:n_r_hat, FUN.VALUE = numeric(1), FUN = \(s) {
    abs(c(0, diff(r))) |> cumsum() |> _[r == s] |> table() |> mean()
  }),
  vapply(1:n_r_hat, FUN.VALUE = numeric(1), FUN = \(s) {
    mean(table(cumsum(abs(c(0, diff(r))))[r == s]))
  })
)
expression(abs(c(0, diff(r))) |> cumsum() |> _[r == s] |> table() |> mean())

mark(
  check = FALSE, min.iterations = 2,
  a = {
    get_results(
      simulations_ys, estimate_models,
      models = map(options$models[unique(model_names$model)], safely),
      lag = lag,
      n_m = n_m, n_t = n_t, n_h = n_h,
      parallel = FALSE, safely = FALSE
    )
  },
  b = {
    get_results(
      simulations_ys, estimate_models,
      models = map(options$models[unique(model_names$model)], safely),
      lag = lag,
      n_m = n_m, n_t = n_t, n_h = n_h,
      parallel = TRUE, safely = FALSE
    )
  }
)
