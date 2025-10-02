
# Setup ------------------------------------------------------------------------

# General modules:
box::use(
  src/utils[...],
  bench[mark]
)

# Parallel modules:
box::use(
  mirai[...],
  parallel[...],
  future[...], future.apply[...],
  rTRNG[...], dqrng[...]
)
# Others: crew, parallelly, furrr, foreach, doFuture

# Models modules:
box::use(
  schange = strucchange,
  mbreaks = mbreaks,
  tsdyn = tsDyn,
  star = starvars,
  mstest = MSTest,
  mswm = MSwM
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



# Parallel Tests ---------------------------------------------------------------

# Old tests removed because of clutter. Old conclusions that still hold:
# - No reason here not to use the load balanced options
# - future is probably the worst in terms of speed, mirai possibly the best but
# parallel might compete

# Questions to answer:
# - Iterate over collection or pass collection and iterate over its names?
# - Best loop orders?



# Models -----------------------------------------------------------------------

# At the correct time, import only the function used, same with mirai


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

# Standardizing
mod <- mbreaks$dofix(
  "y", "ly", data = data[(1 + n_p):(n_t - n_h), ], fixn = 2,
  prewhit = 0, robust = 0, hetdat = 0, hetvar = 0, hetq = 0, hetomega = 0
)
mod |> str()
fitted(mod)

coefs <- mod$beta[(2 * n_coef + 1):(3 * n_coef)]
x <- c(1, data$ly[])

y <- c(data$y[1:n_p], fitted(mod))
#...


# Threshold:
models <- exprs(
  tsdyn = tsdyn$setar(
    data$y[(1 + n_p):(n_t - n_h)], m = 1, nthresh = 2,
    thVar = data$ly[(1 + n_p):(n_t - n_h)],
    d = 1, steps = 1, trim = 0.15
  )
)

mod = tsdyn$setar(
  data$y[(1 + n_p):(n_t - n_h)], m = 1, nthresh = 2,
  thVar = data$ly[(1 + n_p):(n_t - n_h)],
  d = 1, steps = 1, trim = 0.15
)

inject(mark(
  !!!models,
  check = FALSE, , min_iterations = 2
))

mod |> str()
mod$coefficients
mod$fitted.values
mod$model.specific$regime
predict(mod, thVar = data$ly[(n_t - n_h + 1):n_t])


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

mod

library(MSTest)

mod <- mstest$MSARmdl(
  matrix(data$y[1:(n_t - n_h)], n_t - n_h), p = 1, k = 3,
  control = list(getSE = FALSE, thtol = 1e-5, msvar = FALSE)
)

# Clustering:
x <- data |>
  mutate(
    l2y = lag(y, 2, default = 0),
    l3y = lag(y, 3, default = 0),
    rollmean = zoo::rollmean(y, 5, fill = 0, align = "right"),
    rollsd = zoo::rollapply(y, 5, sd, fill = 0, align = "right"),
    rollautocor = zoo::rollapply(y, 5, fill = 0, align = "right", \(x) {
      cor(x[-1], lag(x)[-1])
    }),
    t = 1:n_t
  )


mod <- kmeans(x[1:(n_t - n_h), ], centers = 3)
mod |> str()
mod$cluster

mbreaks:::dofix()
mbreaks:::estim()
mbreaks:::OLS()

arima()



# Metrics ----------------------------------------------------------------------

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
      models = map(models$options[unique(model_names$model)], safely),
      lag = lag,
      n_m = n_m, n_t = n_t, n_h = n_h,
      parallel = FALSE, safely = FALSE
    )
  },
  b = {
    get_results(
      simulations_ys, estimate_models,
      models = map(models$options[unique(model_names$model)], safely),
      lag = lag,
      n_m = n_m, n_t = n_t, n_h = n_h,
      parallel = TRUE, safely = FALSE
    )
  }
)
