
# Setup ------------------------------------------------------------------------

box::use(src/utils[...])

library(syrup)

library(parallel) # library(parallelly)
library(future)
library(future.apply) # library(furrr), library(foreach), library(doFuture)
library(mirai) # library(crew)

library(rTRNG)
library(dqrng)



# Parallel Tests ---------------------------------------------------------------

# Test functions
test_parallel <- function(
  x, f, label, ncl = 1,
  type = "PSOCK", # "MIRAI"
  static_scheduling = TRUE, chunk_size = NULL, ...
) {
  expr <- if (static_scheduling) {
    expr(parLapply(cl, x, f, ..., chunk.size = chunk_size))
  } else {
    expr(parLapplyLB(cl, x, f, ..., chunk.size = chunk_size))
  }

  cl <- makeCluster(ncl, type = type)
  results <- bench::mark(
    eval(expr),
    max_iterations = 100, min_iterations = 3, check = FALSE
  )
  stopCluster(cl)

  results$expression <- label
  results
}

test_future <- function(
  x, f, label, ncl = 1,
  strategy = multisession, # future.mirai::mirai_multisession, future.callr::callr
  future.scheduling = 1, ...
) {
  plan(strategy, workers = ncl)

  results <- bench::mark(
    future_lapply(
      x, f,
      future.scheduling = future.scheduling,
      future.label = FALSE,
      ...
    ),
    max_iterations = 100, min_iterations = 3, check = FALSE
  )

  results$expression <- label
  results
}

test_mirai <- function(
  x, f, label, ncl = 1,
  dispatcher = TRUE, # FALSE, NA
  ...
) {
  daemons(ncl, dispatcher = dispatcher)
  results <- bench::mark(
    mirai = collect_mirai(mirai_map(x, f, ...)),
    max_iterations = 100, min_iterations = 3, check = FALSE
  )
  daemons(0)

  results$expression <- label
  results
}

print_tests <- function(..., cols = c(expression:median, mem_alloc:n_itr)) {
  bind_rows(...) %>%
    select({{cols}}) %>%
    print()
}


# Syrup
ggsyrup <- function(data, col, fork = FALSE) {
  if (fork) worker_ppid <- ps::ps_pid()

    res_mem %>%
    {if (fork) filter(., ppid == worker_ppid | pid == worker_ppid) else .} %>%
    ggplot() +
    aes(x = id, y = {{col}}, group = pid) +
    geom_line() +
    scale_x_continuous(breaks = 1:max(res_mem$id))
}



# Setup
N <- 1000
ncl <- 8

errors <- map(1:N, ~ rnorm(100))
dgps <- list(cumsum, cummax, cummin)


# Better to send data or ids of it?
x1 <- errors
f1 <- cumsum
x2 <- seq_along(errors)
f2 <- \(ind, ...) {
  if (! exists("errors")) errors <- list(...)$errors
  cumsum(errors[[ind]])
}

print_tests(
  test_parallel(x1, f1, "1par_psock_dyn", ncl, static_scheduling = FALSE),
  test_parallel(x2, f2, "1par_psock_dyn", ncl, static_scheduling = FALSE, errors = errors),
  test_mirai(x1, f1, "1mirai_dyn", ncl, dispatcher = TRUE),
  test_mirai(x2, f2, "1mirai_dyn", ncl, dispatcher = TRUE, errors = errors),
  test_future(x1, f1, "1fut_par_dyn", ncl, future.packages = "base"),
  #test_future(x2, f2, "2fut_par_dyn", ncl, future.packages = "base"),
  test_future(x2, f2, "2fut_par_dyn_globals", ncl, future.packages = "base", future.globals = list(errors = errors))
)

# 2 has better performance in future, but parallel and mirai dont support it,
#and have better performance than future. Maybe in very expensive tasks where
#future's overhead is negligible, it is better to use it with 2.


# When is it worth it to make chunks more coarse?
#costly setup, uniform, expensive task
bigthing <- round(rnorm(1000), 5) #object.size(bigthing)
x1 <- map(1:100, ~ bigthing)
f1 <- function(x) {
  Sys.sleep(0.00001)
}

print_tests(
  test_parallel(x1, f1, "par_psock_static", 2, chunk_size = 50),
  test_parallel(x1, f1, "par_mirai_static", 2, type = "MIRAI", chunk_size = 50),
  test_parallel(x1, f1, "par_psock_dyn", 2, static_scheduling = FALSE, chunk_size = 4),
  test_parallel(x1, f1, "par_mirai_dyn", 2, type = "MIRAI", static_scheduling = FALSE, chunk_size = 4)
)


# How each engine deals with non-uniform tasks?
x1 <- seq(1, length.out = 1000, by = 0.1)
f1 <- function(x) {
  Sys.sleep(0.00001 * x)
}

print_tests(
  test_parallel(x1, f1, "par_psock_dyn", ncl, static_scheduling = FALSE),
  test_parallel(x1, f1, "par_mirai_dyn", ncl, type = "MIRAI", static_scheduling = FALSE),
  test_mirai(x1, f1, "mirai_mirai_dyn", ncl, dispatcher = TRUE),
  test_future(x1, f1, "fut_par_dyn", ncl, future.packages = "base"),
  test_future(x1, f1, "fut_mirai_dyn", ncl, future.packages = "base", strategy = future.mirai::mirai_multisession)
)


# Worth to use static scheduling with expensive tasks?
x <- 1:400
f <- \(x) Sys.sleep(0.01)

results3 <- bind_rows(
  test_parallel(x, f, "par_mirai_stat", ncl, type = "MIRAI", static_scheduling = TRUE),
  test_mirai(x, f, "mirai_mirai_stat", ncl, dispatcher = FALSE),
  test_parallel(x, f, "par_mirai_dyn", ncl, type = "MIRAI", static_scheduling = FALSE),
  test_mirai(x, f, "mirai_mirai_dyn", ncl, dispatcher = TRUE),
) %>% select(expression:median, mem_alloc, `gc/sec`, n_itr) %>% print()


# Changing the order of nested loops to parallelize
errors <- map(1:N, ~ rnorm(100))
dgps <- map(10^seq(-4, -8, by = -0.25), \(k) {
  inject(\(x) {
    return(Sys.sleep(!!k))
    !!fn_body(lm) #assume a big function body
  })
})

opts1 <- map(dgps, ~ list(dgp = .x, errors = errors)) # Parellelize over dgps
opts2 <- map(errors, ~ list(dgps = dgps, error = .x)) # Parallelize over errors
opts3 <- expand_grid(dgps, errors) %>% pmap(list) # Parallelize the pairs
walk(list(opts1, opts2, opts3), ~ print(object.size(.x)))

f1 <- \(opt) lapply(opt$errors, \(error) opt$dgp(error))
f2 <- \(opt) lapply(opt$dgps, \(dgp) dgp(opt$error))
f3 <- \(opt) opt$dgp(opt$error)

print_tests(
  test_parallel(opts1, f1, "1", ncl, type = "MIRAI", static_scheduling = FALSE),
  test_parallel(opts2, f2, "2", ncl, type = "MIRAI", static_scheduling = FALSE),
  test_parallel(opts2, f2, "2_static", ncl, type = "MIRAI", static_scheduling = TRUE, chunk_size = 50),
  test_parallel(opts3, f3, "3", ncl, type = "MIRAI", static_scheduling = FALSE),
)

print_tests(
  test_mirai(opts1, f1, "1", ncl),
  test_mirai(opts2, f2, "2", ncl),
  test_mirai(opts2, f2, "2_static", ncl, dispacher = FALSE),
  test_mirai(opts3, f3, "3", ncl),
)

# Manual testing
cl <- makeCluster(ncl, type = "MIRAI")
bench::mark(
  over_dgps = parLapplyLB(cl, opts1, f1, chunk.size = 1),
  over_errors = parLapplyLB(cl, opts2, f2, chunk.size = 24),
  over_both = parLapplyLB(cl, opts3, f3, chunk.size = 1000),
  check = FALSE
) %>% select(expression:median, mem_alloc:n_itr) %>% print()
stopCluster(cl)

daemons(8)
bench::mark(
  over_dgps = mirai_map(opts1, f1),
  over_errors = mirai_map(opts2, f2),
  over_both = mirai_map(opts3, f3),
  check = FALSE
) %>% select(expression:median, mem_alloc:n_itr) %>% print()
daemons(0)

print_tests(
  #test_parallel(opts2, f2, "par_psock_stat", ncl, static_scheduling = TRUE),
  test_parallel(opts2, f2, "par_mirai_stat", ncl, type = "MIRAI", static_scheduling = TRUE),
  #test_parallel(opts2, f2, "par_psock_dyn", ncl, static_scheduling = FALSE),
  test_parallel(opts2, f2, "par_mirai_dyn", ncl, type = "MIRAI", static_scheduling = FALSE),
  test_mirai(opts2, f2, "mirai_mirai_dyn", ncl, dispatcher = TRUE),
  test_mirai(opts2, f2, "mirai_mirai_dyn", ncl, dispatcher = FALSE)
)

# Syrup
plan(future.mirai::mirai_multisession, workers = ncl)

syrup1 <- syrup({
  future_lapply(opts1, f1)
})
ggsyrup(syrup1, rss)
ggsyrup(syrup1, pct_cpu)

syrup2 <- syrup({
  cl <- makeCluster(ncl, type = "MIRAI")
  parLapplyLB(cl, opts1, f1)
  stopCluster(cl)
})

syrup3 <- syrup({
  cl <- makeCluster(ncl, type = "MIRAI")
  parLapplyLB(cl, opts1, f1)
  stopCluster(cl)
})


# RNG
TRNGseed(117)
RcppParallel::setThreadOptions(numThreads = 8)

cl1 <- makeCluster(8, type = "MIRAI")
clusterEvalQ(cl, library("dqrng"))

cl2 <- makeCluster(8, type = "MIRAI")
clusterSetRNGStream(cl2, iseed = 0591823)

daemons(8)
everywhere(library("dqrng"))

bench::mark(
  rtrng = rnorm_trng(1e5, parallelGrain = 100),
  dqrng = {
    clusterApply(cl, 1:8, function(stream) {
      dqRNGkind("Threefry")
      dqset.seed(117, stream)
      dqrnorm(1e5)
    })
  },
  dqrng_mirai = {
    collect_mirai(mirai_map(1:8, function(stream) {
      dqRNGkind("Threefry")
      dqset.seed(117, stream)
      dqrnorm(1e5)
    }))
  },
  mirai = collect_mirai(mirai_map(1:8, function(i) rnorm(1e5))),
  parallel = clusterApply(cl2, 1:8, function(i) rnorm(1e5)),
  check = FALSE
)

stopCluster(cl1); stopCluster(cl2); daemons(0)



# Mirai Behavior ---------------------------------------------------------------

g <- new_function(
  args = pairlist2(x = , y = ),
  body = expr({
    Sys.sleep(0.1)
    x + y
  }),
  env = pkg_env("graphics")
)

mirai::daemons(0)
mirai_map(
  1:2,
  function(x) {
    g(x, 10)
    stats::lm(rnorm(100) ~ rnorm(100))
  },
  g = g
) |>
  collect_mirai()



# Matrix indexing --------------------------------------------------------------

m <- matrix(1:3^4, 9, 9)

ind <- c(0, 1, 0)

bench::mark(
  m[as.logical(ind), ],
  m[ind == 1, ],
  m[ind > 0, ],
  m[ind != 0, ]
)



# Multiplication ---------------------------------------------------------------

a <- 1:10
b <- matrix(a, 10, 1)
c <- matrix(a, 1, 10)

bench::mark(
  check = FALSE,
  c %*% b,
  as.numeric(c %*% b),
  t(b) %*% b,
  sum(a * a)
)



# Models -----------------------------------------------------------------------

install.packages("MSTest")

# At the correct time, import only the function used, same with mirai
box::use(
  schange = strucchange,
  mbreaks = mbreaks,
  tsdyn = tsDyn,
  star = starvars,
  mstest = MSTest,
  mswm = MSwM
)

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

bench <- inject(bench::mark(
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

inject(bench::mark(
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

inject(bench::mark(
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

inject(bench::mark(
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



# Lag ----------------------------------------------------------

lag <- function(x, n = 1L, default = NA) {
  c(rep(default, n), x[-(length(x) - seq_len(n) + 1)])
}

bench::mark(
  lag(1:10, 3),
  dplyr::lag(1:10, 3)
)



# Mirai and function env -------------------------------------------------------

e1 <- new_environment(list(AAA = 1), pkg_env("base"))
f1 <- new_function(exprs(i = ), expr(i + AAA), env = e1)
e2 <- new_environment(list(AAA = 2), pkg_env("base"))
f2 <- new_function(exprs(i = ), expr(i + AAA), env = e2)

daemons(2)
mirai::mirai_map(1:2, f1, AAA = 10) |> collect_mirai()
mirai::mirai_map(1:2, f2, AAA = 10) |> collect_mirai()

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

mirai::mirai_map(0:4, f3, XXX = "aqui") |> collect_mirai()

# Conclusion: mirar probably puts the ... in some env and evaluates the function
#in it at some point unknown. Still, the env of the function is respected and
#passed to the workers, it being the first search path and having priority over
#values in ...

# print_lookup_envs <- function(i, env = rlang::caller_env()) {
#   if (identical(env, baseenv())) {
#     print(env)
#   } else {
#     print(env)
#     print_lookup_envs(rlang::env_parent(env))
#   }
# }



# Metrics ----------------------------------------------------------------------

a[[1]]
y = a[[1]]$y
r = ifelse(a[[1]]$r %in% c(1, 2), a[[1]]$r, NA)
n_r_hat <- 2

bench::mark(
  check = FALSE,
  vapply(.Internal(split(y, as.factor(r))), mean, FUN.VALUE = numeric(1)),
  vapply(1:n_r_hat, \(i) mean(y[r == i], na.rm = TRUE), numeric(1))
)

bench::mark(
  check = FALSE,
  vapply(1:n_r_hat, FUN.VALUE = numeric(1), FUN = \(s) {
    cor(y[r == s][-1], lag(y[r == s], 1)[-1])
  }),
  lapply(1:n_r_hat, \(s) {
    cor(y[r == s][-1], lag(y[r == s], 1)[-1])
  })
)


bench::mark(
  check = FALSE,
  vapply(1:n_r_hat, FUN.VALUE = numeric(1), FUN = \(s) {
    abs(c(0, diff(r))) |> cumsum() |> _[r == s] |> table() |> mean()
  }),
  vapply(1:n_r_hat, FUN.VALUE = numeric(1), FUN = \(s) {
    mean(table(cumsum(abs(c(0, diff(r))))[r == s]))
  })
)
expression(abs(c(0, diff(r))) |> cumsum() |> _[r == s] |> table() |> mean())



bench::mark(
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
