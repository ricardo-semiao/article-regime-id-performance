
# Setup ------------------------------------------------------------------------

source("utils.R")

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
  if (static_scheduling) {
    expr <- expr(parLapply(cl, x, f, ..., chunk.size = chunk_size))
  } else {
    expr <- expr(parLapplyLB(cl, x, f, ..., chunk.size = chunk_size))
  }

  cl <- makeCluster(ncl, type = type)
  results <- bench::mark(eval(expr), max_iterations = 100, min_iterations = 3, check = FALSE)
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
    max_iterations = 100,
    min_iterations = 3,
    check = FALSE
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
  results <- bench::mark(mirai = collect_mirai(mirai_map(x, f)), max_iterations = 100, min_iterations = 3, check = FALSE)
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
f2 <- \(ind) cumsum(errors[[ind]])

print_tests(
  test_parallel(x1, f1, "1par_psock_dyn", ncl, static_scheduling = FALSE),
  test_mirai(x1, f1, "1mirai_dyn", ncl, dispatcher = TRUE),
  test_future(x1, f1, "1fut_par_dyn", ncl, future.packages = "base"),
  test_future(x2, f2, "2fut_par_dyn", ncl, future.packages = "base"),
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
