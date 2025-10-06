
# Setup ----------------------------------------------------------

box::purge_cache()
box::use(
  src/utils[...],
  src/options[sgps, rgps, models],
  src/diagnostics,
  src/others/metrics,
  rTRNG[rnorm_trng],
  bench[mark]
)

box::use(
  mirai[mirai_map, mirai_collect = collect_mirai, mirai_daemons = daemons]
)

# Debug:
#load("personal/workspace.RData")
if (FALSE) {
  save(
    list = grep("bench_", ls(), value = TRUE),
    file = "personal/workspace_benchmarks.RData"
  )
}



# Simulations: Parallel vs Sequential ------------------------------------------

simulate_serie1 <- function(input) {
  sfun <- input$sgp$fun
  rfun <- input$rgp$fun

  n_r <- input$rgp$n_r
  t_start <- input$sgp$t_cut + 1
  y <- input$errors

  r <- matrix(0, nrow = n_t, ncol = n_r)
  r_start <- eval(
    input$rgp$r_start,
    list(y = y, r = r, t_start = t_start, fun = rfun)
  )
  r[seq_len(t_start - 1), r_start] <- 1

  for (t in t_start:n_t) {
    r[t, ] <- rfun(y, r, t)
    y[t] <- sfun(y, r, t)
  }

  list(r = r, y = y)
}

sim_inputs1 <- pmap(sim_names, \(sgp, rgp, dgp, sim, dgp_sim) {
  list(
    sgp = options$sgps[[sgp]],
    rgp = options$rgps[[rgp]],
    errors = errors[, dgp_sim]
  )
}) |>
  set_names(sim_names$dgp_sim)

if (!exists("bench_ref_sim")) bench_seq_sim <- list()

bench_seq_sim$par <- mark(
  min_iterations = 2,
  par = {map_parallel(
    sim_inputs1, simulate_serie1,
    n_t = n_t,
    parallel = TRUE, safe = TRUE
  )}
)

bench_seq_sim$seq <- mark(
  min_iterations = 2,
  seq = {map_parallel(
    sim_inputs1, simulate_serie1,
    n_t = n_t,
    parallel = FALSE, safe = TRUE
  )}
)

bench_seq_sim$par_unsafe <- mark(
  min_iterations = 2,
  par_unsafe = {map_parallel(
    sim_inputs1, simulate_serie1,
    n_t = n_t,
    parallel = TRUE, safe = FALSE
  )}
)

bench_seq_sim$seq_unsafe <- mark(
  min_iterations = 2,
  seq_unsafe = {map_parallel(
    sim_inputs1, simulate_serie1,
    n_t = n_t,
    parallel = FALSE, safe = FALSE
  )}
)

bind_rows(bench_seq_sim)



# Simulations: Reference- vs Object-Passing ------------------------------------

simulate_serie2 <- function(input) {
  sfun <- sgps[[input$sgp]]$fun
  rfun <- rgps[[input$rgp]]$fun

  n_r <- rgps[[input$rgp]]$n_r
  t_start <- sgps[[input$sgp]]$t_cut + 1
  y <- errors[, input$errors]

  r <- matrix(0, nrow = n_t, ncol = n_r)
  r_start <- eval(
    rgps[[input$rgp]]$r_start,
    list(y = y, r = r, t_start = t_start, fun = rfun)
  )
  r[seq_len(t_start - 1), r_start] <- 1

  for (t in t_start:n_t) {
    r[t, ] <- rfun(y, r, t)
    y[t] <- sfun(y, r, t)
  }

  list(r = r, y = y)
}

simulate_serie3 <- function(rgp, sgp, error) {
  sfun <- sgp$fun
  rfun <- rgp$fun

  n_r <- rgp$n_r
  t_start <- sgp$t_cut + 1
  y <- error

  r <- matrix(0, nrow = n_t, ncol = n_r)
  r_start <- eval(
    rgp$r_start,
    list(y = y, r = r, t_start = t_start, fun = rfun)
  )
  r[seq_len(t_start - 1), r_start] <- 1

  for (t in t_start:n_t) {
    r[t, ] <- rfun(y, r, t)
    y[t] <- sfun(y, r, t)
  }

  list(r = r, y = y)
}

simulate_serie4 <- function(input) {
  sfun <- sgps[[input$sgp]]$fun
  rfun <- rgps[[input$rgp]]$fun

  n_r <- rgps[[input$rgp]]$n_r
  t_start <- sgps[[input$sgp]]$t_cut + 1
  y <- input$errors

  r <- matrix(0, nrow = n_t, ncol = n_r)
  r_start <- eval(
    rgps[[input$rgp]]$r_start,
    list(y = y, r = r, t_start = t_start, fun = rfun)
  )
  r[seq_len(t_start - 1), r_start] <- 1

  for (t in t_start:n_t) {
    r[t, ] <- rfun(y, r, t)
    y[t] <- sfun(y, r, t)
  }

  list(r = r, y = y)
}

sim_inputs2 <- pmap(sim_names, \(sgp, rgp, dgp, sim, dgp_sim) {
  list(
    sgp = sgp,
    rgp = rgp,
    errors = dgp_sim
  )
}) |>
  set_names(sim_names$dgp_sim)

sim_inputs3 <- pmap_dfr(sim_names, \(sgp, rgp, dgp, sim, dgp_sim) {
  list(
    sgp = list(options$sgps[[sgp]]),
    rgp = list(options$rgps[[rgp]]),
    error = list(errors[, dgp_sim])
  )
})

sim_inputs4 <- pmap(sim_names, \(sgp, rgp, dgp, sim, dgp_sim) {
  list(
    sgp = sgp,
    rgp = rgp,
    errors = errors[, dgp_sim]
  )
}) |>
  set_names(sim_names$dgp_sim)

if (!exists("bench_ref_sim")) bench_ref_sim <- list()

bench_ref_sim$obj <- mark(
  min_iterations = 2,
  obj = {map_parallel(
    sim_inputs1, simulate_serie1,
    n_t = n_t,
    parallel = TRUE, safe = FALSE
  )}
)

bench_ref_sim$ref <- mark(
  min_iterations = 2,
  ref = {map_parallel(
    sim_inputs2, simulate_serie2,
    n_t = n_t, sgps = options$sgps, rgps = options$rgps, errors = errors,
    parallel = TRUE, safe = FALSE
  )}
)

bench_ref_sim$obj_pmap <- mark(
  min_iterations = 2,
  obj_pmap = {map_parallel(
    sim_inputs3, simulate_serie3,
    n_t = n_t,
    parallel = TRUE, safe = FALSE
  )}
)

bench_ref_sim$hybrid <- mark(
  min_iterations = 2,
  hybrid = {map_parallel(
    sim_inputs4, simulate_serie4,
    n_t = n_t, sgps = options$sgps, rgps = options$rgps,
    parallel = TRUE, safe = FALSE
  )}
)

bind_rows(bench_ref_sim)



# Simulations: Static Obj Passing ----------------------------------------------

map_parallel2 <- function(x, f, ..., parallel, safe, workers = 6) {
  if (inherits_any(x, "data.frame")) {
    cli_warn("{.code x} is a dataframe, {.code pmap}-like behavior may occour")
  }

  #fn_env(f) <- new_environment(list2(...), pkg_env("base"))
  f_safe <- if (safe) safely_modify(f) else f

  if (parallel) {
    on.exit(mirai_daemons(0), add = TRUE)
    mirai_daemons(workers)

    promise <- mirai_map(x, ..., f_safe)
    results <- mirai_collect(promise, options = c(".progress"))

    results <- map(results, \(x) {
      if (inherits_any(x, "try-error")) list(result = NULL, error = x) else x
    }) # Connection resets happen before safely can catch them
  } else {
    cli_abort("Not implemented")
    results <- lapply(x, f_safe)
  }

  results
}

map_parallel3 <- function(x, f, ..., parallel, safe, workers = 6) {
  if (inherits_any(x, "data.frame")) {
    cli_warn("{.code x} is a dataframe, {.code pmap}-like behavior may occour")
  }

  #fn_env(f) <- new_environment(list2(...), pkg_env("base"))
  args <- list2(...)
  fn_fmls(f) <- c(fn_fmls(f), map(args, ~ expr()))
  f_safe <- if (safe) safely_modify(f) else f

  if (parallel) {
    on.exit(mirai_daemons(0), add = TRUE)
    mirai_daemons(workers)

    promise <- mirai_map(x, .args = args, f_safe)
    results <- mirai_collect(promise, options = c(".progress"))

    results <- map(results, \(x) {
      if (inherits_any(x, "try-error")) list(result = NULL, error = x) else x
    }) # Connection resets happen before safely can catch them
  } else {
    results <- lapply(x, f_safe, ...)
  }

  results
}

if (!exists("bench_obj_sim")) bench_obj_sim <- list()

bench_obj_sim$env <- bench_ref_sim$hybrid

bench_obj_sim$mirai_dots <- mark(
  min_iterations = 2,
  mirai_dots = {map_parallel2(
    sim_inputs4, simulate_serie4,
    n_t = n_t, sgps = options$sgps, rgps = options$rgps,
    parallel = TRUE, safe = FALSE
  )}
)

bench_obj_sim$new_args <- mark(
  min_iterations = 2,
  new_args = {map_parallel3(
    sim_inputs4, simulate_serie4,
    n_t = n_t, sgps = options$sgps, rgps = options$rgps,
    parallel = TRUE, safe = FALSE
  )}
)

bind_rows(bench_obj_sim)



# Simulations: Backends --------------------------------------------------------

map_parallel_parm <- function(x, f, ..., parallel, safe, workers = 6) {
  if (inherits_any(x, "data.frame")) {
    cli_warn("{.code x} is a dataframe, {.code pmap}-like behavior may occour")
  }
  
  fn_env(f) <- new_environment(list2(...), pkg_env("base"))
  f_safe <- if (safe) safely_modify(f) else f
  
  if (parallel) {
    on.exit(parallel::stopCluster(cl), add = TRUE)
    cl <- parallel::makeCluster(workers, type = "MIRAI")
    
    results <- parallel::parLapply(cl, x, f_safe)
    
    results <- map(results, \(x) {
      if (inherits_any(x, "try-error")) list(result = NULL, error = x) else x
    }) # Connection resets happen before safely can catch them
  } else {
    results <- lapply(x, f_safe)
  }
  
  results
}

map_parallel_parp <- function(x, f, ..., parallel, safe, workers = 6) {
  if (inherits_any(x, "data.frame")) {
    cli_warn("{.code x} is a dataframe, {.code pmap}-like behavior may occour")
  }
  
  fn_env(f) <- new_environment(list2(...), pkg_env("base"))
  f_safe <- if (safe) safely_modify(f) else f
  
  if (parallel) {
    on.exit(parallel::stopCluster(cl), add = TRUE)
    cl <- parallel::makeCluster(workers, type = "PSOCK")
    
    results <- parallel::parLapply(cl, x, f_safe)
    
    results <- map(results, \(x) {
      if (inherits_any(x, "try-error")) list(result = NULL, error = x) else x
    }) # Connection resets happen before safely can catch them
  } else {
    results <- lapply(x, f_safe)
  }
  
  results
}

if (!exists("bench_back_sim")) bench_back_sim <- list()

bench_back_sim$mirai <- bench_ref_sim$obj

bench_back_sim$parm <- mark(
  min_iterations = 2,
  parm = {map_parallel_parm(
    sim_inputs1, simulate_serie1,
    n_t = n_t,
    parallel = TRUE, safe = FALSE
  )}
)

bench_back_sim$parp <- mark(
  min_iterations = 2,
  parp = {map_parallel_parp(
    sim_inputs1, simulate_serie1,
    n_t = n_t,
    parallel = TRUE, safe = FALSE
  )}
)

bind_rows(bench_back_sim)



# Estimation: Reference- vs Object-Passing -------------------------------------

estimate_models1 <- function(y_name) {
  y <- simulations_ys[[y_name]]
  data <- data.frame(y = y, y_l1 = lag(y, 1L, default = NA))
  # Todo: generalize for n_p > 1

  results <- vector("list", n_m)
  names(results) <- names(models)

  for (mod_name in names(models)) {
    results[[mod_name]] <- models[[mod_name]](data, n_t, n_h)
  }

  results # Todo: transpose?
}

estimate_models2 <- function(y) {
  data <- data.frame(y = y, y_l1 = lag(y, 1L, default = NA))
  # Todo: generalize for n_p > 1

  results <- vector("list", n_m)
  names(results) <- names(models)

  for (mod_name in names(models)) {
    results[[mod_name]] <- models[[mod_name]](data, n_t, n_h)
  }

  results # Todo: transpose?
}

if (!exists("bench_ref_est")) bench_ref_est <- list()

bench_ref_est$ref <- mark(
  min_iterations = 2,
  ref = {map_parallel(
    set_names(names(simulations_ys))[1:200], estimate_models1,
    models = options$models[unique(model_names$model)],
    lag = lag, simulations_ys = simulations_ys,
    n_m = n_m, n_t = n_t, n_h = n_h,
    parallel = TRUE, safe = FALSE
  )}
)

bench_ref_est$obj <- mark(
  min_iterations = 2,
  obj = {map_parallel(
    simulations_ys[1:200], estimate_models2,
    models = options$models[unique(model_names$model)],
    lag = lag, simulations_ys = simulations_ys,
    n_m = n_m, n_t = n_t, n_h = n_h,
    parallel = TRUE, safe = FALSE
  )}
)

bind_rows(bench_ref_est)



# Estimations: Static Obj Passing ----------------------------------------------

if (!exists("bench_obj_est")) bench_obj_est <- list()

bench_obj_est$env <- bench_ref_est$obj

bench_obj_est$mirai_dots <- mark(
  min_iterations = 2,
  mirai_dots = {map_parallel2(
    simulations_ys[1:100], estimate_models2,
    models = options$models[unique(model_names$model)],
    lag = lag, simulations_ys = simulations_ys,
    n_m = n_m, n_t = n_t, n_h = n_h,
    parallel = TRUE, safe = FALSE
  )}
)

bench_obj_est$new_args <- mark(
  min_iterations = 2,
  new_args = {map_parallel3(
    simulations_ys[1:100], estimate_models2,
    models = options$models[unique(model_names$model)],
    lag = lag, simulations_ys = simulations_ys,
    n_m = n_m, n_t = n_t, n_h = n_h,
    parallel = TRUE, safe = FALSE
  )}
)

bind_rows(bench_obj_est)



# Estimation: Backends ---------------------------------------------------------

if (!exists("bench_back_est")) bench_back_est <- list()

bench_back_est$mirai <- bench_ref_est$obj

bench_back_est$parm <- mark(
  min_iterations = 2,
  parm = {map_parallel_parm(
    simulations_ys[1:200], estimate_models2,
    models = options$models[unique(model_names$model)],
    lag = lag, simulations_ys = simulations_ys,
    n_m = n_m, n_t = n_t, n_h = n_h,
    parallel = TRUE, safe = TRUE
  )}
)

bench_back_est$parp <- mark(
  min_iterations = 2,
  parp = {map_parallel_parp(
    simulations_ys[1:200], estimate_models2,
    models = options$models[unique(model_names$model)],
    lag = lag, simulations_ys = simulations_ys,
    n_m = n_m, n_t = n_t, n_h = n_h,
    parallel = TRUE, safe = TRUE
  )}
)

bind_rows(bench_back_est)
