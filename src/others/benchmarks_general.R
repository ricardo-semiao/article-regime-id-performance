
# Setup ------------------------------------------------------------------------

# General modules:
box::use(
  src/utils[...],
  src/parameters[...],
  bench[mark], syrup[syrup],
  ggplot2[...], patchwork[...]
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

# RNG:
set.seed(10126271)
filter_sim_i <- sample(n_i, 20)
filter_sim_i2 <- sample(n_i, 100)



# Models 3 ----------------------------------------------------------------

box::use(
  src/creators/models,
  mirai, mori
)

simulations <- read_rds("data/simulations.rds") |> map("result")
est_inputs <- map2(
  simulations,
  get_varying_param(names(simulations)),
  \(sim, rn_par) list(y = sim$y, rn_par = rn_par)
)

n_l_max <- 4
est_models <- list(
  nors = models$nors(n_r = 1),
  sb = models$sb(n_r = 2),
  ms = models$ms(n_r = 2),
  set= models$set(n_r = 2),
  st = models$st(n_r = 2),
  km = models$km(n_r = 2),
  rf = models$rf()
)


estimate_models <- function(input) {
  data <- data_lags(input$y, n_l = n_l_max)
  
  results <- vector("list", n_m) |> `names<-`(names(mods))
  for (mod_name in names(mods)) {
    results[[mod_name]] <- mods[[mod_name]](data, n_t, n_b, n_h, rn_par = input$rn_par)
  }
  
  results
}

estimate_models2 <- function(i_name) {
  input <- est_inputs[[i_name]]
  data <- data_lags(input$y, n_l = n_l_max)
  
  results <- vector("list", n_m) |> `names<-`(names(mods))
  for (mod_name in names(mods)) {
    results[[mod_name]] <- mods[[mod_name]](data, n_t, n_b, n_h, rn_par = input$rn_par)
  }
  
  results
}


items <- est_inputs %>% .[str_split_i(names(.), "-", 3) %in% filter_sim_i]
s1 <- syrup(interval = 0.1, {
  b1 <<- mark(min_iterations = 1, data = {
    data1 <<- map_parallel(
      items, estimate_models,
      parallel = TRUE, safe = TRUE, workers = 7,
      #setup_packages = c("tsDyn", "MSwM", "stats", "randomForest"),
      setup_data = list(
        mods = est_models, data_lags = data_lags,
        n_b = n_b, n_h = n_h, n_t = n_t, n_l_max = n_l_max, n_m = length(est_models)
      )
    )
  })
})

s2 <- syrup(interval = 0.1, {
  b2 <<- mark(min_iterations = 1, names = {
    data2 <<- map_parallel(
      set_names(names(items)), estimate_models2,
      parallel = TRUE, safe = TRUE, workers = 7,
      #setup_packages = c("tsDyn", "MSwM", "stats", "randomForest"),
      setup_data = list(
        mods = est_models, data_lags = data_lags, est_inputs = items,
        n_b = n_b, n_h = n_h, n_t = n_t, n_l_max = n_l_max, n_m = length(est_models)
      )
    )
  })
})


s2 |> print(n = Inf)
bind_rows(b1, b2)
data <- plot_syrup(data = s1, names = s2, groups = TRUE)$data

data |>
  group_by(obj) |>
  #filter(id >= 7) |>
  summarise(
    across(c(pct_cpu:vms), ~ paste0(
      mean(.x, na.rm = TRUE) |> round(2), " (",
      sd(.x, na.rm = TRUE) |> round(1), ")")
    )
  )


items <- est_inputs
s3 <- syrup(interval = 0.1, {
  b3 <<- mark(max_iterations = 1, names = {
    data3 <<- map_parallel(
      set_names(names(items))[1:840], estimate_models2,
      parallel = TRUE, safe = TRUE, workers = 7,
      #setup_packages = c("tsDyn", "MSwM", "stats", "randomForest"),
      setup_data = list(
        mods = est_models, data_lags = data_lags, est_inputs = items,
        n_b = n_b, n_h = n_h, n_t = n_t, n_l_max = n_l_max, n_m = length(est_models)
      )
    )
  })
})


items2 <- mori::share(est_inputs)
s4 <- syrup(interval = 0.1, {
  b4 <<- mark(max_iterations = 1, names_shared = {
    data4 <<- map_parallel(
      set_names(names(items2))[1:840], estimate_models2,
      parallel = TRUE, safe = TRUE, workers = 7,
      #setup_packages = c("tsDyn", "MSwM", "stats", "randomForest"),
      setup_data = list(
        mods = est_models, data_lags = data_lags, est_inputs = items2,
        n_b = n_b, n_h = n_h, n_t = n_t, n_l_max = n_l_max, n_m = length(est_models)
      )
    )
  })
})

s5 <- syrup(interval = 0.1, {
  b5 <<- mark(max_iterations = 1, data_shared = {
    data5 <<- map_parallel(
      items2, estimate_models,
      parallel = TRUE, safe = TRUE, workers = 7, cleanup = TRUE,
      #setup_packages = c("tsDyn", "MSwM", "stats", "randomForest"),
      setup_data = list(
        mods = est_models, data_lags = data_lags,
        n_b = n_b, n_h = n_h, n_t = n_t, n_l_max = n_l_max, n_m = length(est_models)
      )
    )
  })
})


s4 |> print(n = Inf)
bind_rows(b3, b4, b5)
data <- plot_syrup(names = s3, names_shared = s4, groups = TRUE)$data


items <- est_inputs %>% .[str_split_i(names(.), "-", 3) %in% sample(n_i, 20)]
s6 <- syrup(interval = 0.1, {
  b6 <<- mark(min_iterations = 1, data2 = {
    data6 <<- map_parallel3(
      items, estimate_models,
      parallel = TRUE, safe = TRUE, workers = 7,
      #setup_packages = c("tsDyn", "MSwM", "stats", "randomForest"),
      setup_data = list(
        mods = est_models, data_lags = data_lags,
        n_b = n_b, n_h = n_h, n_t = n_t, n_l_max = n_l_max, n_m = length(est_models)
      )
    )
  })
})
s7 <- syrup(interval = 0.1, {
  b7 <<- mark(min_iterations = 1, names2 = {
    data7 <<- map_parallel3(
      set_names(names(items)), estimate_models2,
      parallel = TRUE, safe = TRUE, workers = 7,
      #setup_packages = c("tsDyn", "MSwM", "stats", "randomForest"),
      setup_data = list(
        mods = est_models, data_lags = data_lags, est_inputs = items,
        n_b = n_b, n_h = n_h, n_t = n_t, n_l_max = n_l_max, n_m = length(est_models)
      )
    )
  })
})

s4 |> print(n = Inf)
bind_rows(b1, b6, b7, b8)
data <- plot_syrup(names = s7, manual = s8, groups = TRUE)$data


s8 <- syrup(interval = 0.1, {
  b8 <<- mark(min_iterations = 1, names2 = {
    data8 <<- map_parallel2(
      set_names(names(items)), estimate_models2,
      parallel = TRUE, safe = TRUE, workers = 7,
      #setup_packages = c("tsDyn", "MSwM", "stats", "randomForest"),
      setup_data = list(
        mods = est_models, data_lags = data_lags, est_inputs = items,
        n_b = n_b, n_h = n_h, n_t = n_t, n_l_max = n_l_max, n_m = length(est_models)
      )
    )
  })
})

f1 <- function(x) {
  #envs <- c(f_env = rlang::current_env(), rlang::env_parents()[])
  envs <- list(
    f_env = rlang::current_env(),
    global = rlang::env_parent()
  )
  lapply(envs, \(e) list(
    names = names(e) %||% character(0),
    size = lobstr::obj_size(e) %||% 0
  ))
}

items_s <- mori::share(items)

datas <- list(
  data11 = map_parallel(
    items[1], f1, setup_data = list(),
    parallel = TRUE, safe = FALSE, workers = 7,
  ),
  data12 = map_parallel(
    set_names(names(items))[1], f1, setup_data = list(items = items),
    parallel = TRUE, safe = FALSE, workers = 7,
  ),
  data12s = map_parallel(
    set_names(names(items))[1], f1, setup_data = list(items = items_s),
    parallel = TRUE, safe = FALSE, workers = 7,
  ),
  data22 = map_parallel2(
    set_names(names(items))[1], f1, setup_data = list(items = items),
    parallel = TRUE, safe = FALSE, workers = 7,
  ),
  data31 = map_parallel3(
    items[1], f1, setup_data = list(),
    parallel = TRUE, safe = FALSE, workers = 7,
  ),
  data32 = map_parallel3(
    set_names(names(items))[1], f1, setup_data = list(items = items),
    parallel = TRUE, safe = FALSE, workers = 7,
  ),
  data32s = map_parallel3(
    set_names(names(items))[1], f1, setup_data = list(items = items_s),
    parallel = TRUE, safe = FALSE, workers = 7,
  )
)

list_flatten(datas)

sumdata <- function(x) {
  list_transpose(x, simplify = FALSE) |>
    map(list_transpose) |>
    map(~ list(
      names = unique(.x$names),
      size_m = (mean(.x$size) / 1e+6) |> round(3),
      size_s = (sum(.x$size) / 1e+6) |> round(3)
    ))
}

sumd <- map(datas, sumdata) |> print()

sumd |>
  iwalk(\(x, opt) {
    cat(opt, ":\n", sep = "")
    iwalk(x, \(e, name) {
      cat(
        "- ", name, ": ",
        e$size_m, ", ", e$size_s, " (",
        paste0(e$names, collapse = ", "), ")\n",
        sep = ""
      )
    })
  })

datas$data22[1:2]


datas$data11[1:10] |>
  list_transpose() |>
  map(list_transpose) |>
  map(~ list(
    names = unique(.x$names),
    size_m = (mean(.x$size) / 1024) |> round(1),
    size_s = (sum(.x$size) / 1024) |> round(1)
  ))

datas$data12 |>
  list_transpose() |>
  _[[1]] |>
  names()

iwalk(datas, \(x, opt) {
  cat(opt, ":\n", sep = "")
  iwalk(x[[1]], \(e, name) {
    cat(
      "- ", name, ": ",
      format(lobstr::obj_size(e)), " (",
      paste0(names(e), collapse = ", "), ")\n",
      sep = ""
    )
  })
})


iwalk(datas, \(x, opt) {
  cat(opt, ":\n", sep = "")
  iwalk(x, \(e, name) {
    cat(
      "- ", name, ": ",
      map_dbl(e, lobstr::obj_size) |> mean() %>% {. / 1024}, " (",
      paste0(names(e[[1]]), collapse = ", "), ")\n",
      sep = ""
    )
  })
})

datas$data11[[1]] |> map()

plot_syrup <- function(..., wpid = ps::ps_pid(), groups = FALSE, plot = TRUE) {
  objs <- list2(...)
  
  cat("Current process ID:", wpid)
  
  cat("\n\nProcesses by name, parent ID, and ID (of first object):\n")
  with(objs[[1]], {
    tab <- table(ppid, pid, name)
    apply(tab, 3, \(x) x[rowSums(x) != 0, colSums(x) != 0, drop = FALSE]) |>
      print()
  })
  
  data <- imap_dfr(objs, ~ mutate(.x, obj = .y)) |>
    filter(ppid == wpid) |>
    mutate(
      id = as.integer(id),
      group = if (groups) {
        (diff(is.na(c(0, pct_cpu))) == 1) |>
          cumsum() %>%
          ifelse(. == 0, 1, .)
      } else {
        1
      },
      across(c(vms, rss), ~ as.double(.x) / 1000000)
    ) |>
    group_by(obj, group) |>
    filter(n() > 10) |> #sum(apply(pick(pct_cpu:vms), 1, \(x) sum(!is.na(x)))) > 10
    mutate(id2 = id - min(id) + 1)
    
  
  graph <- data |>
    pivot_longer(c(pct_cpu, vms, rss), names_to = "stat") |>
    ggplot(aes(id2, value, group = as.factor(pid))) +
    geom_line() +
    facet_grid(vars(stat), vars(obj, group), scales = "free_y") +
    scale_x_continuous(breaks = scales::breaks_pretty())
  
  if (plot) plot(graph)
  list(data = data, graph = graph)
}



map_parallel2 <- function(
    x, f,
    parallel, safe, workers = 7, cleanup = FALSE,
    setup_packages = NULL, setup_data = list(), setup_divide = NULL
) {
  if (inherits_any(x, "data.frame")) {
    cli_warn("{.code x} is a dataframe, {.code pmap}-like behavior may occour")
  }
  
  f_safe <- if (safe) safely_modify(f) else f
  
  if (parallel) {
    on.exit(mirai$daemons(0), add = TRUE)
    
    cat("Setting up infrastructure:\n")
    t1 <- Sys.time()
    
    n <- length(x)
    seqs <- round(seq(0, n, length.out = workers + 1))
    idxs <- sample(n, n)
    w_idxs <- lapply(seq_len(workers), \(w) idxs[(seqs[w] + 1):seqs[w + 1]])
    
    #fn_fmls(f_safe) <- c(fn_fmls(f_safe), setup_data[])
    setup_expr <- call2(`{`,
      !!!imap(setup_packages, ~ expr(library(!!.x, character.only = TRUE)))
    )
    setup_divide <- names(setup_data)[map_lgl(setup_data, ~ length(.x) == length(x))]
    for (w in seq_len(workers)) {
      mirai$daemons(1, cleanup = cleanup, dispatcher = FALSE, .compute = paste0("w", w))
      w_data <- imap(setup_data, \(data, name) {
        if (name %in% setup_divide) data[w_idxs[[w]]] else data
      })
      do.call(mirai$everywhere, c(.expr = setup_expr, w_data[], .compute = paste0("w", w)))
    }
    cat("- duration: ", round(Sys.time() - t1, 1), "s\n", sep = "")

    
    cat("Starting map:\n")
    t1 <- Sys.time()
    promises <- vector("list", workers)
    for (w in seq_len(workers)) {
      w_x <- x[w_idxs[[w]]]
      promises[[w]] <- lapply(w_x, \(elem){
        mirai$mirai(
          .expr = f(x),
          .args = list(f = f_safe, x = elem, .mirai_within_map = TRUE),
          .compute = paste0("w", w)
        )
      }) |>
        `names<-`(names(w_x))
    }
    promise <- list_flatten(promises) |> `class<-`("mirai_map")
    cat("- duration: ", round(Sys.time() - t1, 1), "s\n", sep = "")
    
    cat("Collecting map:\n")
    t1 <- Sys.time()
    results <- mirai$collect_mirai(promise, options = c(".progress"))
    
    results <- map(list_flatten(results), \(x) {
      if (inherits_any(x, "try-error")) list(result = NULL, error = x) else x
    }) # Connection resets happen before safely can catch them
    cat("- duration: ", round(Sys.time() - t1, 1), "s\n", sep = "")
  } else {
    fn_env(f_safe) <- new_environment(setup_data, fn_env(f_safe))
    results <- lapply(x, f_safe) # TODO: add profress
  }
  
  results
}


map_parallel3 <- function(
    x, f,
    parallel, safe, workers = 7, cleanup = FALSE,
    setup_packages = NULL, setup_data = list()
) {
  if (inherits_any(x, "data.frame")) {
    cli_warn("{.code x} is a dataframe, {.code pmap}-like behavior may occour")
  }
  
  f_safe <- if (safe) safely_modify(f) else f
  
  if (parallel) {
    on.exit(mirai$daemons(0), add = TRUE)
    
    cat("Setting up infrastructure:\n")
    t1 <- Sys.time()
    #fn_fmls(f_safe) <- c(fn_fmls(f_safe), setup_data[])
    setup_expr <- call2(`{`,
      !!!imap(setup_packages, ~ expr(library(!!.x, character.only = TRUE)))
    )
    mirai$daemons(workers, cleanup = cleanup)
    do.call(mirai$everywhere, c(.expr = setup_expr, setup_data[]))
    cat("- duration: ", round(Sys.time() - t1, 1), "s\n", sep = "")
    
    cat("Starting map:\n")
    t1 <- Sys.time()
    promise <- lapply(x, \(elem){
      mirai$mirai(
        .expr = f(x),
        .args = list(f = f_safe, x = elem, .mirai_within_map = TRUE)
      )
    }) |>
      `names<-`(names(x)) |>
      `class<-`("mirai_map")
    cat("- duration: ", round(Sys.time() - t1, 1), "s\n", sep = "")

    cat("Collecting map:\n")
    t1 <- Sys.time()
    results <- mirai$collect_mirai(promise, options = c(".progress"))
    
    results <- map(results, \(x) {
      if (inherits_any(x, "try-error")) list(result = NULL, error = x) else x
    }) # Connection resets happen before safely can catch them
    cat("- duration: ", round(Sys.time() - t1, 1), "s\n", sep = "")
  } else {
    fn_env(f_safe) <- new_environment(setup_data, fn_env(f_safe))
    results <- lapply(x, f_safe) # TODO: add profress
  }
  
  results
}



# models 2 ----------------------------------------------------------------

box::use(
  src/utils[...],
  src/creators/models,
  bench[mark],
  mirai
)

run_mirai <- function(
  x, f,
  parallel, safe, workers = 7, cleanup = FALSE,
  setup_packages = NULL, setup_data = list()
) {
  on.exit(mirai$daemons(0), add = TRUE)
  
  f_safe <- safely_modify(f)
  setup_expr <- call2(`{`,
    !!!imap(setup_packages, ~ expr(library(!!.x, character.only = TRUE)))
  )
  
  mirai$daemons(workers, cleanup = cleanup)
  do.call(mirai$everywhere, c(.expr = setup_expr, setup_data[]))
  
  promise <- mirai$mirai_map(x, f)
  results <- mirai$collect_mirai(promise, options = c(".progress"))
  
  results
}

estimate_models <- function(input) {
  data <- data_lags(data.frame(y = input$y), n_l = n_l_max)
  
  results <- vector("list", n_m)
  names(results) <- names(mods)
  
  for (mod_name in names(mods)) {
    results[[mod_name]] <- mods[[mod_name]](data, n_t, n_b, n_h, rn_par = input$rn_par)
  }
  
  results
}

est_models <- list(
  nors = models$nors(n_r = 1),
  sb = models$sb(n_r = 2),
  ms = models$ms(n_r = 2),
  set= models$set(n_r = 2),
  st = models$st(n_r = 2),
  km = models$km(n_r = 2),
  rf = models$rf()
)

simulations <- read_rds("data/simulations.rds") |> map("result")
est_inputs <- map2(
  simulations,
  get_varying_param(names(simulations)),
  \(sim, rn_par) list(y = sim$y, rn_par = rn_par)
)
est_inputs <- est_inputs %>% .[str_split_i(names(.), "-", 3) %in% 1]

data_lags(data.frame(y = est_inputs[[1]]$y), n_l = 4)

b1 <- mark(
  data = run_mirai(
    est_inputs, estimate_models,
    setup_packages = c("tsDyn", "MSwM", "stats", "randomForest"),
    setup_data = list(
      mods = est_models, data_lags = data_lags,
      n_b = n_b, n_h = n_h, n_t = n_t, n_l_max = 4, n_m = n_m
    )
  ),
  min_iterations = 2
)

saveRDS(r1)
b1


# Models -----------------------------------------------------------------------

box::use()
input <- read_rds("data/simulations.rds")[[1]]$result


profvis::profvis(run_model(models$st, n_r = 2))
run_model(models$nors, n_r = 1)

b1 <- mark(
  check = FALSE,

)
b1




# data_lags ---------------------------------------------------------------

y <- 1:100
n <- length(y)

mark(
  check = FALSE,
  c(list(y), lapply(1:4, \(x) double(n))) |> as.data.frame(),
  data.frame(y, lapply(1:4, \(x) double(n))),
  cbind(y, vapply(1:4, \(x) double(n), double(n))) |> as.data.frame()
)

mark(
  sapply(1:4, \(x) double(n)),
  vapply(1:4, \(x) double(n), double(n))
)


a <- function(y, n_l) {
  n <- length(y)
  ls <- seq_len(n_l)
  
  cbind(y, vapply(ls, \(l) lag(y, l), double(n))) |>
    as.data.frame() |>
    `colnames<-`(c("y", paste0("y_l", ls)))
}

b <- function(y, n_l) {
  res <- vector("list", n_l + 1) |>
    `names<-`(c("y", paste0("y_l", 1:n_l)))
  res$y <- y
  for (n in seq_len(n_l)) {
    res[[paste0("y_l", n)]] <- lag(y, n = n)
  }
  as.data.frame(res)
}

mark(
  check = FALSE,
  a(1:100, 4),
  b(1:100, 4),
  data_lags(data.frame(y = 1:100), 4)
)


mark(
  check = FALSE,
  {results <- vector("list", 4)
  names(results) <- letters[1:4]},
  {results <- vector("list", 4) |> `names<-`(letters[1:4])},
  {results <- vector("list", 4) |> setNames(letters[1:4])},
  results <- structure(vector("list", 4), names = letters[1:4])
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
