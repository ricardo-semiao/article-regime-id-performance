
# Setup ------------------------------------------------------------------------

# Modules to be available in all scripts:

#' Modules: General
#' @export
box::use(
  r/core[...],
  cli = cli[cli_abort, cli_warn, cli_inform],
  glue[glue],
  ggplot2[last_plot]
)

#' Modules: Tidyverse
#' @export
box::use(
  dplyr[...], tidyr[...],
  tibble[tibble], readr[write_rds, read_rds],
  stringr[...], forcats[...],
  purrr[...], rlang[...]
)

# Modules for helpers:
box::use(
  mirai,
  ggplot2[...],
  gt[...]
)



# Theme ------------------------------------------------------------------------

#' Theme: Color palette
#' @export
pal <- list(
  main = c(
    orange = "#cc5500", green = "#007f5b", yellow = "#e5b300", blue = "#008c99", red = "#cc0022"
  ),
  dark = c(
    orange = "#7f3500", green = "#004c36", yellow = "#997700", blue = "#00464c", red = "#990019"
  ),
  light = c(
    orange = "#e08d51", green = "#22c395", yellow = "#f7d96d", blue = "#32bfcc", red = "#e05169"
  ),
  aqua = c(
    orange = "#f7d3ba", green = "#a5f2dc", yellow = "#f9ebb8", blue = "#a5ebf2", red = "#f5bcc5"
  ),
  grays = c(
    gray = "#f2f2f2", darkgray = "#cccccc", blackgray = "#666666", fontblack = "#22262a"
  )
)

# Theme: ggplot theme
theme_set(
  theme_bw() +
    theme(
      strip.background = element_rect(
        fill = pal$greys["darkgray"], color = "black"
      )
    )
)



# Infrastructure Helpers -------------------------------------------------------

#' Helper: Abort via multiple conditions
#'
#' @param ... [`logical(1)`] Conditions to test.
#' @param call [`environment()`] Passed to [cli::cli_abort()].
#'
#' @returns  [`NULL`] Aborts if conditions are not met.
#' @export
test_conditions <- function(..., call = caller_env()) {
  conditions <- c(...)

  if (!all(conditions)) {
    call <- caller_call()

    argnames <- fn_fmls_names(caller_fn())
    args <- as.list(call)[-1]
    names(args)[names(args) == ""] <- argnames[! argnames %in% names(args)]

    args_text <- as.list(call)[-1] %>%
      map2(., `if`(is_null(names(.)), "", names(.)), \(x, y) {
        if (y == "") expr_deparse(x) else paste0(y, " = ", expr_deparse(x))
      }) |>
      str_c(collapse = ", ")

    failed <- set_names(names(conditions)[!conditions], "*")

    cli_abort(
      c("Bad arguments {.code ({args_text})}:", failed),
      call = call
    )
  }
}

#' Helper: Custom [cli::cli_alert()]
#' @export
cli_alert_items <- function(failed_items, out = NULL, flatten = FALSE) {
  if (flatten) failed_items <- list_flatten(failed_items)

  if (length(failed_items) == 0) {
    cli$cli_alert_success("No errors found.")
    return(invisible(NULL))
  }

  items_unique <- unique(failed_items)
  items_per_error <- list()

  cli$cli_alert_danger("There were {.val {length(items_unique)}} errors across \\
  {.val {length(failed_items)}} item{?s}.")

  cli$cli_rule()
  cli$cli_h3("Errors:")

  iwalk(unique(failed_items), \(error, i) {
    is_of_error <- map_lgl(failed_items, ~ identical(.x, error))
    items_per_error[[error$message]] <<- names(failed_items)[is_of_error]

    cli$cli_li("Error {.val {i}}:")
    cli$cli_text("Occurances: {.val {sum(is_of_error)}}. On item{?s}: \\
    {.val {names(failed_items[is_of_error])}}.")
    print(error)
    cli$cli_par()
  })

  if (! is_null(out)) {
    imap_chr(items_per_error, \(items, error) {
      paste0(
        "Error: ", error, "\n",
        "- Occurences: ", length(items), "\n",
        "- Items: ", str_c(items, collapse = ", ")
      )
    }) |>
      str_c(collapse = "\n\n") |>
      str_c("Total: ", length(failed_items), "\n\n", .x = _) |>
      writeLines(out)
  }

  invisible(items_per_error)
}

#' Helper: list2 with tibble-like self referencing
#'
#' @param ... Arguments to collect in a list. These dots are dynamic.
#'
#' @returns [`list()`] Named list with evaluated elements.
#' @export
list3 <- function(...) {
  args <- enquos(...)

  no_name <- names(args) == ""
  names(args)[no_name] <- glue("__{seq_along(args)}__")[no_name]

  result <- structure(
    vector("list", length(args)),
    names = names(args)
  )

  for (name in names(args)) {
    result[[name]] <- eval_tidy(args[[name]], data = result)
  }

  names(result)[no_name] <- ""
  result
}

#' Helper: ggsave wrapper with default units
#'
#' @param filename [`character(1)`] Output file path.
#' @param width, height [`numeric(1)`] Plot dimensions.
#' @param ratio [`numeric(1)`] Aspect ratio (height/width).
#' @param ... Additional arguments passed to [ggplot::ggsave()].
#'
#' @returns [`invisible(NULL)`].
#' @export
ggsave2 <- function(filename, width = NA, ratio = 1, scale = 1, ...) {
  env <- caller_env()
  ggsave(
    glue(filename, .envir = env),
    width = width, height = width * ratio, scale = scale, ...
  )
  cli$cli_alert_success("File saved: {.file {filename}}")
}

#' @export
gtsave2 <- function(data, filename, ...) {
  args <- list2(...)
  args[["latex.tbl.pos"]] <- args[["latex.tbl.pos"]] %||% "!htbp"

  data %>%
    {inject(tab_options(., !!!args))} |>
    gtsave(filename)

  table <- readLines(filename)

  content_idx <- grep(r"(\\begin\{tabular\*?\})", table):(grep(r"(\\end\{table\})", table) - 1)
  table <- table[content_idx]

  #table[table == "\\fontsize{12.0pt}{14.0pt}\\selectfont"] <- "\\centering"
  table[table == "\\begin{minipage}{\\linewidth}"] <- "\\begin{minipage}{\\linewidth}\\centering"

  pat <- "(\\\\begin\\{tabular\\*\\})(\\{\\\\linewidth\\})(.+)"
  table[grepl(pat, table)] <- table[grepl(pat, table)] |>
    str_match(pat) %>%
    {paste0("\\begin{tabular}", .[1, 4])}
  table[table == "\\end{tabular*}"] <- "\\end{tabular}"

  #label <- str_split_1(filename, "/") %>% .[length(.)] %>% str_remove("\\.tex$")
  #table <- append(table, glue("\\label{{tbl-{label}}}"), after = 1)

  writeLines(table, filename)
  cli$cli_alert_success("File saved: {.file {filename}}")
}



#' Helper: write_rds wrapper with success message
#'
#' @param x An R object to be saved to file.
#' @param file [`character(1)`] Path to the file where the object will be
#'   saved.
#' @param ... Additional arguments passed to [readr::write_rds()].
#'
#' @returns [`invisible(NULL)`].
#' @export
write_rds2 <- function(x, file, ...) {
  write_rds(x, file, ...)
  cli$cli_alert_success("File saved: {.file {file}}")
}

#' @export
print_summary <- function(x, ...) {
  print(summary(x, ...))
  invisible(x)
}



# Parallel Execution -----------------------------------------------------------

#' Helper: updates a function body to be safely
#'
#' Useful to avoid loading purrr in parallel processing.
#'
#' @param .f [`function(){}`] Function to modify.
#'
#' @returns [`function(){}`] The same function with a tryCatch'ed body.
#' @export
safely_modify <- function(.f) {
  fn_body(.f) <- expr({
    tryCatch(
      expr = list(result = {!!!fn_body(.f)}, error = NULL),
      error = \(e) list(result = NULL, error = e)
    )
  })

  .f
}

#' Helper: Map with parallelism and/or safety
#'
#' `...` is passed to `f`'s environment, as mirai respects it.
#'
#' @param x [`list()`-like] Input data to process.
#' @param f [`function()`] Function to apply to each element of `x`.
#' @param ... Additional arguments passed to `f`.
#' @param parallel, safe [`logical(1)`] Whether to use parallel processing
#'  and/or `safely()`.
#'
#' @returns [`list()`] Results of applying `f` to `x`.
#' @export
map_parallel <- function(
  x, f, ...,
  parallel, safe, workers = 7, cleanup = FALSE,
  setup_packages = NULL, setup_data = list()
) {
  if (inherits_any(x, "data.frame")) {
    cli_warn("{.code x} is a dataframe, {.code pmap}-like behavior may occour")
  }

  f_safe <- if (safe) safely_modify(f) else f

  if (parallel) {
    on.exit(mirai$daemons(0), add = TRUE)

    setup_expr <- call2(`{`,
      !!!imap(setup_packages, ~ expr(library(!!.x, character.only = TRUE)))
    )

    mirai$daemons(workers, cleanup = cleanup) # * No worker cleanup between tasks
    do.call(mirai$everywhere, c(.expr = setup_expr, setup_data[]))

    promise <- mirai$mirai_map(x, f_safe)
    results <- mirai$collect_mirai(promise, options = c(".progress"))

    results <- map(results, \(x) {
      if (inherits_any(x, "try-error")) list(result = NULL, error = x) else x
    }) # Connection resets happen before safely can catch them
  } else {
    fn_env(f_safe) <- new_environment(setup_data, fn_env(f_safe))
    results <- lapply(x, f_safe) # TODO: add profress
  }

  results
}



# Calculations Helpers ---------------------------------------------------------

#' Helper: Compute lagged values
#'
#' @param x [`vector()`] Input vector.
#'
#' @export
lag <- function(x, n = 1L, default = NA) {
  c(rep(default, n), x[-(length(x) - seq_len(n) + 1)])
}
fn_env(lag) <- pkg_env("base")

#' Helper: Add lagged values to a data frame
#' @export
data_lags <- function(data, n_l = 1) {
  data$y_l1 <- lag(data$y)

  for (n in seq_len(n_l - 1)) {
    data[[paste0("y_l", n + 1)]] <- lag(data[[paste0("y_l", n)]])
  }

  data
}
fn_env(data_lags) <- new_environment(list(lag = lag), pkg_env("base"))
# TODO: couldnt it just use lag(., n)?

#' @export
get_varying_param <- function(dgp_names) {
  dgp_names |>
    str_split_i("-", 1) |>
    gsub(".+_([a-z]+)[0-9]+", "\\1", x = _) %>%
    {if_else(. == "rho", "rho1", .)} # sgp names have 'rho' refering to 'rho1'
}

#' @export
regimes_order <- function(coefs, rn_par, dims) {
  order(coefs[, which(rn_par == dims$cols)], decreasing = FALSE)
}
fn_env(regimes_order) <- pkg_env("base")

# Bare versions of stats functions. Assumes numerical vectors of same size and
# na.rm = TRUE.
#' @export
bare_cov <- function(x, y, ...) {
  not_na <- !is.na(x) & !is.na(y)
  yna <- y[not_na]
  xna <- x[not_na]
  n <- length(xna)

  sum((xna - sum(xna) / n) * (yna - sum(yna) / n)) / (n - 1)
}
fn_env(bare_cov) <- pkg_env("base")

#' @export
bare_cor <- function(x, y, ...) {
  not_na <- !is.na(x) & !is.na(y)
  yna <- y[not_na]
  xna <- x[not_na]
  n <- length(yna)

  mx <- sum(xna) / n
  my <- sum(yna) / n

  sum((xna - mx) * (yna - my)) / sqrt(sum((xna - mx)^2) * sum((yna - my)^2))
}
fn_env(bare_cor) <- pkg_env("base")

#' @export
bare_sd <- function(x, na.rm = FALSE, ...) {
  if (na.rm) x <- x[!is.na(x)]
  n <- length(x)

  sqrt(sum((x - sum(x) / n)^2) / (n - 1))
}

#' @export
acor <- function(y, p = 1, na.rm = FALSE) {
  idx <- which(is.na(y))

  if (na.rm && length(idx) > 0) {
    y <- y[-c(idx, idx - p)]
  }

  n <- length(y)
  if (n - p <= 1) return(NA_real_)

  y_bar <- mean(y)
  sum((y[(p + 1):n] - y_bar) * (y[1:(n - p)] - y_bar)) / sum((y - y_bar)^2)
}
fn_env(acor) <- pkg_env("base")

#' @export
clump_dgps <- function(sys_data, keep_rgp = "fam", keep_sgp = "fam") {
  pats_rgp <- c(
    fam = "^(r[0-9]+_[^_]+)_*.*$",
    var = "^.*_(a?symm)_?.*$",
    all = "(.+)"
  )
  pats_sgp <- c(
    fam = "^(r[0-9]+_[^_]+_[^_0-9]+)[0-9]+$",
    var = "^r[0-9]+_[^_]+_[^_0-9]+([0-9]+)$",
    all = "(.+)"
  )

  mutate(sys_data,
    rgp = str_replace(rgp, pats_rgp[keep_rgp], "\\1") |> fct(),
    sgp = str_replace(sgp, pats_sgp[keep_sgp], "\\1") |> fct()
  )
}

#' @export
glue_t_test <- function(x, h0, n = 2, test = TRUE) {
  m <- mean(x, na.rm = TRUE)
  s <- sd(x, na.rm = TRUE)
  ndf <- sum(!is.na(x))

  stars <- if (test) {
    # t <- sqrt(ndf) * (m - h0) / s
    # p <- 2 * pt(-abs(t), df = ndf - 1)
    add_star(t.test(x, mu = h0, conf.level = 0.95)$p.value)
  } else {
    ""
  }

  glue("{round(m, n)}{stars} ({round(s / sqrt(ndf), n + 1)})")
}



# Formatting Helpers -----------------------------------------------------------

#' Helper: Add significance stars to p-values
#' @export
add_star <- function(x, escape = FALSE) {
  levels <- c("***", "**", "*", "")
  if (escape) levels <- str_replace_all(levels, "\\*", "\\\\*")
  cut(x, c(-Inf, 0.01, 0.05, 0.1, Inf), levels) |> as.character()
}

#' @export
reduce_spanners <- function(table, cols, dict) {
  reduce(names(cols), .init = table, \(table, label) {
    tab_spanner(table, label = md(dict[label]), columns = cols[[label]])
  })
}

#' @export
fmt_decimal <- function(x, n = 2, lead_0 = FALSE, trail_0 = FALSE) {
  x <- round(x, n) |> as.character()

  if (!lead_0) x <- gsub("^0\\.", ".", x)
  if (trail_0) x <- ifelse((nchar(x) == n + 1) & grepl("\\.", x), x, paste0(x, "0"))

  x
}

add_footnote <- function(table, cuts = c(0.1, 0.05, 0.01)) {
  text <- map2_chr(cuts, seq_along(cuts), ~ glue("$^{{{strrep('*', .y)}}}$p<{.x}")) |>
    str_c(collapse = "; ") |>
    str_c("_Note:_ ", .x = _)
  tab_footnote(table, md(text))
}

#' @export
add_emtpy_rows <- function(data, rows = NULL) {
  data |>
    reduce(rows, .init = _, ~ add_row(.x, .after = .y)) |>
    mutate(across(everything(), ~ ifelse(is.na(.x), "", .x)))
}
