
# Setup ------------------------------------------------------------------------

library(cli)
library(glue)
library(tidyverse)
library(rlang)



# General helpers --------------------------------------------------------------

#' Test Multiple Conditions and Abort on Failure
#'
#' @param ... [`logical(1)`] Conditions to test.
#' @param call [`environment`] Passed to [cli::cli_abort()].
#'
#' @value [`NULL`] Aborts if conditions are not met.
test_conditions <- function(..., call = caller_env()) {
  conditions <- c(...)

  if (!all(conditions)) {
    call <- caller_call()

    argnames <- fn_fmls_names(caller_fn())
    args <- as.list(call)[-1]
    names(args)[names(args) == ""] <- argnames[! argnames %in% names(args)]

    args_text <- as.list(call)[-1] %>%
      map2(
        ., `if`(is_null(names(.)), "", names(.)),
        ~ if (.y == "") expr_deparse(.x) else paste0(.y, " = ", expr_deparse(.x))
      ) |>
      str_c(collapse = ", ")

    failed <- set_names(names(conditions)[!conditions], "*")

    cli_abort(
      c("Bad arguments {.code ({args_text})}:", failed),
      call = call
    )
  }
}


#' Create a list with reference to previous items
#'
#' @param ... Arguments to collect in a list. These dots are dynamic.
#'
#' @return [`list()`] Named list with evaluated elements.
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



# Specific Helpers -------------------------------------------------------------

#' Create a regime function from lists of functions and arguments
#'
#' @param funs [`function` or `list(function)`] Functions to apply.
#' @param args [`list(list())`] List of argument lists for each function.
#'
#' @return [`function`] A new function combining the regimes.
create_regimes <- function(funs, args) {
  if (!is_list(args) || !all(map_lgl(args, is_list))) {
    cli_abort("{.arg args} must be a list of lists.)")
  }

  n_r <- length(args)

  if (is_function(funs)) {
    funs <- map(seq_len(n_r), ~ funs)
  } else if (!is_list(funs) || length(funs) != n_r) {
    cli_abort("
    {.arg funs} must be a function or a list with {.code length(args)} \\
    ({n_r}) functions.
    ")
  }

  ys_expr <- map2(funs, args, \(f, arg) inject(f(!!!arg)))

  new_function(
    args = pairlist2(y = , t = ),
    body = expr(c(!!!ys_expr) * r[t]),
    env = global_env()
  )
}
