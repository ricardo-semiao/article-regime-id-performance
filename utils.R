
# Setup ------------------------------------------------------------------------

library(cli)
library(glue)
library(patchwork)

library(tidyverse)
library(rlang)



# Theme ----------------------------------------------------------

pal <- list(
  main = c(
    orange = "#cc5500",
    green = "#007f5b",
    yellow = "#e5b300",
    blue = "#008c99",
    red = "#cc0022"
  ),
  dark = c(
    orange = "#7f3500",
    green = "#004c36",
    yellow = "#997700",
    blue = "#00464c",
    red = "#990019"
  ),
  light = c(
    orange = "#e08d51",
    green = "#22c395",
    yellow = "#f7d96d",
    blue = "#32bfcc",
    red = "#e05169"
  ),
  aqua = c(
    orange = "#f7d3ba",
    green = "#a5f2dc",
    yellow = "#f9ebb8",
    blue = "#a5ebf2",
    red = "#f5bcc5"
  ),
  grays = c(
    gray = "#f2f2f2",
    darkgray = "#cccccc",
    blackgray = "#666666",
    fontblack = "#22262a"
  )
)


theme_set(
  theme_bw() +
    theme(
      strip.background = element_rect(
        fill = pal$greys["darkgray"], color = "black"
      )
    )
)

arrow1 <- arrow(length = unit(0.02, "npc"), type = "closed")



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


#' Internal wrapper for ggsave with default units
#'
#' @param filename [`character(1)`] Output file path.
#' @param width [`numeric(1)`] Plot width.
#' @param height [`numeric(1)`] Plot height.
#' @param ... Additional arguments passed to [ggplot::ggsave()].
#' @param units [`character(1)`] Units for width and height.
#'
#' @return [`invisible(NULL)`].
ggsave2 <- function(filename, width, height, ..., units = "cm") {
  ggsave(filename, width = width, height = height, ..., units = units)
}



# Specific Helpers -------------------------------------------------------------

#' Apply with parallelism and/or safety
#'
#' @param x [`list()`-like] Input data to process.
#' @param f [`function()`] Function to apply to each element of `x`.
#' @param ... Additional arguments passed to `f`.
#' @param parallel [`logical(1)`] Whether to use parallel processing.
#' @param safely [`logical(1)`] Whether to wrap `f` with error handling.
#'
#' @return [`list()`] Results of applying `f` to `x`.
get_results <- function(x, f, ..., parallel, safely) {
  if (safely) f <- safely(f)

  if (parallel) {
    daemons(8)
    partial <- mirai_map(x, f, ...)
    results <- collect_mirai(partial, options = c(".progress")) #, ".flat"
    daemons(0)

    results <- map(results, \(x) {
      if (inherits_any(x, "try-error")) {
        list(result = NULL, error = x)
      } else {
        x
      }
    })
  } else {
    results <- lapply(x, f)
  }

  results
}
