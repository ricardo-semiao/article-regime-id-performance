
# Setup ------------------------------------------------------------------------

# Packages to be available across all modules

#' General
#' @export
box::use(
  r/core[...],
  cli[...],
  glue[...],
  patchwork[...]
)

#' Tidyverse
#' @export
box::use(
  ggplot2[...],
  dplyr[...],
  tidyr[...],
  readr[...],
  purrr[...],
  tibble[...],
  stringr[...],
  forcats[...],
  rlang[...]
)

#' Parallel processing
box::use(
  mirai[mirai_map, mirai_collect = collect_mirai, mirai_daemons = daemons]
)



# Theme ----------------------------------------------------------

#' Color palette
#' @export
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


# Ggplot theme
theme_set(
  theme_bw() +
    theme(
      strip.background = element_rect(
        fill = pal$greys["darkgray"], color = "black"
      )
    )
)


#' Arrow for plotting
#' @export
arrow1 <- arrow(length = unit(0.02, "npc"), type = "closed")



# General helpers --------------------------------------------------------------

#' Test Multiple Conditions and Abort on Failure
#'
#' @param ... [`logical(1)`] Conditions to test.
#' @param call [`environment`] Passed to [cli::cli_abort()].
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


#' Internal wrapper for ggsave with default units
#'
#' @param filename [`character(1)`] Output file path.
#' @param width [`numeric(1)`] Plot width.
#' @param height [`numeric(1)`] Plot height.
#' @param ... Additional arguments passed to [ggplot::ggsave()].
#' @param units [`character(1)`] Units for width and height.
#'
#' @returns [`invisible(NULL)`].
#' @export
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
#' @returns [`list()`] Results of applying `f` to `x`.
#' @export
get_results <- function(x, f, ..., parallel, safely) {
  fn_env(f) <- new_environment(list2(...), pkg_env("base"))
  if (safely) f <- safely(f)

  if (parallel) {
    on.exit(mirai_daemons(0), add = TRUE)

    mirai_daemons(4)
    results <- mirai_collect(mirai_map(x, f, ...), options = c(".progress")) #, ".flat"

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


#' Lag:
#' @export
lag <- function(x, n = 1L, default = NA) {
  c(rep(default, n), x[-(length(x) - seq_len(n) + 1)])
}
