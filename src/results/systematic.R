
# Setup ----------------------------------------------------------

box::use(
  src/utils[...],
  stargazer[stargazer],
  gt[...]
)


# Temporary example:
if (FALSE) {
  mod = lm(rmse ~ poly(sim, 9) - 1, sys_data)
  args = list()
  dim = c(3, 3); rows = 1:9; dimnames = NULL
}



# Regression Tables -----------------------------------------------------------

#' @export
format_reg_matrix <- function(
  mod, out, dim = c(3, 3), rows = 8:16, dimnames = NULL, ...
) {
  args <- list2(...)
  args$type <- args$type %||% "text"

  dict_local <- c("SET", "ST", "MS")

  dimnames <- dimnames %||% list(dict_local, dict_local)
  coefs <- summary(mod)$coefficients[rows, ]

  table <- glue("{round(coefs[, 1], 3)} ({round(coefs[, 2], 3)}){add_star(coefs[, 4])}") |>
    matrix(dim[1], dim[2]) |>
    `colnames<-`(dimnames[[2]]) |>
    as.data.frame() |>
    mutate(rgp = dimnames[[1]], .before = 1) |>
    gt(rowname_col = "rgp") |>
    tab_stubhead(md("RGP $\\diagdown$ Model")) |>
    tab_footnote(md("_Note:_  $^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01"))

  writeLines(table, out)
  cat("File written to", out, "\n")
  invisible(table)
}


#' @export
format_reg_table <- function(mods, out, ...) {
  lmp <- function(mod) {
    f <- summary(mod)$fstatistic
    p <- pf(f[1], f[2], f[3], lower.tail = FALSE)
    attributes(p) <- NULL
    round(p, 3) # Consider add_star()
    #format.pval(p, eps = 0.001, digits = 3)
  }

  if (class(mods) == "lm") {
    mods <- list(mods)
  }

  table <- capture.output(stargazer(
    mods, ...
    #add.lines = list(c("F Statistic", vapply(mods, lmp, character(1))))
  ))

  pat <- c(
    "^([^0-9]+)",
    rep(" & ([0-9]+\\.?[0-9+]*)", length(mods)),
    "( \\\\\\\\ )$"
  ) |>
    paste(collapse = "")

  f_pvals <- vapply(mods, lmp, double(1))
  f_idx <- grep("^F Statistic", table)

  if (length(f_idx) > 0) {
    f_old <- table[f_idx] |>
      str_match(pat) |>
      _[1, ]

    table[f_idx] <- paste0(
      f_old[2], " p-value",
      paste0(" & ", f_pvals),
      f_old[length(f_old)]
    )
  }


  writeLines(table, out)
  cat("File written to", out, "\n")
  invisible(table)
}
