
# Setup ----------------------------------------------------------

box::use(
  src/utils[...],
  stargazer[stargazer]
)


# Temporary example:
if (FALSE) {
  #...
}



# Regression Tables -----------------------------------------------------------

#' @export
format_reg_matrix <- function(
  mod, dim = c(3, 3), rows = 8:16, dimnames = NULL, ...
) {
  dimnames <- dimnames %||% list(c("SET", "ST", "MS"), c("SET", "ST", "MS"))
  coefs <- summary(mod)$coefficients[rows, ]

  glue("{round(coefs[, 1], 3)} ({round(coefs[, 2], 3)}){add_star(coefs[, 1])}") |>
    matrix(dim[1], dim[2]) |>
    `dimnames<-`(dimnames) |>
    stargazer(...) |>
    capture.output() |>
    str_replace_all(fixed("\\textasteriskcentered "), "*") |>
    cat(sep = "\n")
}

format_reg_table <- function(mods, ...) {
  stargazer(mods, ...)
}
