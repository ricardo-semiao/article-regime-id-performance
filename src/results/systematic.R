
# Setup ----------------------------------------------------------

box::use(
  src/utils[...],
  src/options[dicts],
  stargazer[stargazer],
  gt[...]
)


# Temporary example:
if (FALSE) {
  mods = lm(rmse ~ poly(sim, 9) - 1, sys_data)
  args = list()
  dim = c(3, 3); rows = 1:9; dimnames = NULL
}



# Regression Tables -----------------------------------------------------------

#' @export
format_reg_matrix <- function(
  mod, out, dim = c(3, 3), rows = 8:16, dimnames = NULL, ...
) {
  dict_local <- c("SET", "ST", "MS")

  dimnames <- dimnames %||% list(Model = dict_local, RGP = dict_local)
  coefs <- summary(mod)$coefficients[rows, ]
  controls <- mod$coefficients

  parts <- str_split_fixed(rownames(coefs), ":", 2)
  coefs_full <- coefs[, 1] + controls[parts[, 1]] + controls[parts[, 2]]

  table <- glue("{round(coefs_full, 3)}{add_star(coefs[, 4])} ({round(coefs[, 2], 3)})") |>
    matrix(dim[1], dim[2]) |>
    `colnames<-`(dimnames[[2]]) |>
    as.data.frame() |>
    mutate(rgp = dimnames[[1]], .before = 1) |>
    gt(rowname_col = "rgp") |>
    tab_stubhead(names(dimnames)[1]) |>
    tab_footnote(md("_Note:_  $^{*}$p<0.1; $^{**}$p<0.05; $^{***}$p<0.01")) |>
    tab_spanner(names(dimnames)[2], dimnames[[2]])

  gtsave2(table, out, ...)
  invisible(table)
}


#' @export
format_reg_table <- function(mods, out, ...) {
  lmp <- function(mod) {
    f <- summary(mod)$fstatistic
    p <- pf(f[1], f[2], f[3], lower.tail = FALSE)
    attributes(p) <- NULL
    rp <- round(p, 3) # Consider add_star()
    ifelse(rp == 0, "<0.001", as.character(rp))
    #format.pval(p, eps = 0.001, digits = 3)
  }

  if (class(mods) == "lm") {
    mods <- list(mods)
  }

  labs <- reduce(mods, .init = character(0), ~ union(.x, names(.y$coefficients))) %>%
    dicts$reg[.] %>%
    .[names(.) != "(Intercept)"]

  label <- str_split_1(out, "/") %>% .[length(.)] %>% str_remove("\\.tex$")

  table <- capture.output(stargazer(
    mods, ..., header = FALSE, covariate.labels = labs,
    dep.var.caption = "", dep.var.labels = "RMSE", # TODO: genrealize
    label = label
    #add.lines = list(c("F Statistic", vapply(mods, lmp, character(1))))
  ))

  content_idx <- grep(r"(\\begin\{tabular\*?\})", table):(grep(r"(\\end\{table\})", table) - 1)
  table <- table[content_idx]

  pat <- c(
    "^([^0-9]+)",
    rep(" & ([0-9,]+\\.?[0-9+]*)(\\$\\^?\\{\\*{1,3}\\}\\$)?", length(mods)),
    "( \\\\\\\\ )$"
  ) |>
    paste(collapse = "")

  f_pvals <- vapply(mods, lmp, character(1))
  f_idx <- grep("^F Statistic", table)

  if (length(f_idx) > 0) {
    f_old <- table[f_idx] |>
      str_match(pat) |>
      _[1, ]

    table[f_idx] <- paste0(
      f_old[2], " p-value",
      paste0(" & ", f_pvals, collapse = ""),
      f_old[length(f_old)]
    )
  }

  writeLines(table, out)
  cli$cli_alert_success("File saved: {.file {out}}")
  invisible(table)
}
