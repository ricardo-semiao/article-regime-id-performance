
# Setup ------------------------------------------------------------------------

box::use(
  src/utils[...],
  src/options[dicts],
  stargazer[stargazer],
  gt[...],
  broom[glance]
)


# Temporary example:
if (FALSE) {
  mods = mod = lm(rmse ~ poly(sim, 9) - 1, sys_data)
  args = list(); out = "test.tex";
  dim = c(3, 3); rows = 1:9; dimnames = NULL; marginal = TRUE
}

dict_stats <- c(
  nobs = "Observations",
  r = "$R^2$,$~$ Adjusted $R^2$",
  test = "Resid. SE,$~$ F stat. p-value"
)



# Regression Shortcuts ---------------------------------------------------------

#' @export
lm_clumped <- function(
  f_rhs, data, models = NULL, clumps = c("all", "all"), ...
) {
  data <- data |>
    clump_dgps(clumps[1], clumps[2]) |>
    filter(! model %in% models)

  lm(new_formula(expr(rmse), enexpr(f_rhs)), data, ...)
}



# Regression Tables ------------------------------------------------------------

#' Results - systematic: Format regression matrix
#'
#' @param mod [`lm`] Linear model object.
#' @param out [`character(1)`] Output file path.
#' @param keep [`character(1)`] Pattern to keep coefficients.
#' @param parts [`character()`] Parts of the coefficients.
#' @param order [`integer()`] Order of parts.
#' @param marginal [`logical(1)`] Whether to compute marginal effects.
#' @param rows [`integer()`] Rows to add empty rows after.
#' @param ... Additional arguments passed to [gt::gtsave()].
#'
#' @returns [`character()`] Saves and returns a formatted regression matrix.
#' @export
format_reg_matrix <- function(
  mod, out,
  keep, parts, order = seq_along(parts),
  marginal = TRUE, rows = NULL, ...
) {
  coefs <- left_join(
    mod$coefficients %>% tibble(Coef = names(.), Estimate = .),
    summary(mod)$coefficients %>% {as_tibble(.) |> mutate(Coef = rownames(.))}
  )
  main <- coefs[grepl(keep, coefs$Coef), ] |>
    separate(Coef, parts, ":")

  if (marginal) {
    values <- main$Estimate
    sd <- glue(" ({round(main[['Std. Error']], 3)})")
  } else {
    controls <- mod$coefficients
    marginals <- apply(main[parts], 2, \(x) controls[x] %>% ifelse(is.na(.), 0, .))
    values <- main$Estimate + rowSums(marginals)
    sd <- ""
  }

  parts <- parts[order]

  table <- main |>
    mutate(
      fmt = glue("{round(values, 3)}{add_star(main[['Pr(>|t|)']])}{sd}")
    ) |>
    select(all_of(unname(parts)), fmt) |>
    filter(!is.na(values)) |>
    pivot_wider(names_from = unname(parts[1]), values_from = fmt) |>
    mutate(across(everything(), as.character)) |>
    add_emtpy_rows(rows) |>
    gt(rowname_col = parts[-1]) |>
    tab_stubhead(names(parts[-1])) |>
    tab_spanner(names(parts[1]), -parts[-1]) |>
    cols_label_with(fn = md) |>
    fmt_markdown(parts[-1]) |>
    add_footnote()

  gtsave2(table, out, ...)
  table <- readLines(out)

  ncol <- length(unique(main[[parts[1]]]))
  stats <- glance(mod) %>%
    {list(
      nobs = .$nobs,
      r = c(.$r.squared, .$adj.r.squared) |> round(3) |>
        paste(collapse = ",$~~~$"),
      test = c(round(.$sigma, 3), round(.$p.value, 3) %>% {if (. == "0") "<0.001" else .}) |>
        paste(collapse = ",$~~~$")
    )} |>
    _[names(dict_stats)] |>
    imap_chr(\(x, nm) {
      nm <- dict_stats[nm]
      glue("\\multicolumn{{2}}{{l|}}{{{nm}}} & \\multicolumn{{{ncol - 1}}}{{l}}{{{x}}} \\\\")
    }) |>
    c("\\midrule\\addlinespace[2.5pt]", .x = _) |>
    unname()

  table <- append(table, stats, grep("^\\\\bottomrule", table) - 1)
  table[grep("^\\\\toprule", table)] <- "\\toprule\\toprule"
  table[grep("^\\\\bottomrule", table)] <- "\\bottomrule\\bottomrule"

  writeLines(table, out)
  invisible(table)
}

#' Results - systematic: Format regression table
#'
#' @param mods [`list[lm]`] List of linear model objects.
#' @param out [`character(1)`] Output file path.
#' @param ... Additional arguments passed to [stargazer()].
#'
#' @returns [`character()`] Saves and returns a formatted regression table.
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
    .[names(.) != "(Intercept)"] |>
    na.omit() # ! Fix keep and omit arguments to stargazer

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
