
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
  mod, out, marginal = TRUE, dim = c(3, 3), rows = 8:16, dimnames = NULL, ...
) {
  dict_local <- c("MS", "SET", "ST")

  dimnames <- dimnames %||% list(Model = dict_local, RGP = dict_local)
  coefs <- summary(mod)$coefficients[rows, ]

  if (marginal) {
    values <- coefs[, 1]
    sd <- glue(" ({round(coefs[, 2], 3)})")
  } else {
    controls <- mod$coefficients
    parts <- str_split_fixed(rownames(coefs), ":", 2)
    values <- coefs[, 1] + controls[parts[, 1]] + controls[parts[, 2]]
    sd <- ""
  }

  table <- glue("{round(values, 3)}{add_star(coefs[, 4])}{sd}") |>
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
format_reg_matrix2 <- function(
  mod, out, marginal = TRUE, dim = c(12, 3), rows = 16:51, dimnames = NULL, ...
) {
  ds <- dicts$sgps$gt %>% set_names(str_replace(names(.), "[0-9]+$", ""))
  dr <- dicts$rgps$gt %>% set_names(str_replace(names(.), "_a?symm_.+", ""))

  controls <- mod$coefficients
  coefs <- left_join(
    mod$coefficients %>% tibble(Coef = names(.), Estimate = .),
    summary(mod)$coefficients %>% {as_tibble(.) |> mutate(Coef = rownames(.))}
  )
  main <- coefs[rows, ]

  ints <- main$Coef %>% str_split_fixed(":", max(str_count(., ":") + 1))

  if (marginal) {
    values <- main$Estimate
    sd <- glue(" ({round(main[['Std. Error']], 3)})")
  } else {
    parts <- apply(ints, 2, \(x) controls[x] %>% ifelse(is.na(.), 0, .))
    values <- main$Estimate + rowSums(parts)
    sd <- ""
  }

  table <- main |>
    transmute(
      fmt = glue("{round(values, 3)}{add_star(main[['Pr(>|t|)']])}{sd}"),
      sgp = ints[, 1], rgp = ints[, 2], model = ints[, 3],
      sgp = ds[str_replace(sgp, "^sgp", "")],
      rgp = dr[str_replace(rgp, "^rgp", "")],
      model = dicts$models$gt[str_replace(model, "^model", "")]
    ) |>
    filter(!is.na(values)) |>
    pivot_wider(names_from = model, values_from = fmt) |>
    relocate(rgp, sgp) |>
    arrange(rgp, sgp) |>
    mutate(across(everything(), as.character)) |>
    reduce(c(3, 7, 11), .init = _, ~ add_row(.x, .after = .y)) |>
    mutate(across(everything(), ~ ifelse(is.na(.x), "", .x))) |>
    gt(rowname_col = c("rgp", "sgp")) |>
    tab_stubhead(c("RGP", "SGP")) |>
    tab_spanner("Model", c("SET", "ST", "MS")) |>
    cols_label_with(fn = md) |>
    fmt_markdown(c("rgp", "sgp")) |>
    tab_style(
      style = cell_borders(sides = "bottom", color = "black", weight = px(1)),
      locations = list(
        cells_stub(rows = seq(3, 9, 3), "rgp"),
        cells_stub(rows = seq(3, 9, 3), "sgp"),
        cells_body(rows = seq(3, 9, 3))
      )
    )

  gtsave2(table, out, ...)
  invisible(table)
}

#' @export
format_reg_matrix3 <- function(
  mod, out, marginal = TRUE, dim = c(24, 3), rows = 30:53, dimnames = NULL, ...
) {
  ds <- dicts$sgps$gt %>% set_names(str_replace(names(.), "[0-9]+$", ""))

  controls <- mod$coefficients
  coefs <- left_join(
    mod$coefficients %>% tibble(Coef = names(.), Estimate = .),
    summary(mod)$coefficients %>% {as_tibble(.) |> mutate(Coef = rownames(.))}
  )
  main <- coefs[rows, ]

  ints <- main$Coef %>% str_split_fixed(":", max(str_count(., ":") + 1))

  if (marginal) {
    values <- main$Estimate
    sd <- glue(" ({round(main[['Std. Error']], 3)})")
  } else {
    parts <- apply(ints, 2, \(x) controls[x] %>% ifelse(is.na(.), 0, .))
    values <- main$Estimate + rowSums(parts)
    sd <- ""
  }

  table <- main |>
    transmute(
      fmt = glue("{round(values, 3)}{add_star(main[['Pr(>|t|)']])}{sd}"),
      sgp = ints[, 3], metric = ints[, 2], model = ints[, 1],
      sgp = ds[str_replace(sgp, "^sgp", "")],
      metric = dicts$metrics$disp_gt[str_replace(metric, "_est", "")],
      model = dicts$models$gt[str_replace(model, "^model", "")]
    ) |>
    filter(!is.na(values)) |>
    pivot_wider(names_from = model, values_from = fmt) |>
    relocate(metric, sgp) |>
    arrange(metric, sgp) |>
    mutate(across(everything(), as.character)) |>
    reduce(c(2, 5), .init = _, ~ add_row(.x, .after = .y)) |>
    mutate(across(everything(), ~ ifelse(is.na(.x), "", .x))) |>
    gt(rowname_col = c("metric", "sgp")) |>
    tab_stubhead(c("Metric", "SGP")) |>
    tab_spanner("Model", c("SET", "ST", "MS")) |>
    cols_label_with(fn = md) |>
    fmt_markdown(c("metric", "sgp")) |>
    tab_style(
      style = cell_borders(sides = "bottom", color = "black", weight = px(2)),
      locations = list(
        cells_stub(rows = seq(2, 4, 2), "metric"),
        cells_stub(rows = seq(2, 4, 2), "sgp"),
        cells_body(rows = seq(2, 4, 2))
      )
    )

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
