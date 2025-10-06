
# Setup ------------------------------------------------------------------------

# Loading dependencies:
box::use(
  ./utils_diagnostics[...],
  ./simulations,
  src/options[dicts],
  scales[col_darker],
)

if (Sys.getenv("RADIAN_VERSION") == "") box::use(gt[...])
# Radian has unicode problems that break gt loading. Run functions that require
# it in RStudio instead


# Temporary example:
if (FALSE) {
  sims = 1
  n_burn = 10; n_h = 10; n_t = 100 + n_burn + n_h
  multiple = TRUE; hline = 0; title = NULL; faceted = NULL
}



# Helpers ----------------------------------------------------------------------

#' Internal: Get error data by joining simulation and estimation results
get_error_data <- function(data_s, data_e, regime_aligned) {
  data_s <- filter(data_s,
    data_s$sgp %in% unique(data_e$sgp) &
      data_s$rgp %in% unique(data_e$rgp) &
      data_s$sim %in% unique(data_e$sim)
  )

  left_join(
    rename(data_s, y_true = y, r_true = r),
    rename(data_e, y_est = y, r_est = r),
    by = c("sgp", "rgp", "sim", "t")
  ) |>
    mutate(
      y = y_est - y_true,
      r = if (regime_aligned) {
        r_est
      } else {
        c("Correct", "Incorrect")[1 + (r_est != r_true)]
      }
    )
}



# Values -----------------------------------------------------------------------

#' Todo: document
#' @export
series_values <- function(
  data_s, data_e,
  sims = 1, n_burn = NA, n_t = length(unique(data_e$t)), n_h = 0,
  multiple = TRUE, hline = NULL, title = NULL, faceted = NULL
) {
  args <- list(
    sims = sims, n_burn = n_burn, multiple = multiple, hline = hline,
    title = title, faceted = faceted
  )

  g_s <- inject(simulations$series_values(data_s, !!!args))
  g_e <- inject(simulations$series_values(data_e, !!!args))

  (g_s - g_e) +
    plot_layout(ncol = 1, guides = "collect", axes = "collect") &
    list( 
      range(data_s$y, data_e$y, na.rm = TRUE) %>% {ylim(.[1], .[2])},
      annotate("rect",
        xmin = n_t - n_h + 0.5, xmax = n_t + 0.5, ymin = -Inf, ymax = Inf,
        fill = "grey", alpha = 0.2
      )
    )
}

#' Todo: document
#' @export
series_distribution <- function(
  data_s, data_e,
  n_burn = NA, hline = NULL, title = NULL, faceted = NULL
) {
  args <- list(
    n_burn = n_burn, hline = hline, title = title, faceted = faceted
  )

  g_s <- inject(simulations$series_distribution(data_s, !!!args))
  g_e <- inject(simulations$series_distribution(data_e, !!!args))

  (g_s - g_e) +
    plot_layout(ncol = 1, guides = "collect", axes = "collect_y")
}

#' Todo: document
#' @export
panel_estimations <- function(
  data_s, data_e,
  n_burn = NA, n_t = length(unique(data_e$t)), n_h = 0,
  title = NULL
) {
  g_values <- series_values(
    data_s, data_e,
    sims = 1, n_burn = n_burn, n_t = n_t, n_h = n_h
  ) &
    theme(legend.position = "none")
  g_distribution <- series_distribution(
    data_s, data_e,
    n_burn = n_burn
  ) &
    geom_line(aes(NA_real_, NA_real_, color = as.factor(r)), linewidth = 1) &
    theme(legend.position = "bottom")

  y_lims <- range(
    c(data_s$y, data_e$y),
    na.rm = TRUE, finite = TRUE
  )
  
  g <- c(as.list(g_values)[], as.list(g_distribution)[]) |>
    wrap_plots(byrow = FALSE, guides = "collect", axes = "collect") &
    ylim(y_lims[1] * 1.1, y_lims[2] * 1.1) &
    conditional_color(TRUE, na.translate = FALSE)
  g +
    plot_annotation(
      title = title,
      theme = theme(legend.position = "bottom")
    )
}



# Residuals --------------------------------------------------------------------

#' Todo: document
#' @export
residuals_values <- function(
  data_s, data_e, regime_aligned = TRUE,
  sims = 1, n_burn = NA, n_t = length(unique(data_e$t)), n_h = 0,
  multiple = TRUE, hline = NULL, title = NULL, faceted = NULL
) {
  args <- list(
    sims = sims, n_burn = n_burn, multiple = multiple, hline = hline,
    title = title, faceted = faceted
  )

  data <- get_error_data(data_s, data_e, regime_aligned)

  inject(simulations$series_values(data, !!!args)) +
    annotate_pred(n_t, n_h) +
    conditional_color(regime_aligned, na.translate = FALSE)
}

#' Todo: document
#' @export
residuals_distribution <- function(
  data_s, data_e, regime_aligned = TRUE,
  n_burn = NA, hline = 0, title = NULL, faceted = NULL
) {
  args <- list(
    n_burn = n_burn, hline = hline, title = title, faceted = faceted
  )

  data <- get_error_data(data_s, data_e, regime_aligned)

  inject(simulations$series_distribution(data, !!!args)) +
    conditional_color(regime_aligned, na.translate = FALSE)
}

#' Todo: document
#' @export
panel_residuals <- function(
  data_s, data_e, regime_aligned = TRUE,
  n_burn = NA, n_t = length(unique(data_e$t)), n_h = 0,
  hline = NULL, title = NULL
) {
  g_values <- residuals_values(
    data_s, data_e, regime_aligned = regime_aligned,
    sims = 1, n_burn = n_burn, n_t = n_t, n_h = n_h, hline = hline
  ) +
    theme(legend.position = "none")
  g_distribution <- residuals_distribution(
    data_s, data_e, regime_aligned = regime_aligned, n_burn = n_burn
  ) +
    geom_line(aes(NA_real_, NA_real_, color = as.factor(r))) +
    theme(legend.direction = "horizontal")

  y_lims <- range(
    get_error_data(data_s, data_e, regime_aligned)$y,
    na.rm = TRUE, finite = TRUE
  )

  (
    g_values - g_distribution &
      ylim(y_lims[1] * 1.1, y_lims[2] * 1.1) 
  ) +
    plot_layout(
      nrow = 1, guides = "collect", axes = "collect_y"
    ) +
    plot_annotation(
      title = title,
      #subtitle = glue("SGP: {unique(data$sgp)}\nRGP: {unique(data$rgp)}"),
      theme = theme(legend.position = "bottom")
    )
}



# Coefficients -----------------------------------------------------------------

#' Todo: document
#' @export
coefs_distribution <- function(
  data, params, model_names,
  models = unique(model_names$model),
  lims = list(mu = c(NA, NA), rho1 = c(NA, NA)), title = NULL
) {
  correct <- get_correct_params(params, dicts, model_names)

  gdata <- data %>%
    filter(model %in% dicts$models[models]) %>%
    unnest_wider(meta) %>%
    rowwise() %>%
    mutate(coefs = list(map(asplit(coefs, 2), c))) %>%
    ungroup() %>%
    unnest_wider(coefs, names_sep = "_") %>%
    unnest_wider(starts_with("coefs_"), names_sep = "_") %>%
    pivot_longer(
      starts_with("coefs_"),
      names_pattern = "coefs_R([0-9]+)_(.+)",
      names_to = c("regime", "coef"),
      values_to = "value",
      names_transform = list(regime = as.integer)
    )

  ggplot(gdata, aes(x = value, color = as.factor(regime))) +
    geom_density() +
    geom_vline(
      aes(xintercept = value, color = as.factor(regime)), correct,
      linetype = "dashed"
    ) +
    ggh4x::facet_grid2(vars(sgp), vars(coef), scales = "free", independent = "y") +
    ggh4x::facetted_pos_scales(
      x = map(lims, \(x) scale_x_continuous(limits = x))
    ) +
    labs(
      title = title, x = "Value", y = "Density",
      color = "Regime", fill = "Regime"
    )
}



# Residuals Table --------------------------------------------------------------

#' Todo: document
#' @export
table_residuals <- function(
  data_s, data_e, regime_aligned = TRUE,
  dgps = NULL, ..., n_burn = n_burn
) {
  # Setup:
  if (Sys.getenv("RADIAN_VERSION") != "") {
    cli_abort("This function requires {{gt}}, which does not work well in \\
    Radian. Consider running this one in RStudio.")
  }

  add_spanner <- function(data, n) {
    r_name <- glue("R{n}")
    tab_spanner(data, r_name, matches(r_name))
  }

  data <- get_error_data(data_s, data_e, regime_aligned) |>
    filter(t > n_burn + 2) # 2 is the warmup. For most models, it is n_l + 1. Todo: generalize
  
  if (!is_null(dgps)) {
    data <- filter(data, str_c(sgp, rgp, sep = "-") %in% dgps)
  }
  n_r <- length(unique(data$r))

  # Metrics and ANOVA:
  data_metrics <- data |> 
    group_by(sgp, rgp, sim) %>%
    reframe(
      sgp_metric = metrics$series_average(y, r, ...),
      r = 1:n_r
    )

  data_anova <- data_metrics %>%
    group_by(sgp, rgp) %>%
    summarise(
      pvalue = anova(lm(sgp_metric ~ r))[["Pr(>F)"]][1]
    )

  data_final <- data_metrics %>%
    group_by(sgp, rgp, r) %>%
    summarise(
      across(sgp_metric, list("Mean" = mean, "SD" = sd), .names = "{.fn}")
    ) %>%
    pivot_wider(
      names_from = r,
      values_from = matches("Mean|SD"),
      names_glue = "{.value}_R{r}"
    ) %>%
    left_join(data_anova, by = c("sgp", "rgp"))


  # GT table:
  data_final |>
    ungroup() |>
    mutate(
      across(matches("Mean|SD"), ~ formatC(.x, format = "f", digits = 2)),
      pvalue = paste0(
        formatC(pvalue, format = "e", digits = 1),
        add_star(pvalue)
      ),
      sgp = fct_relabel(sgp, ~ dicts$sgps[.x]),
      rgp = fct_relabel(rgp, ~ dicts$rgps[.x])
    ) |>
    gt() %>%
    reduce(1:n_r, add_spanner, .init = .) |>
    tab_spanner("ANOVA", "pvalue") |>
    text_transform(location = cells_column_labels(), \(x) {
      str_replace_all(x, c(
        "_R[1-9]" = "",
        "sgp" = "SGP", "rgp" = "RGP",
        "pvalue" = "P-value"
      ))
    })
}
