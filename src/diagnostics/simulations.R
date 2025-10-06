
# Setup ------------------------------------------------------------------------

# Loading dependencies:
box::use(
  ./utils_diagnostics[...]
)

if (Sys.getenv("RADIAN_VERSION") == "") box::use(gt[...])
# Radian has unicode problems that break gt loading. Run functions that require
# it in RStudio instead

# General parameters:
#' @param data [`data.frame()`-like]
#' @param sims [`integer()`] Simulations to visualize.
#' @param n_burn [`integer(1)`] Burn-in period size.
#' @param multiple [`logical(1)`] Whether plot individual simulations or their
#'  average. Defaults to `TRUE` if more than one simulation is provided, and
#' `FALSE` otherwise.

# Aesthetic parameters:
#' @param title [`character(1)`] Title to include. Defaults to none (NULL).
#' @param y_lab [`character(1)`] Y-axis label. Defaults to "Value".
#' @param faceted [`logical(1)`] Whether to facet by SGP and RGP.
#'  If more than one SGP or RGP is present in data, it should be yes. The
#'  default is `TRUE` in that case, and `FALSE` otherwise.

# All functions return a [`ggplot()`]



# Series Values and Distribution -----------------------------------------------

#' Diagnostics - simulations: Visualize simulated series' values
#' @param multiple [`logical(1)`] Here, this argument only controls whether to
#'  include regimes background or not.
#' @export
series_values <- function(
  data, sims = 1, n_burn = NA, multiple = TRUE, hline = NULL,
  title = NULL, faceted = NULL
) {
  gdata <- filter(data, sim %in% sims)

  ggplot(gdata, aes(x = t, y = y)) +
    conditional_rect(sims, multiple = multiple) +
    annotate_burn(n_burn) +
    {if (!is_null(hline)) geom_hline(yintercept = hline)} +
    geom_line(
      aes(color = as.factor(r), group = as.factor(sim)),
      alpha = if (length(sims) == 1) 1 else 0.6, linewidth = 1
    ) +
    conditional_facet(gdata$sgp, gdata$rgp, faceted) +
    # Aesthetics:
    scale_color_manual(values = unname(pal$main), na.translate = FALSE) +
    labs(
      title = title, x = "Time", y = "Value",
      color = "Regime", fill = "Regime"
    )
}

#' Diagnostics - simulations: Visualize simulated series' distribution
#' @export
series_distribution <- function(
  data, n_burn = 0,
  hline = NULL, title = NULL, faceted = NULL
) {
  data %>%
    filter(t > n_burn) %>%
    ggplot(aes(y = y, color = as.factor(r))) +
    {if (!is_null(hline)) geom_hline(yintercept = hline)} +
    geom_density(linewidth = 1) +
    conditional_facet(data$sgp, data$rgp, faceted) +
    # Aesthetics:
    scale_color_manual(values = unname(pal$main)) +
    labs(
      title = title, x = "Density", y = "Value",
      color = "Regime"
    )
}

#' Diagnostics - simulations: Panel of simulated series' values and distribution
#' @export
panel_simulations <- function(
  data, n_burn = NA, title = NULL
) {
  g_values <- series_values(data, sims = 1, n_burn = n_burn) +
    theme(legend.position = "none")
  g_distribution <- series_distribution(data, n_burn = n_burn) +
    geom_line(aes(NA_real_, NA_real_, color = as.factor(r))) +
    theme(legend.direction = "horizontal")

  y_lims <- range(filter(data)$y, na.rm = TRUE, finite = TRUE)

  (
    g_values - g_distribution &
      ylim(y_lims[1] * 1.1, y_lims[2] * 1.1) 
  ) +
    plot_layout(
      nrow = 1, guides = "collect", axes = "collect_y"
    ) +
    plot_annotation(
      title = title,
      subtitle = glue("SGP: {unique(data$sgp)}\nRGP: {unique(data$rgp)}"),
      theme = theme(legend.position = "bottom")
    )
}
# Todo: allow aesthetics customization; standardize y_lims betweem sims and also
# between panels



# Metrics Values ----------------------------------------------------------

#' Diagnostics - metrics: Series or regimes statistics accumulated over time
#' @param stats [`function()`] Function to compute statistics. It should have a
#'  Similar signature to those in `rgp_metrics()` or `sgp_metrics()`.
#' @param regime_aligned [`logical(1)`] Whether the statistics map to regimes,
#'  and thus so should their line colors, or not (the default).
#' @export
stats_accumulated <- function(
  data, stats,
  sims = 1, n_burn = NA, multiple = length(sims) > 1,
  title = NULL, faceted = NULL, regime_aligned = FALSE
) {
  groups <- `if`(multiple,
    exprs(sgp, rgp, sim),
    exprs(sgp, rgp, sim = "placeholder")
  )

  gdata <- data %>%
    filter(sim %in% sims) %>%
    group_by(!!!groups) %>%
    reframe(
      map_dfr(1:max(data$t), \(tmax) {
        stats(y = y[t <= tmax], r = r[t <= tmax], n_r = length(unique(r)))
      }),
      t = 1:max(data$t), r = if (length(sims) == 1) r else NA
    ) %>%
    pivot_longer(
      -c(sgp, rgp, sim, t, r),
      names_to = "stat", values_to = "value"
    )

  ggplot(gdata, aes(t, value)) +
    conditional_rect(sims, multiple) +
    annotate_burn(n_burn) +
    geom_line(
      aes(color = stat, group = interaction(stat, sim)),
      linewidth = 1, alpha = if (length(sims) > 1) 0.6 else 1
    ) +
    conditional_facet(gdata$sgp, gdata$rgp, faceted) +
    # Aesthetics:
    conditional_color(regime_aligned) +
    labs(
      title = title, x = "Time", y = "Value",
      color = "Regime"
    )
}

#' Diagnostics - metrics: Series or regimes statistics' distribution
#' @param stats [`function()`] Function to compute statistics. It should have a
#'  Similar signature to those in `rgp_metrics()` or `sgp_metrics()`.
#' @param regime_aligned [`logical(1)`] Whether the statistics map to regimes,
#'  and thus so should their line colors, or not (the default).
#' @export
stats_density <- function(
  data, stats, sims = 1, n_burn = NA,
  title = NULL, faceted = NULL, regime_aligned = FALSE
) {
  gdata <- data %>%
    group_by(sgp, rgp, sim) %>%
    reframe(
      map_dfc(stats(y = y, r = r, n_r = length(unique(r))), ~ .x)
    ) %>%
    pivot_longer(
      -c(sgp, rgp, sim),
      names_to = "stat", values_to = "value"
    )

  ggplot(gdata, aes(y = value)) +
    geom_density(aes(color = stat)) +
    conditional_facet(gdata$sgp, gdata$rgp, faceted) +
    # Aesthetics:
    conditional_color(regime_aligned) +
    labs(
      title = title, x = "Density", y = "Value",
      linetype = "Statistic", color = "Statistic"
    )
}

#' Diagnostics - metrics: Panel of simulated metrics
#' @param dimension [`character(1)`] Wheter to calculate metrics to "sgp" or
#'  "rgp".
#' @param option [`character(1)`] Name of the SGP or RGP to consider.
#' @param stats [`function(){}`] Function with similar signature as
#'  `sgp_metric()` or `rgp_metric()`.
#' @param regime_aligned [`logical(1)`] Whether the statistics map to regimes,
#'  and thus so should their line colors, or not (the default).
#' @export
panel_stats <- function(
  data, dimension, option, stats = NULL,
  sims = 1, n_burn = NA, regime_aligned, title = NULL
) {
  stats <- stats %||% switch(dimension,
    "sgp" = \(y, r, n_r) sgp_metric(option, y, r, n_r),
    "rgp" = \(y, r, n_r) rgp_metric(option, y, r, n_r),
    cli_abort("{.arg dimension} must be one of {.val 'sgp'} or {.val 'rgp'}.")
  )

  g_accumulated <- stats_accumulated(
    data, stats, sims, n_burn, regime_aligned = regime_aligned
  ) +
    theme(legend.position = "none")

  g_distribution <- stats_density(
    data, stats, n_burn, regime_aligned = regime_aligned
  ) +
    geom_line(aes(NA_real_, NA_real_, color = as.factor(stat))) +
    theme(legend.direction = "horizontal")

  y_lims <- range(g_accumulated$data$value, na.rm = TRUE, finite = TRUE)

  (
    g_accumulated - g_distribution &
      ylim(y_lims[1] * 1.0, y_lims[2] * 1.0) 
  ) +
    plot_layout(nrow = 1, guides = "collect", axes = "collect_y") +
    plot_annotation(
      title = title,
      subtitle = glue("SGP: {unique(data$sgp)}\nRGP: {unique(data$rgp)}"),
      theme = theme(legend.position = "bottom")
    )
}



# Metrics  Tables --------------------------------------------------------------

#' Diagnostics - metrics: Table of SGPs metrics
#' @export
table_sgps <- function(data, dgps = NULL) {
  # Setup:
  if (Sys.getenv("RADIAN_VERSION") != "") {
    cli_abort("This function requires {{gt}}, which does not work well in \\
    Radian. Consider running this one in RStudio.")
  }

  add_spanner <- function(data, n) {
    r_name <- glue("R{n}")
    tab_spanner(data, r_name, matches(r_name))
  }

  if (!is_null(dgps)) {
    data <- filter(data, str_c(sgp, rgp, sep = "-") %in% dgps)
  }
  n_r <- length(unique(data$r))

  # Metrics and ANOVA:
  data_metrics <- data %>%
    group_by(sgp, rgp, sim) %>%
    reframe(
      sgp_metric = sgp_metric(sgp[1], y, r),
      r = 1:length(unique(r))
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

#' Diagnostics - metrics: Table of RGPs metrics
#' @export
table_rgps <- function(data, dgps = NULL) {
  # Setup:
  if (Sys.getenv("RADIAN_VERSION") != "") {
    cli_abort("This function requires {{gt}}, which does not work well in \\
    Radian. Consider running this one in RStudio.")
  }
  apply_and_format <- function(res, f) {
    formatC(apply(res, 2, f), format = "f", digits = 2) |>
      str_c(collapse = ", ") |>
      str_replace(" NA", "-")
  }

  if (!is_null(dgps)) {
    data <- filter(data, str_c(sgp, rgp, sep = "-") %in% dgps)
  }
  dgp_names <- expand_grid(
    sgp_name = unique(data$sgp),
    rgp_name = unique(data$rgp)
  )

  # Metrics:
  data_final <- pmap_dfr(dgp_names, \(sgp_name, rgp_name) {
    data_group <- filter(data, sgp == sgp_name & rgp == rgp_name)

    res <- map_dfr(unique(data_group$sim), \(x) {
      with(filter(data_group, sim == x), rgp_metric(rgp_name, y, r))
    })

    list(
      sgp = sgp_name, rgp = rgp_name,
      Mean = apply_and_format(res, mean), SD = apply_and_format(res, sd)
    )
  })

  # GT table:
  data_final |>
    ungroup() |>
    mutate(
      sgp = fct_relabel(sgp, ~ dicts$sgps[.x]),
      rgp = fct_relabel(rgp, ~ dicts$rgps[.x])
    ) |>
    gt() |>
    text_transform(location = cells_column_labels(), \(x) {
      str_replace_all(x, c(
        "_R[1-9]" = "",
        "sgp" = "SGP", "rgp" = "RGP",
        "pvalue" = "P-value"
      ))
    })
}
