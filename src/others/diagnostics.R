
# Setup ------------------------------------------------------------------------

# Loading dependencies
box::use(
  src/utils[...],
  src/metrics,
  src/options/sgps[sgp_names = options_names],
  src/options/rgps[rgp_names = options_names]
)

# Graphing functions often include a title argument that suppresses the title
#  by default. Similarly, the faceted argument removes faceting, and by default
#  does it if there is no variation in both faceting variables


# Helpers ----------------------------------------------------------------------

#' Internal: Burn-in period background via annotate
annotate_burn <- function(n_burn, ...) {
  args_annot <- list2(...)
  args_annot$alpha <- args_annot$alpha %||% 0.5
  args_annot$fill <- args_annot$fill %||% "darkgrey"

  inject(list(
    annotate(
      "rect", xmin = 1, xmax = n_burn, ymin = -Inf, ymax = Inf, !!!args_annot
    )
  ))
}

#' Internal: Regimes background via annotate
#' @param axis [`character(1)`] Across which axis the regimes vary?
annotate_regimes <- function(axis = "x", ...) {
  args_annot <- list2(...)
  args_annot$alpha <- args_annot$alpha %||% 0.3

  coords_order <- switch(axis,
    "x" = c("xmin", "xmax", "ymin", "ymax"),
    "y" = c("ymin", "ymax", "xmin", "xmax")
  )

  coords_1 <- set_names(c(1 - 0.25, 1 + 0.25, -Inf, Inf), coords_order)
  coords_2 <- set_names(c(2 - 0.25, 2 + 0.25, -Inf, Inf), coords_order)

  inject(list(
    annotate("rect", !!!args_annot, fill = pal$main["green"], !!!coords_1),
    annotate("rect", !!!args_annot, fill = pal$main["orange"], !!!coords_2)
  ))
}

#' Internal: Regimes background via geom_rect
#' Assumes x axis is `t` and regime variable is `r`
rect_regimes <- function(...) {
  args_rect <- list2(...)
  args_rect$alpha <- args_rect$alpha %||% 0.3

  inject(geom_rect(
    aes(
      ymin = -Inf, ymax = Inf, xmin = t - 0.5, xmax = t + 0.5,
      fill = as.factor(r)
    ),
    !!!args_rect
  ))
}



# Data Subsetting --------------------------------------------------------------

#' Helper: Subset and relabel simulation data
#'
#' @param data [`data.frame()`] Input data containing simulations information.
#' @param sgps, rgps [`character()`][optional] SGP/RGP values to filter by.
#'  If `NULL`, no filtering is applied on RGP.
#' @param sims [`numeric()`][optional] Simulation identifiers to filter by. If
#'  `NULL`, no filtering is applied on simulations.
#'
#' @returns [`data.frame()`] Filtered and relabeled simulations data.
#' @export
subset_simulations <- function(data, sgps = NULL, rgps = NULL, sims = NULL) {
  data %>%
    filter(
      if (is_null(sgps)) TRUE else sgp %in% sgps,
      if (is_null(rgps)) TRUE else rgp %in% rgps,
      if (is_null(sims)) TRUE else sim %in% sims
    ) %>%
    mutate(
      sgp = fct_relabel(sgp, ~ sgp_names[.x]),
      rgp = fct_relabel(rgp, ~ rgp_names[.x])
    )
}



# Errors -----------------------------------------------------------------------

#' Diagnostics - errors: Dependence between error segments
#'
#' @param errors [`numeric()`]
#' @param grain [`integer(1)`] Number of segments to divide the errors by.
#'  Often the number of parallel tasks used to create them, assuming they
#'  weren't reordered.
#' @param simmetric, triangular [`logical(1)`] Whether to use absolute
#'  correlation and whether to show the top triangular matrix only.
#'
#' @returns [`ggplot()`] Correlation matrix heatmap of error segments.
#' @export
errors_dependence <- function(
  errors, grain = 100,
  simmetric = TRUE, triangular = TRUE, title = FALSE
) {
  if (simmetric) {
    get_cor <- function(matrix) abs(cor(matrix))
    limits <- c(0, 1)
  } else {
    get_cor <- function(matrix) cor(matrix)
    limits <- c(-1, 1)
  }

  error_cor <- errors %>%
    matrix(nrow = length(.) / grain, ncol = grain) |>
    get_cor() |>
    `dim<-`(NULL) |>
    tibble(x = rep(1:grain, each = grain), y = rep(1:grain, grain), cor = _) |>
    filter(if(triangular) y > x else x != y)

  ggplot(error_cor, aes(x, y, fill = cor)) +
    geom_tile() +
    scale_fill_viridis_c(option = "plasma", limits = limits) +
    coord_equal() +
    labs(
      title = if (title) "Error correlation (100-sample segments)",
      x = "Parallel task N°", y = "Parallel task N°",
      fill = "Absolute Correlation"
    )
}

#' Diagnostics - errors: Error distribution
#'
#' @param errors [`numeric()`]
#'
#' @returns [`patchwork`] Combined QQ plot and histogram of error distribution.
#' @export
errors_distribution <- function(errors, title = FALSE) {
  g_qq <- ggplot(tibble(x = sample(errors, 3000))) +
    geom_abline(
      slope = 1, intercept = 0,
      color = pal$main["orange"], linewidth = 1
    ) +
    geom_qq(aes(sample = x), alpha = 0.3, color = pal$gray["blackgray"]) +
    coord_equal() +
    labs(title = "QQ plot", x = "Theoretical Quantiles", y = "Sample Quantiles")

  g_hist <- ggplot(tibble(x = sample(errors, 3000))) +
    geom_histogram(
      aes(x, after_stat(density)), bins = 70,
      fill = pal$gray["blackgray"], color = "black"
    ) +
    stat_function(
      fun = dnorm, args = list(mean = 0, sd = 1),
      color = pal$main["orange"], linewidth = 1
    ) +
    labs(title = "Histogram", x = "Value", y = "Density")

  ((g_hist - g_qq) * theme(plot.title = element_text(size = 12))) +
    plot_annotation(
      title = if (title) "Error distribution (sample of 3000)"
    )
}



# Simulations Graphs -----------------------------------------------------------

#' Diagnostics - simulations: Visualize simulated series' values
#'
#' @param data [`data.frame()`-like]
#' @param sims [`integer()`] Simulations to visualize.
#' @param n_burn [`integer(1)`] Burn-in period size.
#'
#' @returns [`ggplot()`]
#' @export
series_values <- function(
  data, sims = 1, n_burn,
  title = FALSE, faceted = NULL
) {
  faceted <- faceted %||% (
    length(unique(data$sgp)) > 1 || length(unique(data$rgp)) > 1
  )

  gdata <- filter(data, sim %in% sims)

  ggplot(gdata, aes(x = t, y = y)) +
    annotate_burn(n_burn) +
    geom_line(
      aes(color = as.factor(r), group = sim),
      alpha = 0.6, linewidth = 1.25
    ) +
    {if (length(sims) == 1) rect_regimes()} +
    {if (faceted) facet_grid(vars(sgp), vars(rgp))} +
    scale_color_manual(values = unname(pal$main)) +
    labs(
      title = if (title) "Simulated series - values",
      x = "Time", y = "Value", color = "Regime", fill = "Regime"
    )
}

#' Diagnostics - simulations: Visualize simulated series' distribution
#'
#' @param data [`data.frame()-like`]
#' @param n_burn [`integer(1)`] Burn-in period size.
#'
#' @returns [`ggplot()`]
#' @export
series_distribution <- function(data, n_burn, title = FALSE, faceted = NULL) {
  faceted <- faceted %||% (
    length(unique(data$sgp)) > 1 || length(unique(data$rgp)) > 1
  )

  data %>%
    filter(t > n_burn) %>%
    ggplot(aes(x = y, color = as.factor(r))) +
    geom_density() +
    {if (faceted) facet_grid(vars(sgp), vars(rgp), scales = "free")} +
    scale_color_manual(values = unname(pal$main)) +
    labs(
      title = if (title) "Simulated series - distribution",
      x = "Value", y = "Density", color = "Regime"
    )
}

#' Diagnostics - series: Visualize simulated series' conditional statistics
#'
#' @param data [`data.frame()`-like]
#' @param stat [`function()`]
#' @param stat_name [`character(1)`] Label for the y axis.
#' @param sims [`integer()`] Simulations to visualize.
#' @param multiple [`logical(1)`] Whether to consider multiple
#' @param n_burn [`integer(1)`] Burn-in period size.
#'
#' @returns [`ggplot()`] Plot of conditional statistics by regime over time.
#' @export
series_stats <- function(
  data, stat, stat_name,
  sims = 1, multiple = length(sims) > 1, n_burn, faceted = NULL
) {
  conditional_stat <- function(y, t, r, cond_t, cond_r) {
    #print(cond_r); print(length(y))
    t_idx <- t <= cond_t
    stat(y[t_idx][r[t_idx] == cond_r])
  }

  faceted <- faceted %||% (
    length(unique(data$sgp)) > 1 || length(unique(data$rgp)) > 1
  )
  groups <- `if`(multiple,
    exprs(sgp, rgp, sim),
    exprs(sgp, rgp, sim = "placeholder")
  )

  gdata <- data %>%
    filter(sim %in% sims) %>%
    group_by(!!!groups) %>%
    reframe(
      map_dfc(set_names(unique(r), ~ glue("R{.x}")), \(conditional_r) {
        map_dbl(1:max(data$t), ~ conditional_stat(y, t, r, .x, conditional_r))
      }),
      t = 1:max(data$t),
      r = if (length(sims) == 1) r else NA
    ) %>%
    pivot_longer(
      matches("^R[0-9]+$"),
      names_prefix = "R", names_to = "conditional_r", values_to = "value"
    )

  ggplot(gdata, aes(t, value)) +
    {if (!multiple & length(sims) == 1) rect_regimes()} +
    annotate_burn(n_burn) +
    geom_line(
      aes(color = conditional_r, group = interaction(sim, conditional_r)),
      linewidth = 1, alpha = if (length(sims) > 1) 0.6 else 1
    ) +
    {if (faceted) facet_grid(vars(sgp), vars(rgp))} +
    scale_color_manual(values = unname(pal$main)) +
    scale_fill_manual(values = unname(pal$main)) +
    labs(
      #title = glue("Simulated series - conditional {stat_name}"),
      x = "Time", y = stat_name,
      color = "Regime", fill = "Regime", linetype = "Statistic"
    )
}

#' Diagnostics: Visualize conditional statistics of simulated regimes
#'
#' @param gdata [`data.frame()`] Input data containing simulation results.
#' @param stats [`function()`] Function to compute statistics on the regimes.
#' @param sims [`integer(1)`] Simulation identifier to visualize.
#' @param n_burn [`integer(1)`] Number of initial time steps to consider as
#'   burn-in.
#' @param stat_name [`character(1)`] Name of the statistic for labeling.
#'
#' @returns [`ggplot()`] Plot of conditional statistics by regime over time.
#' @export
regimes_stats <- function(
  data, stats, stat_name = NULL,
  sims = 1, multiple = length(sims) > 1, n_burn, faceted = NULL
) {
  faceted <- faceted %||% (
    length(unique(data$sgp)) > 1 || length(unique(data$rgp)) > 1
  )
  groups <- `if`(multiple,
    exprs(sgp, rgp, sim),
    exprs(sgp, rgp, sim = "placeholder")
  )

  gdata <- data %>%
    filter(sim %in% sims) %>%
    group_by(!!!groups) %>%
    reframe(
      map_dfr(1:max(data$t), ~ stats(y = y[t <= .x], r = r[t <= .x])),
      t = 1:max(data$t),
      r = if (length(sims) == 1) r else NA
    ) %>%
    pivot_longer(
      -c(sgp, rgp, sim, t, r),
      names_to = "stat",
      values_to = "value"
    )

  ggplot(gdata, aes(t, value)) +
    {if (!multiple & length(sims) == 1) rect_regimes()} +
    annotate_burn(n_burn) +
    geom_line(
      aes(color = stat, group = interaction(stat, sim)),
      linewidth = 1
    ) +
    {if (faceted) facet_grid(vars(sgp), vars(rgp))} +
    scale_color_viridis_d(option = "plasma", end = 0.8) +
    #scale_color_manual(values = unname(pal$main)) +
    scale_fill_manual(values = unname(pal$main)) +
    labs(
      #title = glue("Simulated series - regime {stat_name}"),
      x = "Time", y = stat_name,
      linetype = "Statistic", color = "Statistic", fill = "Regime"
    )
}

#' Diagnostics: Panel of simulated series and statistics
#' @export
panel_simulations <- function(
  data, sgp, rgp, sims, multiple, n_burn,
  y_stat, y_stat_name, r_stats, r_stat_name,
  ...
) {
  gdata <- subset_simulations(
    data, sgps = sgp, rgps = rgp, sims = sims
  )

  g_values <- diagnose$series_values(gdata, sims = sims, n_burn = n_burn)
  g_dist <- diagnose$series_distribution(
    gdata, n_burn = n_burn
  ) + theme(legend.position = "none")
  g_y <- diagnose$series_stats(
    gdata, stat = y_stat, stat_name = y_stat_name,
    sims = sims, multiple = multiple, n_burn = n_burn
  ) + theme(legend.position = "none")
  g_r <- diagnose$regimes_stats(
    gdata, stats = r_stats, stat_name = r_stat_name,
    sims = sims, multiple = multiple, n_burn = n_burn
  )

  (g_values + g_dist + g_y + g_r) +
    plot_layout(guides = "collect") +
    plot_annotation(...)
}



# Simulations: Tables ----------------------------------------------------------

#' ...
sgp_metric <- function(sgp, y, r, n_r = length(unique(r))) {
  force(n_r)

  switch(str_replace(sgp[1], "^r[0-9]_ar[0-9]_([a-z]]+)[0-9]$", "\\1"),
    "mu"   = metrics$series_average(y, r, n_r),
    "rho"  = metrics$series_autocorr(y, r, n_r),
    "sign" = metrics$series_sign_prop(y, r, n_r),
    "vol"  = metrics$series_volatility(y, r, n_r),
    NA # Not considering new lag
  )
}

#' ...
rgp_metric <- function(rgp, y, r, n_r = length(unique(r))) {
  force(n_r)

  switch(str_replace(rgp[1], "^r[0-9]_([a-z]+)_[a-z0-9_]+$", "\\1"),
    "threshold" = metrics$regimes_thresholds(y, r, n_r),
    "markov"    = metrics$regimes_transmat(y, r, n_r) |> diag(),
    NA # Not considering multinomial nor st nor sbreaks
  )
}

#' Diagnostics: Table of simulation metrics
#' @export
table_simulations <- function(data) {
  data %>%
    group_by(sgp, rgp, sim) %>%
    reframe(
      r = 1:length(unique(r)),
      sgp_metric = sgp_metric(sgp, y, r),
      rgp_metric = rgp_metric(rgp, y, r)
    ) %>%
    group_by(sgp, rgp) %>%
    summarise(
      across(
        -c(sim, r), list("mean" = mean, "SD" = sd),
        .names = "{.col} ({.fn})"
      ),
    ) %>%
    ungroup() %>%
    mutate(
      sgp = fct_relabel(sgp, ~ sgp_names[.x]),
      rgp = fct_relabel(rgp, ~ rgp_names[.x])
    )
}



# Unused -----------------------------------------------------------------------

#' Diagnostics: Visualize simulated paths
#'
#' @param gdata [`data.frame()`] Input data containing simulation results.
#' @param t_max [`integer(1)`] Maximum time step to visualize.
#' @param sims [`integer(1)`] Simulation identifiers to visualize.
#' @param n_burn [`integer(1)`] Number of initial time steps to consider as
#'   burn-in.
#'
#' @returns [`ggplot()`] Plot of simulated paths in regime-value space.
series_paths <- function(
  data, t_max = max(data$t), sims = 1,
  n_burn, faceted = NULL
) {
  faceted <- faceted %||% (
    length(unique(data$sgp)) > 1 || length(unique(data$rgp)) > 1
  )

  gdata <- data %>%
    filter(sim %in% sims & t %in% 1:t_max) %>%
    group_by(sgp, rgp) %>%
    mutate(
      across(c(r, y), ~ jitter(.x, amount = 0.2)),
      r_lead = lead(r), y_lead = lead(y), t_lead = lead(t),
      burn = t <= n_burn
    )

  ggplot(gdata, aes(x = r, y = y)) +
    annotate_regimes("x") +
    geom_segment(
      aes(xend = r_lead, yend = y_lead, color = t, group = sim),
      arrow = arrow1
    ) +
    scale_color_viridis_c(
      option = "plasma", end = 0.8, direction = -1,
      breaks = c(1, seq(20, t_max, by = 20))
    ) +
    #ggnewscale::new_scale_color() +
    #geom_text(aes(label = t, y = y + 0.3, color = burn)) +
    #scale_color_manual(values = c("TRUE" = pal$gray["blackgray"], "FALSE" = "#0c0c0c")) +
    geom_point(
      data = filter(gdata, t == 1),
      size = 4, color = pal$main["red"], shape = 18
    ) +
    {if (faceted) facet_grid(vars(sgp), vars(rgp))} +
    scale_x_continuous(breaks = 1:2, minor_breaks = NULL) +
    labs(
      #title = "Simulated series - paths",
      x = "Regime", y = "Value", color = "Time:"
    ) +
    theme(legend.position = "bottom")
}

#' Diagnostics: Visualize simulated regime paths
#'
#' @param gdata [`data.frame()`] Input data containing simulation results.
#' @param t_max [`integer(1)`] Maximum time step to visualize.
#' @param sims [`integer(1)`] Simulation identifiers to visualize.
#' @param n_burn [`integer(1)`] Number of initial time steps to consider as
#'   burn-in.
#'
#' @returns [`ggplot()`] Plot of simulated regime paths over time.
regimes_values <- function(
  data, t_max = max(data$t), sims = 1,
  n_burn, faceted = NULL
) {
  faceted <- faceted %||% (
    length(unique(data$sgp)) > 1 || length(unique(data$rgp)) > 1
  )

  gdata <- data %>%
    filter(sim %in% sims & t %in% 1:t_max) %>%
    group_by(sgp, rgp, sim) %>%
    mutate(
      if (length(sims) > 1) across(c(r), ~ jitter(.x, amount = 0.1)),
      r_lead = lead(r), y_lead = lead(y), t_lead = lead(t)
    )

  ggplot(gdata, aes(x = t, y = r)) +
    annotate_regimes("y") +
    annotate_burn(n_burn) +
    geom_segment(
      aes(xend = t_lead, yend = r_lead, group = sim),
      arrow = arrow1, alpha = if (length(sims) > 1) 0.4 else 1,
    ) +
    {if (faceted) facet_grid(vars(sgp), vars(rgp))} +
    scale_y_continuous(breaks = 1:2, minor_breaks = NULL) +
    labs(
      #title = "Simulated series - regimes",
      x = "Time", y = "Regime"
    )
}
