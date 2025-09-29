
# Setup ------------------------------------------------------------------------

# Loading dependencies
box::use(
  ./utils[...],
  ./options/sgps[sgp_names = options_names],
  ./options/rgps[rgp_names = options_names]
)



# Helpers ----------------------------------------------------------------------

#' Estimate transition matrix from regime sequence
#'
#' @param r [`integer()`] Vector of regime labels, typically representing a
#'   sequence of states or regimes.
#' @param n_r [`integer(1)`] Number of unique regimes. Defaults to the number of
#'   unique values in `r`.
#' @param prop [`logical(1)`] Whether to return transition probabilities
#'   (`TRUE`) or raw counts (`FALSE`). Defaults to `TRUE`.
#'
#' @returns [`matrix(ncol = n_r, nrow = n_r)`] A square matrix of transition
#'   counts or probabilities between regimes, depending on the value of `prop`.
#' @export
estimate_transmat <- function(r, n_r = length(unique(r)), prop = TRUE) {
  counts <- matrix(0, n_r, n_r)
  r_diff <- c(0, diff(r))
  ind <- r_diff != 0

  counts[c(2, 3)] <- tabulate(r_diff[ind] + 2)[-2]
  counts[c(1, 4)] <- tabulate(r[!ind])

  if (prop) {
    counts / rowSums(counts)
  } else {
    counts
  }
}
# Todo: rethink wether to export or not


#' Subset and relabel simulation data
#'
#' Filters the input based on the arguments and relabels the SGPs and RGPs.
#'
#' @param data [`data.frame()`] Input data containing simulations information.
#' @param sgps [`character()`][optional] SGP values to filter by. If `NULL`,
#'   no filtering is applied on SGP.
#' @param rgps [`character()`][optional] RGP values to filter by. If `NULL`,
#'   no filtering is applied on RGP.
#' @param sims [`numeric()`][optional] Simulation identifiers to filter by. If
#'   `NULL`, no filtering is applied on simulations.
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


annotate_burn <- function(n_burn) {
  list(annotate(
    "rect", fill = "darkgrey", alpha = 0.5,
    xmin = 1, xmax = n_burn, ymin = -Inf, ymax = Inf
  ))
}


annotate_regimes <- function(axis = "x") {
  coords_order <- switch(axis,
    "x" = c("xmin", "xmax", "ymin", "ymax"),
    "y" = c("ymin", "ymax", "xmin", "xmax")
  )

  coords_1 <- set_names(c(1 - 0.25, 1 + 0.25, -Inf, Inf), coords_order)
  coords_2 <- set_names(c(2 - 0.25, 2 + 0.25, -Inf, Inf), coords_order)

  inject(list(
    annotate("rect", alpha = 0.3, fill = pal$main["green"], !!!coords_1),
    annotate("rect", alpha = 0.3, fill = pal$main["orange"], !!!coords_2)
  ))
}



# Errors -----------------------------------------------------------------------

#' Diagnostics: Dependence between error segments
#'
#' @param errors [`numeric()`] Vector of error values to be analyzed for
#'   dependence.
#' @param grain [`integer(1)`] Number of segments to divide the errors into for
#'   correlation analysis.
#'
#' @returns [`ggplot`] A heatmap plot visualizing the correlation between error
#'   segments.
#' @export
error_dependence <- function(
  errors, grain = 100,
  simmetric = TRUE, triangular = TRUE
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
      #title = "Error correlation (100-sample segments)",
      x = "Parallel task N°", y = "Parallel task N°",
      fill = "Absolute Correlation"
    )
}

#' Diagnostics: Error distribution visualization
#'
#' @param errors [`numeric()`] Vector of error values to visualize.
#'
#' @returns [`patchwork`] Combined QQ plot and histogram of error distribution.
#' @export
error_distribution <- function(errors) {
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
      #title = "Error distribution (sample of 3000)"
    )
}



# Simulated Data ---------------------------------------------------------------

#' Diagnostics: Visualize simulated series
#'
#' @param data [`data.frame()`] Input data containing simulation results.
#' @param sims [`integer()`] Simulation identifiers to visualize.
#' @param n_burn [`integer(1)`] Number of initial time steps to consider as
#'   burn-in.
#'
#' @returns [`ggplot`] Plot of simulated series colored by regime.
#' @export
series_values <- function(data, sims = 1, n_burn) {
  gdata <- filter(data, sim %in% sims)

  ggplot(gdata, aes(x = t, y = y)) +
    annotate_burn(n_burn) +
    geom_line(
      aes(color = as.factor(r), group = sim),
      alpha = 0.6, linewidth = 1.25
    ) +
    if (length(sims) == 1) annotate_regimes("x")
    facet_grid(vars(sgp), vars(rgp)) +
    scale_color_manual(values = unname(pal$main)) +
    labs(
      #title = "Simulated series - values",
      x = "Time", y = "Value", color = "Regime"
    )
}


#' Diagnostics: Visualize simulated paths
#'
#' @param gdata [`data.frame()`] Input data containing simulation results.
#' @param t_max [`integer(1)`] Maximum time step to visualize.
#' @param sims [`integer(1)`] Simulation identifiers to visualize.
#' @param n_burn [`integer(1)`] Number of initial time steps to consider as
#'   burn-in.
#'
#' @returns [`ggplot`] Plot of simulated paths in regime-value space.
#' @export
series_paths <- function(gdata, t_max = max(gdata$t), sims = 1, n_burn) {
  gdata_segment <- gdata %>%
    filter(sim == sims & t %in% 1:t_max) %>%
    group_by(sgp, rgp) %>%
    mutate(
      across(c(r, y), ~ jitter(.x, amount = 0.2)),
      r_lead = lead(r), y_lead = lead(y), t_lead = lead(t),
      burn = t <= n_burn
    )

  ggplot(gdata_segment, aes(x = r, y = y)) +
    annotate_regimes("x") +
    geom_segment(aes(xend = r_lead, yend = y_lead, color = t), arrow = arrow1) +
    scale_color_viridis_c(
      option = "plasma", end = 0.8, direction = -1,
      breaks = c(1, seq(20, t_max, by = 20))
    ) +
    #ggnewscale::new_scale_color() +
    #geom_text(aes(label = t, y = y + 0.3, color = burn)) +
    #scale_color_manual(values = c("TRUE" = pal$gray["blackgray"], "FALSE" = "#0c0c0c")) +
    geom_point(
      data = filter(gdata_segment, t == 1),
      size = 4, color = pal$main["red"], shape = 18
    ) +
    facet_grid(vars(sgp), vars(rgp)) +
    scale_x_continuous(breaks = 1:2, minor_breaks = NULL) +
    labs(
      #title = "Simulated series - paths",
      x = "Regime", y = "Value", color = "Time:"
    ) +
    theme(legend.position = "bottom")
}


#' Diagnostics: Visualize simulated series distribution
#'
#' @param gdata [`data.frame()`] Input data containing simulation results.
#'
#' @returns [`ggplot`] Density plot of simulated series by regime.
#' @export
series_distribution <- function(gdata) {
  gdata %>%
    ggplot(aes(x = y, color = as.factor(r))) +
    geom_density() +
    facet_grid(sgp ~ rgp, scales = "free") +
    scale_color_manual(values = unname(pal$main)) +
    labs(
      #title = "Simulated series - distribution",
      x = "Value", y = "Density", color = "Regime"
    )
}
# Todo: remove burn-in?


#' Diagnostics: Visualize simulated regime paths
#'
#' @param gdata [`data.frame()`] Input data containing simulation results.
#' @param t_max [`integer(1)`] Maximum time step to visualize.
#' @param sims [`integer(1)`] Simulation identifiers to visualize.
#' @param n_burn [`integer(1)`] Number of initial time steps to consider as
#'   burn-in.
#'
#' @returns [`ggplot`] Plot of simulated regime paths over time.
#' @export
regimes_values <- function(gdata, t_max = max(gdata$t), sims = 1, n_burn) {
  gdata_segment <- gdata %>%
    filter(sim == sims & t %in% 1:t_max) %>%
    group_by(sgp, rgp) %>%
    mutate(
      r_lead = lead(r), y_lead = lead(y), t_lead = lead(t),
      burn = t <= n_burn
    )

  ggplot(gdata_segment, aes(x = t, y = r)) +
    annotate_regimes("y") +
    annotate_burn(n_burn) +
    geom_segment(aes(xend = t_lead, yend = r_lead), arrow = arrow1) +
    facet_grid(vars(sgp), vars(rgp)) +
    scale_y_continuous(breaks = 1:2, minor_breaks = NULL) +
    labs(
      #title = "Simulated series - regimes",
      x = "Time", y = "Regime"
    )
}


#' Diagnostics: Visualize conditional statistics of simulated series
#'
#' @param gdata [`data.frame()`] Input data containing simulation results.
#' @param stats [`function()`] Function to compute statistics on the series.
#' @param sims [`integer(1)`] Simulation identifier to visualize.
#' @param n_burn [`integer(1)`] Number of initial time steps to consider as
#'   burn-in.
#' @param stat_name [`character(1)`] Name of the statistic for labeling.
#'
#' @returns [`ggplot`] Plot of conditional statistics by regime over time.
#' @export
series_stats <- function(gdata, stats, sims = 1, n_burn, stat_name = NULL) {
  gdata_stat <- gdata %>%
    filter(sim == sims) %>%
    group_by(sgp, rgp) %>%
    reframe(
      t = t, r = r,
      map_dfc(unique(r), \(reg) {
        map_dfr(1:max(gdata$t), ~ stats(y[1:.x][r[1:.x] == reg])) %>%
          set_names(glue("{names(.)}_{reg}"))
      })
    ) %>%
    pivot_longer(
      -c(sgp:r),
      names_sep = "_", names_to = c("stat", "regime"), values_to = "value"
    )

  ggplot(gdata_stat, aes(t, value)) +
    geom_rect(
      aes(
        ymin = -Inf, ymax = Inf, xmin = t - 0.5, xmax = t + 0.5,
        fill = as.factor(r)
      ),
      alpha = 0.1
    ) +
    annotate_burn(n_burn) +
    geom_line(aes(color = regime, linetype = stat), linewidth = 1) +
    facet_grid(vars(sgp), vars(rgp)) +
    scale_color_manual(values = unname(pal$main)) +
    scale_fill_manual(values = unname(pal$main)) +
    labs(
      #title = glue("Simulated series - conditional {stat_name}"),
      x = "Time", y = "Value",
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
#' @returns [`ggplot`] Plot of conditional statistics by regime over time.
#' @export
regimes_stats <- function(gdata, stats, sims = 1, n_burn, stat_name = NULL) {
  gdata_stat <- gdata %>%
    filter(sim == sims) %>%
    group_by(sgp, rgp) %>%
    reframe(
      t = t, r = r,
      map_dfr(1:max(gdata$t), ~ stats(r[1:.x]))
    ) %>%
    pivot_longer(
      -c(t, r, sgp, rgp),
      names_to = "stat",
      values_to = "value"
    )

  ggplot(gdata_stat, aes(t, value)) +
    geom_rect(
      aes(
        ymin = -Inf, ymax = Inf, xmin = t - 0.5, xmax = t + 0.5,
        fill = as.factor(r)
      ),
      alpha = 0.1
    ) +
    annotate_burn(n_burn) +
    geom_line(aes(linetype = stat), linewidth = 1) +
    facet_grid(vars(sgp), vars(rgp)) +
    scale_color_manual(values = unname(pal$main)) +
    scale_fill_manual(values = unname(pal$main)) +
    labs(
      #title = glue("Simulated series - regime {stat_name}"),
      x = "Time", y = "Value",
      linetype = "Statistic", fill = "Regime"
    )
}
