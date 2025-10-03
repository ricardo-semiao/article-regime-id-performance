
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
