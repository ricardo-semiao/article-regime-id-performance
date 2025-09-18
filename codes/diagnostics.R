
# Helpers ----------------------------------------------------------------------

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



# Errors -----------------------------------------------------------------------

diagnose_dependence <- function(errors, parallelGrain = 100) {
  error_cor <- errors |>
    matrix(nrow = (n_t * n_p * n_s) / parallelGrain, ncol = parallelGrain) |>
    cor()

  tibble(
    x = rep(1:100, 100),
    y = rep(1:100, each = 100),
    cor = as.numeric(error_cor)
  ) |>
    filter(x != y) |>
    ggplot(aes(x, y, fill = cor)) +
    geom_tile() +
    scale_fill_viridis_c(option = "plasma") +
    coord_equal() +
    labs(
      #title = "Error correlation (100-sample segments)",
      x = "Parallel task N°", y = "Parallel task N°", fill = "Correlation"
    )
}

diagnose_errors <- function(errors) {
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
    labs(
      #title = "Histogram",
      x = "Value", y = "Density"
    )

  ((g_hist - g_qq) * theme(plot.title = element_text(size = 12))) +
    plot_annotation(
      #title = "Error distribution (sample of 3000)"
    )
}



# Simulated Data ---------------------------------------------------------------

diagnose_series <- function(gdata, only_one = FALSE, chosen_sim = 1) {
  if (only_one) gdata <- gdata %>% filter(sim == chosen_sim)

  ggplot(gdata, aes(x = t, y = y)) +
    annotate(
      "rect", fill = "darkgrey", alpha = 0.5,
      xmin = 1, xmax = n_burn, ymin = -Inf, ymax = Inf
    ) +
    geom_line(
      aes(color = as.factor(r), group = sim),
      alpha = 0.6, linewidth = 1.25
    ) +
    facet_grid(vars(sgp), vars(rgp)) +
    scale_color_manual(values = unname(pal$main)) +
    labs(
      #title = "Simulated series - values",
      x = "Time", y = "Value", color = "Regime"
    )
}


diagnose_paths <- function(gdata, t_max = 30, chosen_sim = 1) {
  gdata_segment <- gdata %>%
    filter(sim == chosen_sim & t %in% 1:t_max) %>%
    group_by(sgp, rgp) %>%
    mutate(
      across(c(r, y), ~ jitter(.x, amount = 0.2)),
      r_lead = lead(r), y_lead = lead(y), t_lead = lead(t),
      burn = t <= n_burn
    )

  ggplot(gdata_segment, aes(x = r, y = y)) +
    annotate(
      "rect", alpha = 0.3, fill = pal$main["green"],
      xmin = 1 - 0.25, xmax = 1 + 0.25, ymin = -Inf, ymax = Inf
    ) +
    annotate(
      "rect", alpha = 0.3, fill = pal$main["orange"],
      xmin = 2 - 0.25, xmax = 2 + 0.25, ymin = -Inf, ymax = Inf
    ) +
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


diagnose_regimes <- function(gdata, t_max = n_t, chosen_sim = 1) {
  gdata_segment <- gdata %>%
    filter(sim == chosen_sim & t %in% 1:t_max) %>%
    group_by(sgp, rgp) %>%
    mutate(
      r_lead = lead(r), y_lead = lead(y), t_lead = lead(t),
      burn = t <= n_burn
    )

  ggplot(gdata_segment, aes(x = t, y = r)) +
    annotate(
      "rect", alpha = 0.3, fill = pal$main["green"],
      ymin = 1 - 0.25, ymax = 1 + 0.25, xmin = -Inf, xmax = Inf
    ) +
    annotate(
      "rect", alpha = 0.3, fill = pal$main["orange"],
      ymin = 2 - 0.25, ymax = 2 + 0.25, xmin = -Inf, xmax = Inf
    ) +
    annotate(
      "rect", fill = "darkgrey", alpha = 0.5,
      xmin = 1, xmax = n_burn, ymin = -Inf, ymax = Inf
    ) +
    geom_segment(aes(xend = t_lead, yend = r_lead), arrow = arrow1) +
    facet_grid(vars(sgp), vars(rgp)) +
    scale_y_continuous(breaks = 1:2, minor_breaks = NULL) +
    labs(
      #title = "Simulated series - regimes",
      x = "Time", y = "Regime"
    )
}


diagnose_stat_y <- function(gdata, stat, stat_name, chosen_sim = 1) {
  gdata_stat <- gdata %>%
    filter(sim == chosen_sim) %>%
    group_by(sgp, rgp) %>%
    reframe(
      t = t, r = r,
      map_dfc(unique(r), \(reg) {
        map_dfr(1:n_t, ~ stat(y[1:.x][r[1:.x] == reg])) %>%
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

diagnose_stat_r <- function(gdata, stat, stat_name, chosen_sim = 1) {
  gdata_stat <- gdata %>%
    filter(sim == chosen_sim) %>%
    group_by(sgp, rgp) %>%
    reframe(
      t = 1:n_t,
      map_dfr(1:n_t, ~ stat(r[1:.x])),
      r = as.character(r)
    ) %>%
    pivot_longer(
      -c(t, r, sgp, rgp),
      names_to = "stat",
      values_to = "value"
    )

  ggplot(gdata_stat, aes(t, value)) +
    geom_rect(
      aes(ymin = -Inf, ymax = Inf, xmin = t - 0.5, xmax = t + 0.5, fill = r),
      alpha = 0.1
    ) +
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
