# Setup ------------------------------------------------------------------------

# Loading dependencies:
box::use(
  src/utils[...],
  src/utils2[...],
  src/options[dicts, params],
  ./simulations,
  scales[col_darker],
  ggplot2[...]
)

if (Sys.getenv("RADIAN_VERSION") == "") box::use(gt[...])
# Radian has unicode problems that break gt loading. Run functions that require
# it in RStudio instead


# utils2 ----------------------------------------------------------

# Formerly loaded via ./utils2[...]

#' Helper: Subset and relabel simulation data
#'
#' @param sgps, rgps, sims, models [`character()`] values to filter by. If
#'  `NULL`, no filtering is applied on RGP.
#' @param sims [`numeric()`] Simulation identifiers to filter by. If
#'  `NULL`, no filtering is applied on simulations.
#'
#' @returns [`data.frame()`-like] Filtered and relabeled simulations data.
#' @export
subset_results <- function(
  data,
  sgps = NULL, rgps = NULL, sims = NULL, models = NULL
) {
  has_model <- "model" %in% colnames(data)

  data %>%
    filter(
      if (is_null(sgps)) TRUE else sgp %in% sgps,
      if (is_null(rgps)) TRUE else rgp %in% rgps,
      if (is_null(sims)) TRUE else sim %in% sims,
      if (!has_model || is_null(models)) TRUE else model %in% models
    ) %>%
    mutate(
      sgp = fct_relabel(sgp, ~ dicts$sgps[.x]),
      rgp = fct_relabel(rgp, ~ dicts$rgps[.x]),
      model = if (has_model) fct_relabel(model, ~ dicts$models[.x])
    )
}

#' TODO: document and think where to put
#' @export
plot_sgps_sim <- function(
  data, f,
  sgps, rgp,
  ..., lims = rep(NA_integer_, 2)
) {
  map(sgps, \(sgp) {
    gdata <- subset_results(data, sgps = sgp, rgps = rgp)
    f(gdata, ...)
  }) |>
    reduce(.init = NULL, \(g1, g2) {
      g1 <- g1 & theme(
        legend.position = "none", axis.title.x = element_blank(),
        axis.text.x = element_blank(), axis.ticks.x = element_blank()
        )
      g1 / (g2 + xlim(lims[1], lims[2])) # TODO: make dynamic
    }) +
    plot_annotation(
      caption = glue("SGPs (top to bottom): \u03bc change, \u03c1 change, \u03c3 change")
    )
}

#' TODO: document and think where to put
#' @export
plot_sgps_est <- function(
  data_e, data_s, f,
  sgps, rgp, model,
  ..., lims = rep(NA_integer_, 2)
) {
  map(sgps, \(sgp) {
    gdata_e <- subset_results(data_e, sgps = sgp, rgps = rgp, model = model)
    gdata_s <- subset_results(data_s, sgps = sgp, rgps = rgp)
    f(gdata_e, gdata_s, ...)
  }) |>
    reduce(.init = NULL, \(g1, g2) {
      g1 <- g1 & theme(
        legend.position = "none", axis.title.x = element_blank(),
        axis.text.x = element_blank(), axis.ticks.x = element_blank()
        )
      g1 / (g2 + xlim(lims[1], lims[2])) # Todo: make dynamic
    }) +
    plot_annotation(
      caption = glue("SGPs (top to bottom): \u03bc change, \u03c1 change, \u03c3 change")
    )
}

#' Helper: Get correct parameters from params list
#' TODO: document and think where to put
#' @export
get_correct_params <- function(dgp_names, relabel = TRUE) {
  res <- imap_dfr(params$sgps, \(p, sgp_name) {
    imap_dfr(p$args, \(arg, s) {
      c(regime = s, arg[])
    }) |>
      pivot_longer(-regime, names_to = "coef", values_to = "value") |>
      mutate(sgp = sgp_name, .before = 1)
  }) %>%
    filter(coef %in% c("mu", "rho1"), sgp %in% unique(dgp_names$sgp))

  if (relabel) {
    mutate(res, sgp = fct_relabel(sgp, ~ dicts$sgps[.x]))
  } else {
    res
  }
}



# utils_diagnostics ----------------------------------------------------------

# Formerly loaded via ./utils_diagnostics[...]

# Annotates:

#' Internal: Burn-in period background via annotate
#' @export
annotate_burn <- function(n_b = NA_real_, ...) {
  args_annotate <- list2(...)
  args_annotate$alpha <- args_annotate$alpha %||% 0.5
  args_annotate$fill <- args_annotate$fill %||% "darkgrey"

  inject(list(
    annotate(
      "rect", !!!args_annotate,
      xmin = 1, xmax = as.numeric(n_b), ymin = -Inf, ymax = Inf
    )
  ))
}

#' Internal: Regimes background via annotate
#' @param axis [`character(1)`] Across which axis the regimes vary?
#' @export
annotate_regimes <- function(axis = "x", ...) {
  args_annotate <- list2(...)
  args_annotate$alpha <- args_annotate$alpha %||% 0.3

  coords_order <- switch(axis,
    "x" = c("xmin", "xmax", "ymin", "ymax"),
    "y" = c("ymin", "ymax", "xmin", "xmax")
  )

  coords_1 <- set_names(c(1 - 0.25, 1 + 0.25, -Inf, Inf), coords_order)
  coords_2 <- set_names(c(2 - 0.25, 2 + 0.25, -Inf, Inf), coords_order)

  inject(list(
    annotate("rect", !!!args_annotate, fill = pal$main["green"], !!!coords_1),
    annotate("rect", !!!args_annotate, fill = pal$main["orange"], !!!coords_2)
  ))
}

#' Internal: Prediction horizon background via annotate
#' @export
annotate_pred <- function(n_t, n_h = 0, ...) {
  args_annotate <- list2(...)
  args_annotate$alpha <- args_annotate$alpha %||% 0.3
  args_annotate$fill <- args_annotate$fill %||% "darkgrey"

  inject(list(
    annotate(
      "rect", !!!args_annotate,
      xmin = n_t - n_h + 0.5, xmax = n_t + 0.5, ymin = -Inf, ymax = Inf
    )
  ))
}

#' Internal: Regimes background via geom_rect
#' Assumes x axis is `t` and regime variable is `r`
#' @export
rect_regimes <- function(...) {
  args_rect <- list2(...)
  args_rect$alpha <- args_rect$alpha %||% 0.3

  inject(geom_rect(
    aes(
      ymin = -Inf, ymax = Inf,
      xmin = pmax(1, t - 0.5), xmax = pmin(t + 0.5, max(t)),
      fill = as.factor(r)
    ),
    !!!args_rect
  ))
}



# Conditionals:

#' Internal: Conditional faceting
#' @export
conditional_facet <- function(sgp, rgp, faceted = NULL) {
  faceted <- faceted %||% (
    length(unique(sgp)) > 1 || length(unique(rgp)) > 1
  )

  if (faceted) facet_grid(vars(sgp), vars(rgp))
}

#' Internal: Conditional regimes background
#' @export
conditional_rect <- function(sims, multiple = NULL) {
  multiple <- multiple %||% (length(sims) > 1)

  if (!multiple & length(sims) == 1) rect_regimes()
}

#' Internal: Conditional color scale
#' @export
conditional_color <- function(regime_aligned, ...) {
  if (regime_aligned) {
    scale_color_manual(values = unname(pal$main), ...)
  } else {
    scale_color_viridis_d(option = "plasma", end = 0.8, ...)
  }
}



# Paths ----------------------------------------------------------

#' Diagnostics: Visualize simulated paths
#'
#' @param gdata [`data.frame()`] Input data containing simulation results.
#' @param t_max [`integer(1)`] Maximum time step to visualize.
#' @param sims [`integer(1)`] Simulation identifiers to visualize.
#' @param n_b [`integer(1)`] Number of initial time steps to consider as
#'   burn-in.
#'
#' @returns [`ggplot()`] Plot of simulated paths in regime-value space.
series_paths <- function(
  data, t_max = max(data$t), sims = 1,
  n_b, faceted = NULL
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
      burn = t <= n_b
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
#' @param n_b [`integer(1)`] Number of initial time steps to consider as
#'   burn-in.
#'
#' @returns [`ggplot()`] Plot of simulated regime paths over time.
regimes_values <- function(
  data, t_max = max(data$t), sims = 1,
  n_b, faceted = NULL
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
    annotate_burn(n_b) +
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



# Estimations ----------------------------------------------------------


#' Internal: Get error data by joining simulation and estimation results
get_error_data <- function(data_s, data_e, regime_aligned) {
  left_join(
    rename(data_e, y_est = y, r_est = r),
    rename(data_s, y_true = y, r_true = r),
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



# Values:

#' TODO: document
#' @export
series_values <- function(
  data_s, data_e,
  sims = 1, n_b = NA, n_t = length(unique(data_e$t)), n_h = 0,
  multiple = TRUE, hline = NULL, title = NULL, faceted = NULL
) {
  args <- list(
    sims = sims, n_b = n_b, multiple = multiple, hline = hline,
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

#' TODO: document
#' @export
series_distribution <- function(
  data_s, data_e,
  n_b = NA, hline = NULL, title = NULL, faceted = NULL
) {
  args <- list(
    n_b = n_b, hline = hline, title = title, faceted = faceted
  )

  g_s <- inject(simulations$series_distribution(data_s, !!!args))
  g_e <- inject(simulations$series_distribution(data_e, !!!args))

  (g_s - g_e) +
    plot_layout(ncol = 1, guides = "collect", axes = "collect_y")
}

#' TODO: document
#' @export
panel_estimations <- function(
  data_s, data_e,
  n_b = NA, n_t = length(unique(data_e$t)), n_h = 0,
  title = NULL
) {
  g_values <- series_values(
    data_s, data_e,
    sims = 1, n_b = n_b, n_t = n_t, n_h = n_h
  ) &
    theme(legend.position = "none")
  g_distribution <- series_distribution(
    data_s, data_e,
    n_b = n_b
  ) &
    geom_line(aes(NA_real_, NA_real_, color = as.factor(r)), linewidth = 1) &
    theme(legend.position = "bottom")

  y_lims <- range(
    c(data_s$y, data_e$y),
    na.rm = TRUE, finite = TRUE
  )
  
  g <- c(as.list(g_values)[], as.list(g_distribution)[]) |>
    wrap_plots(
      byrow = FALSE, guides = "collect", axes = "collect", design = "112"
    ) &
    ylim(y_lims[1] * 1.1, y_lims[2] * 1.1) &
    conditional_color(TRUE, na.translate = FALSE)
  g +
    plot_annotation(
      title = title,
      theme = theme(legend.position = "bottom")
    )
}



# Residuals:

#' TODO: document
#' @export
residuals_values <- function(
  data_s, data_e, regime_aligned = TRUE,
  sims = 1, n_b = NA, n_t = length(unique(data_e$t)), n_h = 0,
  multiple = TRUE, hline = NULL, title = NULL, faceted = NULL
) {
  args <- list(
    sims = sims, n_b = n_b, multiple = multiple, hline = hline,
    title = title, faceted = faceted
  )

  data <- get_error_data(data_s, data_e, regime_aligned)

  inject(simulations$series_values(data, !!!args)) +
    annotate_pred(n_t, n_h) +
    conditional_color(regime_aligned, na.translate = FALSE)
}

#' TODO: document
#' @export
residuals_distribution <- function(
  data_s, data_e, regime_aligned = TRUE,
  n_b = NA, hline = 0, title = NULL, faceted = NULL
) {
  args <- list(
    n_b = n_b, hline = hline, title = title, faceted = faceted
  )

  data <- get_error_data(data_s, data_e, regime_aligned)

  inject(simulations$series_distribution(data, !!!args)) +
    conditional_color(regime_aligned, na.translate = FALSE)
}

#' TODO: document
#' @export
panel_residuals <- function(
  data_s, data_e, regime_aligned = TRUE,
  n_b = NA, n_t = length(unique(data_e$t)), n_h = 0,
  hline = NULL, title = NULL
) {
  g_values <- residuals_values(
    data_s, data_e, regime_aligned = regime_aligned,
    sims = 1, n_b = n_b, n_t = n_t, n_h = n_h, hline = hline
  ) +
    theme(legend.position = "none")
  g_distribution <- residuals_distribution(
    data_s, data_e, regime_aligned = regime_aligned, n_b = n_b
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
      nrow = 1, guides = "collect", axes = "collect_y", design = "112"
    ) +
    plot_annotation(
      title = title,
      #subtitle = glue("SGP: {unique(data$sgp)}\nRGP: {unique(data$rgp)}"),
      theme = theme(legend.position = "bottom")
    )
}



# Coefficients:

#' TODO: document
#' @export
coefs_distribution <- function(
  data, params, model_names,
  models = unique(model_names$model),
  lims = list(mu = c(NA, NA), rho1 = c(NA, NA)), title = NULL
) {
  correct <- get_correct_params(model_names)

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
    ) |>
    mutate(regime = c(2, 1)[regime])

  ggplot(gdata, aes(x = value, color = as.factor(regime))) +
    geom_density() +
    geom_vline(
      aes(xintercept = value, color = as.factor(regime)), correct,
      linetype = "dashed"
    ) +
    ggh4x::facet_grid2(vars(sgp), vars(coef), scales = "free", independent = "y") +
    xlim(-4, 4) +
    scale_color_manual(values = unname(pal$main)) +
    labs(
      title = title, x = "Value", y = "Density",
      color = "Regime", fill = "Regime"
    )
}



# Residuals Table:

#' TODO: document
#' @export
table_residuals <- function(
  data_s, data_e, regime_aligned = TRUE,
  dgps = NULL, ..., n_b = n_b
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
    filter(t > n_b + 2) # 2 is the warmup. For most models, it is n_l + 1. TODO: generalize
  
  if (!is_null(dgps)) {
    data <- filter(data, str_c(sgp, rgp, sep = "-") %in% dgps)
  }
  n_r <- length(unique(data$r))

  # Metrics and ANOVA:
  data_metrics <- data |> 
    group_by(sgp, rgp, sim) %>%
    reframe(
      sgp_metric = metrics$series_avg(y, r, ...),
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



# Simulations ----------------------------------------------------------

# Series Values and Distribution:

#' Diagnostics - simulations: Visualize simulated series' values
#' @param multiple [`logical(1)`] Here, this argument only controls whether to
#'  include regimes background or not.
#' @export
series_values <- function(
  data, sims = 1, n_b = NA, multiple = TRUE, hline = NULL,
  title = NULL, faceted = NULL
) {
  gdata <- filter(data, sim %in% sims)

  ggplot(gdata, aes(x = t, y = y)) +
    conditional_rect(sims, multiple = multiple) +
    annotate_burn(n_b) +
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
  data, n_b = 0,
  hline = NULL, title = NULL, faceted = NULL
) {
  data %>%
    filter(t > n_b) %>%
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
  data, n_b = NA, title = NULL
) {
  g_values <- series_values(data, sims = 1, n_b = n_b) +
    theme(legend.direction = "horizontal")
  g_distribution <- series_distribution(data, n_b = n_b) +
    theme(legend.position = "none")

  y_lims <- range(filter(data)$y, na.rm = TRUE, finite = TRUE)

  (
    g_values - g_distribution &
      ylim(y_lims[1] * 1.1, y_lims[2] * 1.1) 
  ) +
    plot_layout(
      nrow = 1, guides = "collect", axes = "collect_y", design = "112"
    ) +
    plot_annotation(
      title = title,
      subtitle = glue("SGP: {unique(data$sgp)}\nRGP: {unique(data$rgp)}"),
      theme = theme(legend.position = "bottom")
    )
}
# TODO: allow aesthetics customization; standardize y_lims betweem sims and also
# between panels



# Metrics Values:

#' Diagnostics - metrics: Series or regimes statistics accumulated over time
#' @param stats [`function()`] Function to compute statistics. It should have a
#'  Similar signature to those in `rgp_metrics()` or `sgp_metrics()`.
#' @param regime_aligned [`logical(1)`] Whether the statistics map to regimes,
#'  and thus so should their line colors, or not (the default).
#' @export
stats_accumulated <- function(
  data, stats,
  sims = 1, n_b = NA, multiple = length(sims) > 1,
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
      }) |>
        `colnames<-`(c("1", "2")),
      t = 1:max(data$t), r = if (length(sims) == 1) r else NA
    ) %>%
    pivot_longer(
      -c(sgp, rgp, sim, t, r),
      names_to = "stat", values_to = "value"
    )

  ggplot(gdata, aes(t, value)) +
    conditional_rect(sims, multiple) +
    annotate_burn(n_b) +
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
  data, stats, sims = 1, n_b = NA,
  title = NULL, faceted = NULL, regime_aligned = FALSE
) {
  gdata <- data %>%
    group_by(sgp, rgp, sim) %>%
    reframe(
      map_dfc(stats(y = y, r = r, n_r = length(unique(r))), ~ .x) |>
        `colnames<-`(c("1", "2"))
    ) %>%
    pivot_longer(
      -c(sgp, rgp, sim),
      names_to = "stat", values_to = "value"
    )

  ggplot(gdata, aes(y = value)) +
    geom_density(aes(color = stat), linewidth = 1) +
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
  sims = 1, n_b = NA, regime_aligned, title = NULL
) {
  stats <- stats %||% switch(dimension,
    "sgp" = \(y, r, n_r) sgp_metric(option, y, r, n_r),
    "rgp" = \(y, r, n_r) rgp_metric(option, y, r, n_r),
    cli_abort("{.arg dimension} must be one of {.val 'sgp'} or {.val 'rgp'}.")
  )

  g_accumulated <- stats_accumulated(
    data, stats, sims, n_b, regime_aligned = regime_aligned
  ) +
    theme(legend.direction = "horizontal")

  g_distribution <- stats_density(
    data, stats, n_b, regime_aligned = regime_aligned
  ) +
    theme(legend.position = "none")

  y_lims <- range(g_accumulated$data$value, na.rm = TRUE, finite = TRUE)

  (
    g_accumulated - g_distribution &
      ylim(y_lims[1] * 1.0, y_lims[2] * 1.0) 
  ) +
    plot_layout(
      nrow = 1, guides = "collect", axes = "collect_y", design = "112"
    ) +
    plot_annotation(
      title = title,
      subtitle = glue("SGP: {unique(data$sgp)}\nRGP: {unique(data$rgp)}"),
      theme = theme(legend.position = "bottom")
    )
}



# Metrics  Tables:

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
      pvalue = t.test(sgp_metric ~ r)[["p.value"]]
    )
  # TODO: consider `anova(lm(sgp_metric ~ r))[["Pr(>F)"]][1]`

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
