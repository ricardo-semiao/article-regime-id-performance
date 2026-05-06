
# Setup ------------------------------------------------------------------------

box::use(
  src/utils[...],
  src/options[dicts],
  ggplot2[...],
  gt[...],
  patchwork[...]
)


# Temporary example
if (FALSE) {
  errors = errors_raw; data_s = simulations_data
  sample_size = 3000; sd = 1; title = NULL
}


# Dependence and Distribution --------------------------------------------------

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
error_dependence <- function(
  errors, grain = 100,
  simmetric = TRUE, triangular = TRUE, title = NULL
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
      title = title, x = "Parallel task N°", y = "Parallel task N°",
      fill = "Absolute\nCorrelation"
    )
}

#' Diagnostics - errors: Error distribution
#'
#' @param errors [`numeric()`]
#'
#' @returns [`patchwork`] Combined QQ plot and histogram of error distribution.
#' @export
error_distribution_sim <- function(
  errors, sample_size = 3000, sd = 1,
  title = NULL
) {
  g_qq <- ggplot(tibble(x = sample(errors, sample_size))) +
    geom_abline(
      slope = 1, intercept = 0,
      color = pal$main["orange"], linewidth = 1
    ) +
    geom_qq(aes(sample = x), alpha = 0.3, color = pal$gray["blackgray"]) +
    coord_equal() +
    labs(title = "A - QQ plot", x = "Theoretical Quantiles", y = "Sample Quantiles")

  g_hist <- ggplot(tibble(x = sample(errors, sample_size))) +
    geom_histogram(
      aes(x, after_stat(density)), bins = 70,
      fill = pal$gray["blackgray"], color = "black"
    ) +
    stat_function(
      fun = dnorm, args = list(mean = 0, sd = sd),
      color = pal$main["orange"], linewidth = 1
    ) +
    xlim(-4 * sd, 4 * sd) +
    labs(title = "B - Histogram", x = "Value", y = "Density")

  ((g_hist - g_qq) * theme(plot.title = element_text(size = 12))) +
    plot_annotation(
      title = title,
      caption = glue("Sample of {sample_size} errors from total of \\
      {formatC(length(errors), format = 'd', big.mark = ',')}.")
    )
}



# Regimes Proportions ----------------------------------------------------------

#' @export
regimes_proportions_sim <- function(data_s, n = 2) {
  data <- data_s |>
    group_by(rgp, sgp) |>
    summarise(
      prop_sgp = {
        tab <- tabulate(r)
        max(tab) / sum(tab) - 0.5
      }
    ) |>
    group_by(rgp) |>
    mutate(prop_base = mean(prop_sgp)) |>
    ungroup() |>
    pivot_wider(names_from = sgp, values_from = prop_sgp)

  data |>
    mutate(
      rgp = dicts$rgps$gt_param[rgp],
      across(-rgp, ~ fmt_decimal(.x, n, trail_0 = TRUE))
    ) |>
    rename_with(~ dicts$sgps$gt_param[.x], .cols = -c(rgp, prop_base)) |>
    rename("RGP" = rgp, "Uncond." = prop_base) |>
    gt(rowname_col = "RGP", groupname_col = NULL) |>
    cols_label_with(fn = md) |>
    fmt_markdown(RGP)
}
