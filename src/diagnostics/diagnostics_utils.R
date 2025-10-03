
# Setup ------------------------------------------------------------------------

# Loading dependencies
#' @export
box::use(
  src/utils[...],
  src/others/metrics,
  src/options/sgps[sgp_names = options_names],
  src/options/rgps[rgp_names = options_names]
)

# Graphing functions often include a title argument that suppresses the title
#  by default. Similarly, the faceted argument removes faceting, and by default
#  does it if there is no variation in both faceting variables



# Helpers ----------------------------------------------------------------------

#' Internal: Burn-in period background via annotate
#' @export
annotate_burn <- function(n_burn = NA_real_, ...) {
  args_annotate <- list2(...)
  args_annotate$alpha <- args_annotate$alpha %||% 0.5
  args_annotate$fill <- args_annotate$fill %||% "darkgrey"

  inject(list(
    annotate(
      "rect", !!!args_annotate,
      xmin = 1, xmax = as.numeric(n_burn), ymin = -Inf, ymax = Inf
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
conditional_color <- function(regime_aligned) {
  if (regime_aligned) {
    scale_color_manual(values = unname(pal$main))
  } else {
    scale_color_viridis_d(option = "plasma", end = 0.8)
  }
}
