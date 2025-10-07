
# Setup ------------------------------------------------------------------------

# Loading dependencies:
box::use(
  src/utils[...]
)


# Modules for helpers:
box::use(
  src/options[dicts, params]
)

# These utility functions depend on `dicts` and `params`, and thus must be
# defined after loading `src/utils`, which `src/options` depends on



# General Helpers --------------------------------------------------------------

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

#' Helper: Order regimes by a varying parameter
#' Todo: document and think where to put
#' @export
regimes_order <- function(coefs, sgp_name) {
  param_varying <- params$sgp[[sgp_name]]$args |>
    list_transpose() |>
    map_lgl(~ length(unique(.x)) != 1) |>
    which()

  col <- case_when(
    grepl("^rho[0-9]+$", names(param_varying)) ~ 2, # if any rho varies, use rho1
    "mu" == names(param_varying) ~ 1,
    "vol" == names(param_varying) ~ 2, # vol is not estimated, doesn't matter
    TRUE ~ NA
  )
  if (is.na(col)) cli_abort("Varying parameter {.val {names(param_varying)}} \\
  recognized.")

  order(coefs[, col], decreasing = TRUE)
}

#' Helper: Get correct parameters from params list
#' Todo: document and think where to put
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



# Combined Plots Helpers -------------------------------------------------------

#' Todo: document and think where to put
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
      g1 / (g2 + xlim(lims[1], lims[2])) # Todo: make dynamic
    }) +
    plot_annotation(
      caption = glue("SGPs (top to bottom): \u03bc change, \u03c1 change, \u03c3 change")
    )
}

#' Todo: document and think where to put
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
