
#' @export
box::use(
  ./km[km], ./ms[ms], ./nors[nors], ./rf[rf], ./sb[sb], ./set[set], ./st[st]
)

# Parameters always include n_r and n_p. Often include optimization
# parameters such as min_r_size, tol, and max_iter
# All return a generator function enclosing a child of base env carrying the
# hyperparameters, model function, and methods for getting predictions and
# regimes
# Model functions are passed via usual `::`, as box might interact weirdly with
# parallelism

# Results are always a list with elements y, r, and meta
# - y: full series with n_b + n_l + warmup NAs at start. Warmup NAs vary by
# model, but are usually 0 or 1
# - r: regimes, with n_b + n_l + warmup NAs at start. This is already the
# summarized regime, the categorical column-vector, not the full matrix of e.g.
# probabilities
# - meta: list with model-specific information:abstol
#   - Always contains coefs, a matrix with each row being a regime and each
#   column a coefficient, in the order: intercept, lag 1, lag 2, ..., lag n_l.
#   - Also contains switches, the meta information about regime changes (e.g.
#   thresholds or transition matrix)
