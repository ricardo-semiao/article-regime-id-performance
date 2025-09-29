mean_pairwise_dist <- function(x) {
  mean(dist(x))
}


rmse <- function(y, y_true, n_h, n_t) {
  sqrt(sum((y[(n_t - n_h + 1):n_t] - y_true[(n_t - n_h + 1):n_t])^2) / length(n_h))
}


average <- function(y, r, n_r_hat) {
  vapply(1:n_r_hat, FUN.VALUE = numeric(1), FUN = \(s) {
    mean(y[r == s])
  })
}

autocorr <- function(y, r, n_r_hat, n = 1) {
  vapply(1:n_r_hat, FUN.VALUE = numeric(1), FUN = \(s) {
    cor(y[r == s][-(1:n)], lag(y[r == s], n)[-(1:n)])
  })
}

sign_prop <- function(y, r, n_r_hat) {
  vapply(1:n_r_hat, FUN.VALUE = numeric(1), FUN = \(s) {
    mean(diff(y[r == s]) >= 0)
  })
}

volatility <- function(y, r, n_r_hat) {
  vapply(1:n_r_hat, FUN.VALUE = numeric(1), FUN = \(s) {
    sd(y[r == s])
  })
}


average_duration <- function(y, r, n_r_hat) {
  vapply(1:n_r_hat, FUN.VALUE = numeric(1), FUN = \(s) {
    mean(table(cumsum(abs(c(0, diff(r))))[r == s]))
  })
}
# Only for 2 regimes

n_instances <- function(y, r, n_r_hat) {
  vapply(1:n_r_hat, FUN.VALUE = numeric(1), FUN = \(s) {
    (c(0, diff(r)) > 0)[r == s]
  })
}
# Only for 2 regimes
