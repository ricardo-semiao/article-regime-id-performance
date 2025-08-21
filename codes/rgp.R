regime_markov <- function(n_r, probs) {
  \(n_t) {
    r <- integer(n_t)
    r[1] <- sample(1:n_r, 1)

    for (t in 2:n_t) {
      r[t] <- sample(1:n_r, 1, prob = probs[r[t - 1], ])
    }

    r
  }
}

markov_simmat <- function(persistance, n_r) {
  mat <- matrix((1 - persistance) / (n_r - 1), n_r, n_r)
  diag(mat) <- persistance
  mat
}

regime_binom <- function(n_r, probs) {
  \(n_t) sample(1:n_r, n_t, replace = TRUE, probs)
}
