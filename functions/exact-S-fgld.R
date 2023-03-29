
inf_series<-function(n, r, s){
  purrr::map2_dbl(r, s, \(r,s){
    h <- 1:1e4
    sum(exp(-log(h) + lgamma(h + r) - lgamma(n + h + 1) + log(digamma(n + h + 1) - digamma(h + s))))
    }
    )
}

int_xi <- function(n, r, s){
  exp(lgamma(n + 1) - lgamma(r) + log(inf_series(n, r, s)))
}

cov_fgld <- function(theta, n, r, s){
  # ifelse(
  #   r == s,
  #   theta[2] ^ 2 * exp(log(r) + log(n - r + 1) - 2*log(n + 1) - log(n + 2)) +
  #     theta[2] * theta[3] * 2 * exp(log(n - r + 1) - 2*log(n + 1)) +
  #     theta[2] * theta[4] * 2 * exp(log(r) - 2*log(n + 1)) +
  #     theta[3] ^ 2 * (trigamma(r) - trigamma(n + 1)) +
  #     theta[3] * theta[4] * 2 * trigamma(n + 1) +
  #     theta[4] ^ 2 * (trigamma(n - r + 1) - trigamma(n + 1)),
  #   theta[2] ^ 2 * exp(log(r) + log(n - s + 1) - 2 * log(n + 1) - log(n + 2)) +
  #     theta[2] * theta[3] * exp(log(n - s + 1) + log(r + s) - 2 * log(n + 1) - log(s)) +
  #     theta[2] * theta[4] * exp(log(r) + log(2 * n - r - s + 2) - 2 * log(n + 1) - log(n - r + 1)) +
  #     theta[3] ^ 2 * (trigamma(s) - trigamma(n + 1)) +
  #     theta[3] * theta[4] * ((digamma(s) - digamma(n + 1)) * (digamma(n - r + 1) - digamma(n + 1)) + trigamma(n + 1) - int_xi(n, r, s)) +
  #     theta[4] ^ 2 * (trigamma(n - r + 1) - trigamma(n + 1))
  # )
  theta[2] ^ 2 * exp(log(r) + log(n - s + 1) - 2 * log(n + 1) - log(n + 2)) +
    theta[2] * theta[3] * exp(log(n - s + 1) + log(r + s) - 2 * log(n + 1) - log(s)) +
    theta[2] * theta[4] * exp(log(r) + log(2 * n - r - s + 2) - 2 * log(n + 1) - log(n - r + 1)) +
    theta[3] ^ 2 * (trigamma(s) - trigamma(n + 1)) +
    theta[3] * theta[4] * 2 * trigamma(n + 1) +
    theta[4] ^ 2 * (trigamma(n - r + 1) - trigamma(n + 1))
}


S_fgld <- function(theta, n){
  j <- sequence(1:n)
  k <- rep(1:n, 1:n)
  cols <- cov_fgld(theta, n, j, k)
  S <- matrix(nrow = n, ncol = n)
  S[upper.tri(S, TRUE)] <- cols
  S[lower.tri(S, FALSE)] <- t(S)[lower.tri(S, FALSE)]
  return(S)
}

