library(Matrix)

cov_quad <- function(theta, n, r, s){
  theta[2] ^ 2 * exp(log(r) + log(n - s + 1) - 2 * log(n + 1) - log(n + 2)) +
    theta[2] * theta[3] * exp(log(2) +  log(r) + log(n - s + 1) + log(r + s + 2) - 2 * log(n + 1) - log(n + 2) - log(n + 3)) +
    theta[3] ^ 2 * exp(log(2) + log(r) + log(r + 1) + log(n - s + 1) + log(n * (2 * s + 3) + 5 * s + 6) - 2 * log(n + 1) - 2 * log(n + 2) - log(n + 3) - log(n + 4))
}


# S_quad <- function(theta, n){
#   j <- sequence(1:n)
#   k <- rep(1:n, 1:n)
#   cols <- cov_quad(theta, n, j, k)
#   S <- matrix(nrow = n, ncol = n)
#   S[upper.tri(S, TRUE)] <- cols
#   S[lower.tri(S, FALSE)] <- t(S)[lower.tri(S, FALSE)]
#   return(S)
# }


S_quad <- function(theta, n){
  j <- sequence(1:n)
  k <- rep(1:n, 1:n)
  cols <- cov_quad(theta, n, j, k)
  S <- matrix(nrow = n, ncol = n)
  S[upper.tri(S, TRUE)] <- cols
  return(Matrix::forceSymmetric(S))
}


