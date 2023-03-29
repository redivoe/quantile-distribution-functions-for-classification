
qquad <- function(u, theta) {
  theta[1] + theta[2] * u + theta[3] * u^2
}

qquad_deriv <- function(u, theta) {
  ifelse(u >= 1 - 1e-10 | u <= 1e-10,
         1e10,
         theta[2] + 2 * u * theta[3])
}