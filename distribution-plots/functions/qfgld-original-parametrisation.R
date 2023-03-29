
qfgld_original <- function(u, theta) {
  theta[1] + theta[2] * ((1 - theta[3]) * log(u) - theta[3] * log(1 - u) + theta[4] *
                           u)
}

qfgld_original_deriv <- function(u, theta) {
  ifelse(u >= 1 - 1e-10 | u <= 1e-10,
         1e10,
         theta[2] * ((1 - theta[3]) / (u) + theta[3] / (1 - u) + theta[4]))
}