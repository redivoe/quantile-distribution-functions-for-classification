
theta2beta <- function(theta) {
  beta <- numeric()
  beta[1] <- theta[1]
  beta[2] <- theta[2] * theta[4]
  beta[3] <- theta[2] * (1 - theta[3])
  beta[4] <- theta[2] * theta[3]
  
  return(beta)
}

# Sample theta (original parametrisation of the distribution) within their (interpretable bounds)
# and transform them to their linear form (beta).

sample_parameters_fgld <- function(B,
                              param_lims = list(alpha = c(-10, 10),
                                                beta = c(1, 5),
                                                delta = c(0, 1))){
  theta <- cbind(
    runif(n = B, min = param_lims$alpha[1], max = param_lims$alpha[2]),
    runif(n = B, min = param_lims$beta[1], max = param_lims$beta[2]),
    runif(n = B, min = param_lims$delta[1], max = param_lims$delta[2]),
    rexp(n = B, rate = 0.5)
  )
  
  beta <- t(apply(theta, 1, theta2beta))
  return(beta)
}

# library(purrr)
# library(ggplot2)
# walk(c("dQ.R", "invQ.R", "distributions.R"), ~ source(here("functions", .x)))
# i <- sample(1000,1)
# ggplot()+
#   stat_function(fun = dQ,
#                 xlim = c(qfgld(u = 0.001, theta = beta[i, ]), qfgld(u = 0.999, theta = beta[i, ])),
#                 n = 1e3,
#                 args = list(theta = beta[i, ], Q = qfgld, Qprime = qfgld_deriv))
