
sample_parameters_quad <- function(B){
  
  theta0 <- runif(n = B, -5, 5)
  theta2 <- runif(n = B, -20, 20)
  theta1_lb <- pmax(0, -2 * theta2)
  theta1 <- runif(n = B, min = theta1_lb, max = theta1_lb + 20)
  
  return(cbind(theta0, theta1, theta2))
}



# library(purrr)
# library(ggplot2)
# walk(c("dQ.R", "invQ.R", "distributions.R"), ~ source(here("functions", .x)))
# 
# B <- 1000
# theta <- sample_parameters_quad(B)
# i <- sample(B, 1)
# 
# ggplot()+
#   stat_function(fun = dQ,
#                 xlim = c(qquad(u = 0.001, theta = theta[i, ]), qquad(u = 0.999, theta = theta[i, ])),
#                 n = 1e3,
#                 args = list(theta = theta[i, ], Q = qquad, Qprime = qquad_deriv))
# 
# ggplot()+
#   stat_function(fun = qquad,
#                 xlim = c(0, 1),
#                 n = 1e3,
#                 args = list(theta = theta[i, ]))
# 
# ggplot()+
#   stat_function(fun = qquad_deriv,
#                 xlim = c(1e-3, 1-1e-3),
#                 n = 1e3,
#                 args = list(theta = theta[i, ]))

