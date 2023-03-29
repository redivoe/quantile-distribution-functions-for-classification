library(here)
library(tidyverse)
library(future.apply)
library(Matrix)

my_functions<-c("invQ.R",
                "dQ.R",
                "distributions.R",
                "fitting-functions.R",
                "exact-S-fgld.R",
                "sampling-fgld-parameters.R",
                "exact-S-quad.R",
                "sampling-quad-parameters.R")
walk(my_functions, ~ source(here("functions", .x)))


# uncomment chosen distribution

# distr <- "quad"
distr <- "fgld"

if(distr == "fgld"){
  degrees_of_freedom <- 4
  q <- qfgld
  fit_fun <- fit_fgld_ls
  sample_parameters <- sample_parameters_fgld
  get_S <- S_fgld
  get_X <- get_X_fgld
}else if(distr == "quad"){
  degrees_of_freedom <- 3
  q <- qquad
  fit_fun <- fit_quad_ls
  sample_parameters <- sample_parameters_quad
  get_S <- S_quad
  get_X <- get_X_quad
}


n <- c(50, 100, 200, 500, 1000)
B1 <- 1e3
B2 <- 200

set.seed(211210)
beta <- sample_parameters(B = B2)
# Creating a list for beta (real parameters) for using map functions
beta_list <- array_tree(beta, 1)

plan(strategy = multisession(workers = 6))

distances <- list()
typeones <- list()

B2_blocks <- rep(1:B2, each = B1)
criticalvalue <- qchisq(0.95, df = degrees_of_freedom)

get_G <- function(theta) {
  t(A) %*% get_S_n(theta) %*% A
}

for(ni in 1:length(n)){
  
  y <- purrr::map(beta_list, \(b_i) rerun(B1, rerun(2, q(runif(n[ni]), b_i))))
  y_flat <- flatten(flatten(y))
  
  b <- future_lapply(y_flat, fit_fun, future.scheduling = 1)
  
  B <- get_X(n[ni]) |> 
    as(Class = "dgeMatrix")
  A <- B %*% solve(crossprod(B))
  get_S_n <- purrr::partial(get_S, n = n[ni])
  
  Gamma <- future_lapply(b, \(b_i) t(A) %*% get_S(b_i, n[ni]) %*% A)
  
  nested_b <- list()
  nested_Gamma <- list()
  for(i in 1:(B2*B1)){
    nested_b[[i]] <- list(b[[2*i-1]], b[[(2*i)]])
    nested_Gamma[[i]] <- list(Gamma[[2*i-1]],Gamma[[(2*i)]])
  }
  
  distances[[ni]] <- map2_dbl(nested_b,
                              nested_Gamma,
                              ~ c(as.matrix(t(.x[[1]] - .x[[2]])%*%solve(.y[[1]] + .y[[2]])%*%(.x[[1]] - .x[[2]]))))
  
  typeones[[ni]] <- tapply(X = distances[[ni]], INDEX = B2_blocks,FUN = \(x) mean(x > criticalvalue))
  
  cat(ni,"\n")
}


# Alternative commands
# No flattening but not immediately parallelizable

# b <- map_depth(y, 3, fit_quad_ls)
# Gamma <- map_depth(b, 3, approx_G)
# distances[[ni]] <- map2_dbl(flatten(b), flatten(Gamma), ~ (.x[[1]] - .x[[2]])%*%solve(.y[[1]] + .y[[2]])%*%(.x[[1]] - .x[[2]]))

rm(list = setdiff(ls(), c("typeones", "distances", "n", "B1", "B2", "distr", "beta_list", "beta", "critical_value")))
save.image(file = here("output", str_c("sim-test-typeone-", distr, ".RData")))


