library(tidyverse)
library(yardstick)
library(here)

my_functions<-c("invQ.R",
                "dQ.R",
                "distributions.R",
                "fitting-functions.R",
                "exact-S-fgld.R",
                "sampling-fgld-parameters.R",
                "exact-S-quad.R",
                "sampling-quad-parameters.R")
walk(my_functions, ~ source(here("functions", .x)))


distrs <- c("fgld", "quad")
auc <- list()

for(i in seq_along(distrs)){
  distr <- distrs[i]
  
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
  
  
  # sample size for each group (balanced dataset)
  n <- c(20, 50, 100, 200)
  
  # number of variables
  p <- 1e3
  
  # proportion of variables that are equal between the two groups
  prop_diff <- 0.5
  p_diff <- prop_diff * p
  p_nodiff <- p - p_diff
  # set of parameters needed
  # p_nodiff + 2*p_diff = p - p_diff + 2*p_diff = p + p_diff
  n_param <- p + p_diff
  
  # sampling the parameters
  set.seed(15)
  
  beta <- sample_parameters(B = n_param)
  
  # separating the parameters for the variables
  beta_nodiff <- beta[1:p_nodiff,] %>% array_tree(margin = 1)
  
  beta_diff <- array(dim = c(p_diff, degrees_of_freedom, 2))
  temp <- beta[-c(1:p_nodiff),]
  beta_diff[,,1] <- temp[1:(nrow(temp)/2),]
  beta_diff[,,2] <- temp[-c(1:(nrow(temp)/2)),]
  beta_diff <- beta_diff %>% array_tree(margin = c(1,3))
  
  pvalues <- list()
  
  for(ni in seq_along(n)){
    
    y_nodiff <- map(beta_nodiff, \(b_i) rerun(2, q(runif(n[ni]), b_i)))
    y_diff <- map_depth(beta_diff, .depth = 2, \(b_i) q(runif(n[ni]), b_i))
    y <- list(y_nodiff, y_diff)
    
    b <- map_depth(.x = y, .depth = 3, .f = fit_fun)
    
    B <- get_X(n[ni])
    A <- B%*%solve(crossprod(B))
    Gamma <- map_depth(.x = b,.depth = 3,.f = \(b) t(A)%*%get_S(b, n = n[ni])%*%A)
    
    distances <- map2_dbl(flatten(b), flatten(Gamma), ~ c(as.matrix((.x[[1]] - .x[[2]])%*%solve(.y[[1]] + .y[[2]])%*%(.x[[1]] - .x[[2]]))))
    pvalues[[ni]] <- map_dbl(distances, pchisq, df = degrees_of_freedom, lower.tail = FALSE)
  }
  
  pvalues <- set_names(pvalues, n)
  
  truth <- factor(rep(0:1, times = c(p_nodiff, p_diff)))
  
  # AUCs
  auc[[i]] <- map(pvalues, ~ roc_auc_vec(truth = truth,estimate = .x))
  
  tibble(n = n) %>% 
    mutate(roc = map(pvalues, ~ roc_curve(tibble(truth = truth, estimate = .x), truth, estimate))) %>% 
    unnest(cols = roc) %>% 
    ggplot(aes(x = 1 - specificity, y = sensitivity, col = factor(n, ordered = TRUE))) +
    geom_path() +
    geom_abline(lty = 3) +
    scale_color_viridis_d(option = "G", end = 0.8, name = "n",alpha = 0.7) +
    coord_equal() +
    theme_bw()
  
  ggsave(here("output", str_c("roc-test-", distr, ".pdf")), width = 5.5, height = 5)
}

# save.image(here("output", "sim-test-power.RData"))

