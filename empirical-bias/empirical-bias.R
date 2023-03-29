library(tidyverse)
library(here)
library(xtable)

foos <- c("distributions.R",
          "fitting-functions.R")
purrr::walk(foos, \(x) source(here("..", "functions", x)))

gen_data <- function(distr, n, B){
  u <- replicate(B, runif(n))
  X <- switch(
    distr,
    norm = qnorm(u),
    t = qt(u, df = 3),
    logabst = log(abs(qt(u, df = 3))),
    ilogabst = log(abs(qt(u, df = 3))),
    exp = qexp(u, rate = 0.5)
  )
  return(X)
}

empirical_bias <- function(obs, exp){
  if (length(obs) != length(exp)) {
    stop("Different lengths")
  }else{
    n <- length(obs)
  }
  
  sqrt(sum((obs - exp)^2)/n)
}


scenario <- c("norm", "t", "logabst", "ilogabst","exp")

n <- 100
B <- 100

data <- map(scenario, gen_data, n, B) |> 
  map(\(x) array_branch(x, margin = 2)) |> 
  set_names(scenario)
sorted_data <- map_depth(data, 2, sort)


models <- c("lin", "quad", "fgld")

out <- list()
for(i in seq_along(models)){
  model <- models[i]
  
  X <- switch(model,
              "fgld" = get_X_fgld(n),
              "quad" = get_X_quad(n),
              "lin" = get_X_lin(n))
  
  fit_fun <- switch(model,
                    "fgld" = fit_fgld_ls,
                    "quad" = fit_quad_ls,
                    "lin" = fit_lin_ls)
  
  
  out[[i]] <- map_depth(sorted_data, 2, \(x) empirical_bias(x, X %*% fit_fun(x))) |>
    map(\(x) reduce(x, c)) |> 
    imap_dfr(\(x, y) tibble("distr" = y, "mean" = mean(x), "sd" = sd(x)))
}

out <- set_names(out, models) |> 
  imap(\(x, y) mutate(x, model = y)) |> 
  reduce(bind_rows)



out |> 
  transmute(distr, model,
            mean_sd = str_c(round(.data$mean, 2)," (", round(.data$sd, 2), ")")) |> 
  pivot_wider(names_from = model, values_from = mean_sd) |> 
  xtable::xtable() |> 
  print(include.rownames=FALSE)


# obs_i <- sorted_data[[1]][[1]]
# exp_i <- X_fgld %*% fit_fgld_ls(obs_i)
# qqplot(obs_i, exp_i)
# empirical_bias(obs_i, exp_i)

