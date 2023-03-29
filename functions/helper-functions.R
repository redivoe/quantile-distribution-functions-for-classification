test_fun <- function(y, X) {
  out <- naive_bayes(
    y = y,
    X = X,
    distr = "fgld",
    var_sel = TRUE,
    fitted_values = FALSE
  )$test
  
  out <- out |>
    arrange(pvalue)
  
  return(out)
}

sec_axis_fun <- function(x, pvalues){
  out <- numeric(length = length(x))
  p <- length(pvalues)
  
  cond1 <- x<=0
  cond2 <- x>=p

  out[cond1] <- NA
  out[cond2] <- 1

  rest <- !cond1 & !cond2
  out[rest] <- quantile(pvalues, probs = x[rest]/p)
  return(out)
}



