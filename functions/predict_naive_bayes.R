predict_naive_bayes <- function(out, X) {
  distr <- out$distr
  model <- out$model
  is_num <- out$vars$is_num
  
  # Subset categorical variables
  if(missing(X)){
    X <- out$data$X
  }
  
  if(out$distr != "discrete"){
    d_fun_theta <- switch(
      distr,
      normal = function(x, theta) dnorm(x = x, mean = theta[1], sd = theta[2], log = TRUE),
      kde = function(x, theta) log(stats::approx(theta$x, theta$y, xout = x, rule = 2, ties = "ordered")$y),
      st = function(x, theta) sn::dst(x = x, dp = theta, log = TRUE),
      gev = function(x, theta) evd::dgev(x = x, loc = theta[1], scale = theta[2], shape = theta[3], log = TRUE),
      purrr::partial(dQ,
                     Q = switch(
                       distr,
                       fgld = qfgld,
                       fod = qfod,
                       npm = qnpm,
                       gn = qgn,
                       gf = purrr::partial(qgf, a = out$a),
                       gfgld = purrr::partial(qgfgld, a = out$a),
                       quad = qquad
                     ),
                     Qprime = switch(
                       distr,
                       fgld = qfgld_deriv,
                       fod = qfod_deriv,
                       npm = qnpm_deriv,
                       gn = qgn_deriv,
                       gf = purrr::partial(qgf_deriv, a = out$a),
                       gfgld = purrr::partial(qgfgld_deriv, a = out$a),
                       quad = qquad_deriv
                     ),
                     log = TRUE)
    )
  }else if(out$distr == "discrete"){
    # Remove min and max for using findInterval later
    breaks <- purrr::imap(out$breaks, \(b, i) b[-c(1,length(b))])
  }
  
  # if(out$weighted){
  #   w <- dplyr::pull(out$var_imp, dist)
  #   w <- purrr::set_names(x = w, nm = dplyr::pull(out$var_imp, x))
  #   w <- w/sum(w)*length(w)
  # }
  
  
  # i in imap gets the name of the column!
  # logpost <- purrr::map(model$theta, \(t) purrr::imap_dfc(X,
  #                                                         function(x, j) {
  #                                                           if (is_num[j] & distr != "discrete") {
  #                                                             lp <- d_fun_theta(x = x, theta = t[[j]])
  #                                                           } else if (is_num[j] & distr == "discrete") {
  #                                                             lp <- t[[j]]$pi[findInterval(x = x, vec = breaks[[j]], left.open = TRUE, all.inside = FALSE) + 1]
  #                                                           } else if (!is_num[j]) {
  #                                                             lp <-t[[j]]$pi[unclass(x)]
  #                                                           }
  #                                                           # if(out$weighted){
  #                                                           #   w[j]*lp
  #                                                           # }else{
  #                                                           #   lp
  #                                                           # }
  #                                                         }) %>% rowSums(na.rm = TRUE))
  
  # number of response classes
  K <- length(model$theta)
  # number of variables
  J <- ncol(X)
  # number of samples
  I <- nrow(X)
  
  varnames <- colnames(X)
  classnames <- names(model$theta)
  
  logpost <- array(dim = c(K, I, J) , dimnames = list(classnames, NULL, varnames))
  
  
  for(k in 1:K){
    current_class <- classnames[k]
    for(j in 1:J){
      current_var <- varnames[j]
      if (is_num[j] & distr != "discrete") {
        logpost[k, , j] <- d_fun_theta(x = X[, j], theta = model$theta[[current_class]][[current_var]])
      }else if (is_num[j] & distr == "discrete"){
        logpost[k, , j] <- model$theta[[current_class]][[current_var]]$pi[findInterval(x = X[, j], vec = breaks[[current_var]], left.open = TRUE, all.inside = FALSE) + 1]
      }else if (!is_num[j]){
        logpost[k, , j] <- model$theta[[current_class]][[current_var]]$pi[unclass(X[, j])]
      }
    }
  }
  
  logpost <- rowSums(logpost, dims = 2)
  logpost[logpost == -Inf] <- -.Machine$double.xmin
  logpost <- logpost + log(model$prop)
  class <- apply(X = logpost, MARGIN = 2, FUN = which.max) |> 
    factor(levels = 1:length(levels(model$y)), labels = levels(model$y))
  logpost_totals <- apply(X = logpost, 2, matrixStats::logSumExp)
  post <- exp(t(logpost) - logpost_totals)
  
  return(list(class = class,
              logpost = t(logpost),
              post = post))
  
}
