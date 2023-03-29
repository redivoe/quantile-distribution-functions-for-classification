naive_bayes <- function(y,
                        X,
                        distr = "fgld",
                        a = 0.5,
                        fitted_values = TRUE,
                        var_sel = FALSE,
                        alpha = 0.05,
                        var_info = FALSE,
                        weighted = FALSE) {
  
  if(!is.factor(y)){
    y <- factor(y)
  }else{
    y <- droplevels(y)
  }
  
  is_num <- sapply(X, is.numeric)
  n_num <- sum(is_num)
  n_cat <- ncol(X) - n_num

  if (n_num > 0 & distr == "discrete") {
    
    # n_bins <- rep(floor(sqrt(nrow(X))), n_num)
    
    n_bins <- rep(floor(log(nrow(X),base = 2)+1), n_num)
    
    # Proportional discretization
    breaks <- purrr::map2(X[, is_num, drop = FALSE], n_bins, function(x, y) ggplot2:::breaks(x = x, equal = "width", nbins = y)) 
    n_dup <- purrr::map_dbl(breaks, anyDuplicated)
    
    while (any(n_dup > 0)) {
      n_bins[n_dup > 0] <- n_bins[n_dup > 0] - 1
      breaks <- purrr::map2(X[, is_num, drop = FALSE], n_bins, function(x, b) ggplot2:::breaks(x = x, equal = "width", nbins = b))
      n_dup <- purrr::map_dbl(breaks, anyDuplicated)
    }
    
    breaks <- purrr::map(breaks, unname)
    
    discrete_fun <- function(x, j){
      cut(x = x, breaks = breaks[[j]], include.lowest = TRUE) %>% 
        forcats::fct_count(f = .) %>% 
        dplyr::transmute(f,
                         pi = (n + 1 / (length(breaks[[j]]) - 1)) / (sum(n) + 1))
    }
  }
  if (n_num > 0 & distr != "discrete") {
    fit_fun <- switch(
      distr,
      fgld = fit_fgld_ls,
      fod = fit_fod_ls,
      npm = fit_npm_ls,
      gn = fit_gn_ls,
      gf = purrr::partial(fit_gf_ls, a = a),
      gfgld = purrr::partial(fit_gfgld_ls, a = a),
      quad = fit_quad_ls,
      st = function(y) { sn::selm(y ~ 1, family = "ST") %>% coefficients(param.type = "DP") },
      gev = purrr::compose(
        purrr::partial(getElement, name = "estimate"),
        purrr::partial(evd::fgev, std.err = FALSE)
      ),
      kde = stats::density,
      normal = function(y) { xbar <- mean(y); s <- sd(y); s <- ifelse(s==0, 1e-10, s); return(c(xbar, s))}
    )
  }
  if(n_cat > 0){
    C <- X[, !is_num, drop = FALSE] %>%
      purrr::map(purrr::compose(length, levels))
    
    prop_fun <- function(x, j){
      forcats::fct_count(x) %>%
        tidyr::drop_na() %>%
        dplyr::transmute(f,
                         pi = (n + 1 / C[[j]]) / (sum(n) + 1))
    } 
  }
  
  m <- tibble::tibble(y, X) %>%
    dplyr::group_by(y) %>%
    dplyr::summarise(
      theta = purrr::imap(dplyr::cur_data(),
                          function(x, j) {
                            if (distr != "discrete" && is_num[j]) {
                              fit_fun(x)
                            } else if (distr == "discrete" && is_num[j]) {
                              discrete_fun(x, j)
                            } else{
                              prop_fun(x, j)
                            }
                          }) %>% list,
      n = dplyr::n(),
      .groups = "drop"
    ) %>%
    dplyr::mutate(prop = n / sum(n),
                  theta = purrr::set_names(theta, y))
  
  out <- list(model = m,
              distr = distr,
              data = list("y" = y, "X" = X),
              vars = list("is_num" = is_num))
  
  if (distr %in% c("gf", "gfgld")) {
    out$weighted <- weighted
    out$a <- a
  }
  if (distr == "discrete") {
    out$breaks <- breaks
  }
  if (var_info){
    out$var_info <- info_imp(out)
  }
  if (distr == "fgld" && var_sel){
    
    B <- purrr::map(m$n, get_X_fgld)
    A <- purrr::map(B, \(B) B%*%solve(crossprod(B)))
    
    Gamma <- purrr::pmap(list(m$theta, m$n, A), \(b, n, A) purrr::map(.x = b,\(b_i) t(A)%*%S_fgld(b_i, n = n)%*%A))

    test <- tibble::tibble("x" = colnames(X)) %>%
      tidyr::expand_grid("y1" = levels(y), "y2" = levels(y)) %>%
      dplyr::filter(y1 < y2) %>% 
      dplyr::mutate(
        dist = purrr::pmap_dbl(list(x,y1,y2),
        \(x, y1, y2) (m$theta[[y1]][[x]] - m$theta[[y2]][[x]])%*%solve(Gamma[[y1]][[x]] + Gamma[[y2]][[x]])%*%(m$theta[[y1]][[x]] - m$theta[[y2]][[x]])),
        pvalue = pchisq(q = dist, df = 4,  lower.tail = FALSE))
    
    selected_vars <- test %>% 
      dplyr::group_by(x) %>% 
      dplyr::slice_min(order_by = pvalue, n = 1) %>% 
      dplyr::filter(pvalue < alpha) %>% 
      dplyr::pull(x)
    
    out$test <- test
    out$selected_vars <- selected_vars
  }
  
  if (fitted_values == TRUE) {
    predict_out <- predict_naive_bayes(out)
    out$class <- predict_out$class
    out$logpost <- predict_out$logpost
    out$post <- predict_out$post
  }
  
  return(out)
}
