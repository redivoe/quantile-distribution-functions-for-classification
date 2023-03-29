
naive_hist <- function(data, class_name) {
  # k<- data %>% 
  #   pull({{class_name}}) %>% 
  #   unique %>% 
  #   length
  # 
  # breaks <- data %>%
  #   summarise(across(.cols = -{{class_name}},
  #                    .fns = \(x) range(x))) %>%
  #   mutate(id = c("min","max")) %>%
  #   pivot_longer(cols = -id,names_to = "var") %>%
  #   pivot_wider(names_from = id,values_from = value) %>%
  #   pivot_wider(names_from = id,values_from = value) %>%
  #   select(-var)
  # 
  # breaks<-rbind(breaks,breaks)
  # 
  # count<-0
  # var<-0
  # breaks_fun <- function(x){
  #   count <<- count + 1L
  #   if((count/2)%%k==0 | (count)%%k==0){
  #     var<<-var +1
  #     breaks[var,] %>% as.numeric()
  #   }
  # }
  
  data %>%
    dplyr::select(c({{class_name}}, where(is.numeric))) %>% 
    tidyr::pivot_longer(-{{class_name}},names_to = "var",values_to = "x") %>% 
    ggplot2::ggplot(ggplot2::aes(x))+
    ggplot2::geom_histogram(ggplot2::aes(y = ..ncount..), fill = "grey80") +
    ggplot2::facet_grid(rows = vars({{class_name}}),cols = vars(var),scales = "free")+
    # scale_x_continuous(breaks = breaks_fun)+
    # n.breaks = 3)+
    # ggthemes::theme_tufte(base_family = "sans")+
    ggplot2::theme_bw() +
    ggplot2::theme(
      axis.title = ggplot2::element_blank(),
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.grid = ggplot2::element_blank()
    )
}







naive_visual <- function(out,
                         type = "density",
                         faceting = TRUE,
                         max_var = 20,
                         n_bins = 30) {
  distr <- out$distr
  y <- out$data$y
  is_num <- out$vars$is_num
  X <- out$data$X[,is_num, drop = FALSE]
  if (sum(is_num) > max_var) {
    X <- X[, 1:max_var]
  }
  
  
  if(sum(out$var$is_num)==0){
    stop("No numeric variables used in the model: this function aims at visualising the fit to numerical variables")
  }
  
  if (distr != c("discrete")) {
    Q_fun <- switch(
      distr,
      fgld = qfgld,
      fod = qfod,
      npm = qnpm,
      gn = qgn,
      gf = purrr::partial(qgf, a = out$a),
      gfgld = purrr::partial(qgfgld, a = out$a),
      quad = qquad,
      normal = function(u, theta)
        stats::qnorm(p = u, mean = theta[1], sd = theta[2]),
      kde = NULL
    )
    d_fun_theta <- switch(
      distr,
      normal = function(x, theta) dnorm(x = x, mean = theta[1], sd = theta[2]),
      kde = function(x, theta) stats::approx(theta$x, theta$y, xout = x, rule = 2, ties = "ordered")$y,
      purrr::partial(dQ,
                     Q = Q_fun,
                     Qprime = switch(
                       distr,
                       fgld = qfgld_deriv,
                       fod = qfod_deriv,
                       npm = qnpm_deriv,
                       gn = qgn_deriv,
                       gf = purrr::partial(qgf_deriv, a = out$a),
                       gfgld = purrr::partial(qgfgld_deriv, a = out$a),
                       quad = qquad_deriv
                       )
                     )
    )
  }
  

  unnested_theta <- out$model %>%
    mutate(theta = map(theta, function(t){
      t <- t[names(is_num)[is_num]]
      if (sum(is_num) > max_var) {
          t <- t[1:max_var]
          }
      return(t)}
      )) %>% 
    dplyr::select(-c(n, prop)) %>%
    tidyr::unnest(c(theta)) %>%
    dplyr::mutate("var" = names(theta))
  
  
  if (type == "density") {
    theme_adhoc <- function() {
      ggplot2::theme_bw() +
        ggplot2::theme(
          axis.title = ggplot2::element_blank(),
          axis.text.y = ggplot2::element_blank(),
          axis.ticks.y = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank()
        )
    }
    if(distr != "discrete"){
      range_vars <- X %>%
        tidyr::pivot_longer(cols = dplyr::everything(), names_to = "var") %>%
        dplyr::group_by(var) %>%
        dplyr::summarise(min = min(value), max = max(value))
      
      d_dens <- dplyr::full_join(range_vars,
                                 unnested_theta,
                                 by = "var") %>%
        dplyr::mutate(x = purrr::map2(min, max,  ~ seq(.x, .y, len = 2e2)),
                      fx = purrr::map2(x, theta, ~ d_fun_theta(x = .x,theta = .y))) %>%
        dplyr::select(y, var, x, fx) %>%
        tidyr::unnest(c(x, fx))
      
      if(faceting){
        
        dplyr::tibble(y, X) %>%
          tidyr::pivot_longer(-y, names_to = "var", values_to = "x") %>%
          ggplot2::ggplot(aes(x = x)) +
          # ggplot2::geom_rug(col="grey80") +
          ggplot2::geom_histogram(
            ggplot2::aes(y = ..density..),
            fill = "grey80",
            col = NA,
            bins = n_bins
          ) +
          ggh4x::facet_grid2(
            rows = dplyr::vars(y),
            cols = dplyr::vars(var),
            scales = "free",
            independent = "y"
          ) +
          ggplot2::geom_line(ggplot2::aes(y = fx), data = d_dens) +
          theme_adhoc()
        
      }else{
        
        nrows <- 3
        ncols <- 10
        n_pages_paginate <- ceiling(ncol(X)/(nrows*ncols))
        
        plottable <- dplyr::tibble(y, X) %>%
          tidyr::pivot_longer(-y, names_to = "var", values_to = "x")

        returnable <- purrr::map(seq_len(n_pages_paginate), ~  plottable %>%
          ggplot2::ggplot(ggplot2::aes(x = x, col = y)) +
          ggforce::facet_wrap_paginate(facets = dplyr::vars(
            forcats::fct_relevel(var, colnames(X))),
                                       scales = "free",
                                       nrow = nrows,
                                       ncol = ncols,
                                       page = .x) +
          ggplot2::geom_line(ggplot2::aes(y = fx), data = d_dens) +
          theme_adhoc()+
          ggplot2::theme(legend.position = "none"))
        
        return(returnable)
      }

      
      
    }else if(distr == "discrete"){
      
      if (sum(is_num) > max_var) {
        out$breaks <- out$breaks[1:max_var]
      }
      
      d_dens <- map(out$breaks, list) %>%
        dplyr::as_tibble() %>% 
        tidyr::pivot_longer(cols = everything(),names_to = "var",values_to = "breaks") %>% 
        dplyr::full_join(., unnested_theta, by = "var") %>% 
        dplyr::mutate(h = purrr::map2(theta, breaks, ~ c(0,.x$pi/diff(.y),0)),
                      x = purrr::map(breaks, \(b) c(b[1], b))) %>%
        dplyr::select(y, var, x, h) %>%
        tidyr::unnest(c(x, h))
      
      
      dplyr::tibble(y, X) %>%
        tidyr::pivot_longer(-y, names_to = "var", values_to = "x") %>%
        ggplot2::ggplot(aes(x = x)) +
        ggplot2::geom_rug()+
        ggh4x::facet_grid2(
          rows = dplyr::vars(y),
          cols = dplyr::vars(var),
          scales = "free",
          independent = "y"
        ) +
        ggplot2::geom_step(aes(y=h), data = d_dens)+
        theme_adhoc()
      
    }
  } else if (type %in% c("qq", "qqrotated")) {
    
    nest_data <- dplyr::tibble(y, X) %>%
      tidyr::pivot_longer(-y, names_to = "var", values_to = "x") %>%
      dplyr::group_by(y, var) %>%
      dplyr::summarise(x = list(x),.groups = "drop")
    
    d_qq <- dplyr::inner_join(x = unnested_theta,
                              y = nest_data,
                              by = c("y", "var")) %>%
      dplyr::mutate(x = purrr::map(x, sort),
             q = purrr::map2(x, theta, ~ Q_fun(
               u = ppoints(length(.x)), theta = .y
             ))) %>%
      dplyr::select(-theta) %>%
      tidyr::unnest(c(x, q))
    
    
    theme_adhoc2 <- function() {
      ggplot2::theme_bw() +
        ggplot2::theme(
          axis.title = ggplot2::element_blank(),
          axis.text = ggplot2::element_blank(),
          axis.ticks = ggplot2::element_blank(),
          panel.grid = ggplot2::element_blank()
        )
    }
    
    if (type == "qq") {
      d_qq %>%
        ggplot2::ggplot(aes(x = q, y = x)) +
        geom_abline(slope = 1,
                    intercept = 0,
                    lty = 1) +
        ggplot2::geom_point(size = 0.8,
                            alpha = 0.9,
                            col = "grey30") +
        ggh4x::facet_grid2(
          rows = dplyr::vars(y),
          cols = dplyr::vars(var),
          scales = "free",
          independent = "all"
        ) +
        theme_adhoc2()
      
    } else if (type == "qqrotated") {
      
      d_qq %>%
        ggplot2::ggplot(aes(x = x + q, y = x - q)) +
        geom_hline(yintercept = 0) +
        ggplot2::geom_point(size = 0.8,
                            alpha = 0.9,
                            col = "grey30") +
        ggh4x::facet_grid2(
          rows = dplyr::vars(y),
          cols = dplyr::vars(var),
          scales = "free",
          independent = "x"
        ) +
        theme_adhoc2()
    }
  }
}
