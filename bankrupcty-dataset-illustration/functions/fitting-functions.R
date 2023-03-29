
get_u_ls<-function(y){
  n<-length(y)
  u <- (1:n - 0.5)/n
  u<-u[rank(y,ties.method = "first")]
  
  return(u)
}


##------
##  gn  
##------

fit_gn_ls<-function(y){
  u <- get_u_ls(y)
  
  z <- qnorm(u)
  t <- qt(u, 3)
  
  X <- cbind(1, z, t, u, log(u), -log(1 - u))
  
  A <- sapply(seq(1e-10, 1 - 1e-10, len = 1e3),
              \(u) c(0, 1/dnorm(qnorm(u)), 1/dt(qt(u,3),3), 1, 1/u, 1/(1-u)))
  
  out_solveQP<-quadprog::solve.QP(Dmat = crossprod(X,X),
                                  dvec = crossprod(y,X),
                                  Amat = A,
                                  bvec = rep(1e-3,1e3))

  return(out_solveQP$solution)
}



##--------
##  fgld  
##--------

get_X_fgld<-function(n){
  x2 <- digamma(1:n) - digamma(n + 1)
  
  X <- cbind(1,
             (1:n) / (n + 1),
             x2,
             rev(-x2))
  return(X)
}


fit_fgld_ls<-function(y){
  n <- length(y)
  
  X <- get_X_fgld(n)

  y <- sort(y)
  
  A <- sapply(seq(1e-10, 1 - 1e-10, len = 1e3),
              \(u) c(0, 1, 1/u, 1/(1-u)))
  
  out_solveQP<-quadprog::solve.QP(Dmat = crossprod(X,X),dvec = crossprod(X,y),
                                  Amat = A, bvec = rep(1e-3,1e3))

  return(out_solveQP$solution)
}



##---------
##  gfgld  
##---------


get_X_gfgld <- function(n, a = 0.5){
  x3 <- exp(lgamma(n:1 - a) + lgamma(n + 1) - lgamma(n:1) - lgamma(n - a + 1))
  x5 <- digamma(1:n) - digamma(n + 1)
  
  X <- cbind(1,
             (1:n) / (n + 1),
             x3,
             rev(-x3),
             x5,
             rev(-x5))
  
  return(X)
}

fit_gfgld_ls <- function(y, a = 0.5) {
  n <- length(y)
  
  X <- get_X_gfgld(n, a)
  
  y <- sort(y)
  
  A <- sapply(seq(1e-10, 1 - 1e-10, len = 1e3),
              \(u) c(0, 1,  a / (1 - u) ^ (a + 1), a / u ^ (a + 1), 1 / u, 1 / (1 - u)))
  
  out_solveQP <-
    quadprog::solve.QP(
      Dmat = crossprod(X, X),
      dvec = crossprod(X, y),
      Amat = A,
      bvec = rep(1e-3,1e3))
  
  return(out_solveQP$solution)
}






##------
##  gf  
##------


fit_gf_ls <- function(y, a = 0.5) {
  n <- length(y)
  
  x2 <- exp(lgamma(n:1 - a) + lgamma(n + 1) - lgamma(n:1) - lgamma(n - a + 1))
  
  X <- cbind(1,
             (1:n) / (n + 1),
             x2,
             rev(-x2))
  
  y <- sort(y)
  
  A <- sapply(seq(1e-10, 1 - 1e-10, len = 1e3),
              \(u) c(0, 1,  a/(1 - u)^(a + 1), a/u^(a + 1)))
  
  out_solveQP<-quadprog::solve.QP(Dmat = crossprod(X,X),dvec = crossprod(X,y),
                                  Amat = A,bvec = rep(1e-3,1e3))

  return(out_solveQP$solution)
}

##---------------------------------
##  Logistic with quadratic terms  
##---------------------------------


get_X_quad <- function(n){
  x2 <- exp(log(1:n) + log(1:n + 1) - log(n+2) - log(n+1))
  x5 <- digamma(1:n) - digamma(n + 1)
  
  X <- cbind(1,
             x2,
             rev(-x2),
             x5,
             rev(-x5))
  
  return(X)
}

fit_quad_ls<-function(y){
  n <- length(y)
  
  x2<-digamma(1:n)-digamma(n+1)
  
  X<-get_X_quad(n)
  y<-sort(y)
  
  A <- sapply(seq(1e-10, 1 - 1e-10, len = 1e3),
              \(u) c(0, 2 * u, 2 * (1 - u), 1 / u, 1 / (1 - u)))
  
  
  out_solveQP<-quadprog::solve.QP(Dmat = crossprod(X,X), dvec = crossprod(X,y),
                                  Amat = A,bvec = rep(1e-3,1e3))
  
  return(out_solveQP$solution)
}




##-------
##  FOD  
##-------


get_X_fod<-function(n){
  
  i <- 2:(n-1)
  x3 <- exp(lgamma(i - 0.5) + lgamma(n+1) - lgamma(i) - lgamma(n + 0.5)) - n/(i - 1)
  # x4 <- exp(lgamma(1:n - 0.5) + lgamma(n - 1:n + 1.5) - lgamma(1:n) - lgamma(n:1))
  
  X <- cbind(1,
             # i / (n + 1),
             x3,
             rev(-x3))
  
  return(X)
}


fit_fod_ls<-function(y){
  n <- length(y)
  
  X <- get_X_fod(n)
  
  y <- sort(y)
  y <- y[2:(n-1)]

  A <- sapply(seq(1e-10, 1 - 1e-10, len = 1e3),
              \(u) c(0,  (2 - sqrt(u)) / (2 * u ^ 2),  (2 - sqrt(1 - u)) / (2 * (1 - u) ^ 2)))
  
  out_solveQP<-quadprog::solve.QP(Dmat = crossprod(X,X),dvec = crossprod(X,y),
                                  Amat = A, bvec = rep(1e-3,1e3))
  
  return(out_solveQP$solution)
}
