


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





##---------------------------------
##  Quadratic  
##---------------------------------


get_X_quad <- function(n){
  
  x1 <- (1:n) / (n + 1)
  x2 <- exp(log(1:n) + log(1:n + 1) - log(n+2) - log(n+1))

  X <- cbind(1,
             x1,
             x2)
  
  return(X)
}

fit_quad_ls<-function(y){
  n <- length(y)
  
  X <- get_X_quad(n)
  y <- sort(y)
  
  A <- rbind(0, 1, 2 * seq(1e-10, 1 - 1e-10, len = 1e3))
  
  out_solveQP <- quadprog::solve.QP(Dmat = crossprod(X, X), dvec = crossprod(X, y), 
                                    Amat = A, bvec = rep(1e-3, 1e3))
  
  return(out_solveQP$solution)
}


