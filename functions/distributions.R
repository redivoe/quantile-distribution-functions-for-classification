
##--------
##  FGLD  
##--------

qfgld <- function(u, theta) {
  theta[1] + theta[2] * u + theta[3] * log(u) - theta[4] * log(1 - u)
}


qfgld_deriv <- function(u, theta) {
  ifelse(u >= 1 - 1e-10 | u <= 1e-10,
         1e10,
         theta[2] + theta[3] / u + theta[4] / (1 - u))
}

qfgld_d2<-function(u,theta){
  ifelse(u >= 1 - 1e-10 | u <= 1e-10,
         1e10,
         -theta[3] / u ^ 2 + theta[4] / (1 - u) ^ 2)
}

qfgld_d3<-function(u,theta){
  ifelse(u >= 1 - 1e-10 | u <= 1e-10,
         1e10,
         2 * theta[3] / u ^ 3 + 2 * theta[4] / (1 - u) ^ 3)
}


##---------------------------------
##  Quadratic quantile function  
##---------------------------------


qquad <- function(u, theta) {
  theta[1] + theta[2] * u + theta[3] * u^2
}

qquad_deriv <- function(u, theta) {
  ifelse(u >= 1 - 1e-10 | u <= 1e-10,
         1e10,
         theta[2] + 2 * u * theta[3])
}



##---------------------------------
##  Linear quantile function  
##---------------------------------


qlin <- function(u, theta) {
  theta[1] + theta[2] * u
}

qlin_deriv <- function(u, theta) {
  ifelse(u >= 1 - 1e-10 | u <= 1e-10,
         1e10,
         theta[2])
}

