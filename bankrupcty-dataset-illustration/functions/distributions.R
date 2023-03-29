
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



##------
##  GF  
##------

qgf <- function(u, theta, a = 0.5) {
  theta[1] + theta[2] * u  + theta[3] / (1 - u) ^ a - theta[4] / u ^ a
}

qgf_deriv <- function(u, theta, a = 0.5) {
  ifelse(u >= 1 - 1e-10 | u <= 1e-10,
         1e10,
         theta[2]  + theta[3] * a / (1 - u) ^ (a + 1) + theta[4] * a / u ^ (a + 1)
  )
}


##------
##  GN  
##------

qgn <- function(u, theta) {
  theta[1] + theta[2] * qnorm(u) + theta[3] * qt(u, 3) + theta[4] * u + theta[5] *
    log(u) - theta[6] * log(1 - u)
}

qgn_deriv<- function(u, theta) {
  ifelse(u >= 1 - 1e-10 | u <= 1e-10,
         1e10,
         theta[2]/dnorm(qnorm(u)) + 
           theta[3]/dt(qt(u,3),3) + 
           theta[4] + 
           theta[5]/u + 
           theta[6]/(1-u))
}



##-----------
##  GFGLD  
##-----------

qgfgld <- function(u, theta, a = 0.5) {
  theta[1] + theta[2] * u + theta[3] / (1 - u) ^ a - theta[4] / u ^ a + theta[5] *
    log(u) - theta[6] * log(1 - u)
}

qgfgld_deriv <- function(u, theta, a = 0.5) {
  ifelse(u >= 1 - 1e-10 | u <= 1e-10,
         1e10,
         theta[2] + theta[3] * a / (1 - u) ^ (a + 1) + theta[4] * a / u ^ (a + 1) +
           theta[5] / u + theta[6] / (1 - u)
  )
}



##---------------------------------
##  Logistic with quadratic terms  
##---------------------------------

qquad <- function(u, theta) {
  theta[1] + theta[2] * u^2 - theta[3] * (1-u)^2 + theta[4] *
    log(u) - theta[5] * log(1 - u)
}

qquad_deriv <- function(u, theta) {
  ifelse(u >= 1 - 1e-10 | u <= 1e-10,
         1e10,
         theta[2] * 2 * u + theta[3] * 2 * (1 - u) + theta[4] / u + theta[5] / (1 - u)
  )
}

qquad_d2<-function(u,theta){
  ifelse(u >= 1 - 1e-10 | u <= 1e-10,
         1e10,
         2 * theta[2] - 2 * theta[3] - theta[4] / u ^ 2 + theta[5] / (1 - u) ^ 2
  )
}

qquad_d3<-function(u,theta){
  ifelse(u >= 1 - 1e-10 | u <= 1e-10,
         1e10,
         2 * theta[4] / u ^ 3 + 2 * theta[5] / (1 - u) ^ 3)
}


##-------
##  FOD  
##-------

qfod <- function(u, theta){
  theta[1] + theta[2] * u + theta[3] * (sqrt(u) - 1) / u - theta[4] * (sqrt(1 - u) - 1) / (1 - u)
}


qfod_deriv <- function(u, theta) {
  ifelse(
    u >= 1 - 1e-10 | u <= 1e-10,
    1e10,
    theta[2] + theta[3] * (2 - sqrt(u)) / (2 * u ^ 2) + theta[4] *
      (2 - sqrt(1 - u)) / (2 * (1 - u) ^ 2)
  )
}

# qfod_d2<-function(u,theta){
#   ifelse(u >= 1 - 1e-10 | u <= 1e-10,
#          1e10,
#          -theta[3] / u ^ 2 + theta[4] / (1 - u) ^ 2)
# }
# 
# qfod_d3<-function(u,theta){
#   ifelse(u >= 1 - 1e-10 | u <= 1e-10,
#          1e10,
#          2 * theta[3] / u ^ 3 + 2 * theta[4] / (1 - u) ^ 3)
# }

