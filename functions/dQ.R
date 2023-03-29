

dQ <- function(x, theta, log = FALSE, Q, Qprime) {
  u <- invQ(x, theta, Q)
  if (isTRUE(log)) {
    return(- log(Qprime(u, theta)))
  } else{
    return(1 / Qprime(u, theta))
  }
}
