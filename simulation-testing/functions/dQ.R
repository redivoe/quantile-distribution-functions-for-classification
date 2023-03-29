

dQ <- function(x, theta, log = FALSE, Q, Qprime) {
  if (log == TRUE) {
    return(invQ(x, theta, Q) %>% {
      \(u) - log(Qprime(u, theta))
    }())
  } else{
    return(invQ(x, theta, Q) %>% {
      \(u) 1 / Qprime(u, theta)
    }())
  }
}
