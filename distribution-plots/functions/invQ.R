# Now 100% more vectorised

invQ <- function(x, theta, Q) {
  purrr::map_dbl(
    x,
    ~ tryCatch(
      stats::uniroot(function(u) Q(u, theta) - .x,
        interval = c(1 - 1e-10, 1e-10)
      )$root,
      error = function(cond) {
        ifelse(.x > theta[1], 1 - 1e-10, 1e-10)
      }
    )
  )
}
