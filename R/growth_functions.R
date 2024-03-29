

#' Lotka-Volterra Growth function
#'
#' @param p1Score numeric: previous condition for population 1
#' @param p2Score numeric: previous condition for population 2
#' @param parameters list: parameters passed from the `game()` function
#'
#' @return numeric: the next generation values
#' @export
#'
#' @example inst/examples/LV.R
#'
LV <- function(p1Score, p2Score, parameters) {
  LotkaVolt <- function(t, state, parameters) {
    with(as.list(c(state, parameters)), {
      # rate of change
      dx1 <- r1 * x1 * (1 - ((x1 * (genome1^mutation1) + a12 * x2) / k1))
      dx2 <- r2 * x2 * (1 - ((x2 * (genome2^mutation2) + a21 * x1) / k2))
      # return the rate of change
      list(c(dx1, dx2))
    })
  }

  with(as.list(c(p1Score, p2Score, parameters)), {
    #  Compute population dynamics for this parameters
    state <- c(x1 = as.numeric(p1Score), x2 = as.numeric(p2Score))
    tiempos <- seq(0, 1, by = 1) # sequence for the passage of time
    out <- deSolve::ode(func = LotkaVolt, y = state, times = tiempos, parms = parameters)
    p1Score_new <- out[2, 2]
    p2Score_new <- out[2, 3]
    return(c(p1Score_new, p2Score_new))
  })
}




#' May Logistic Growth function
#'
#' @param p1Score numeric: previous condition for population 1
#' @param p2Score numeric: previous condition for population 2
#' @param parameters list: parameters passed from the `game()` function
#'
#' @return Two numerics with the next generation value
#' @export
#' @example inst/examples/MAY.R

MAY <- function(p1Score, p2Score, parameters) {
  with(as.list(c(p1Score, p2Score, parameters)), {
    p1Score_new <- (r1 * p1Score * (H1 - (p1Score * (genome1**mutation1))))
    p2Score_new <- (r2 * p2Score * (H2 - (p2Score * (genome2**mutation2))))
    return(c(p1Score_new, p2Score_new))
  })
}
