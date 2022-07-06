#####################################################################
#####           Definimos la funcion de Lotka-volterra          #####
#####################################################################




LV_general<-function(p1Score,p2Score,parameters){
  
  LotkaVolt <- function(t, state, parameters){
    with(as.list(c(state, parameters)),{
      # rate of change
      dx1 <- r1*x1*(1-((x1*(genome1^mutation1) + a12 * x2)/k1))
      dx2 <- r2*x2*(1-((x2*(genome2^mutation2) + a21 * x1)/k2))
      # return the rate of change
      list(c(dx1, dx2))
    })
  }
  
  with(as.list(c(p1Score, p2Score,parameters)),{
  #  Compute population dynamics for this parameters
  state <- c(x1 = as.numeric(p1Score), x2 = as.numeric(p2Score))
  tiempos <- seq(0, 1, by = 1) # sequence for the passage of time
  out <- deSolve::ode(func = LotkaVolt, y = state, times = tiempos, parms = parameters)
  p1Score <- out[2,2]
  p2Score <- out[2,3]
  return(c(p1Score,p2Score))
  })
  
  
}




MAY_general<-function(p1Score,p2Score,parameters){
    
  with(as.list(c(p1Score, p2Score,parameters)),{
    p1Score <- (r1*p1Score*(H1-(p1Score*(genome1**mutation1))))
    p2Score <- (r2*p2Score*(H2-(p2Score*(genome2**mutation2))))
    return(c(p1Score,p2Score))
  })
  
  
}

