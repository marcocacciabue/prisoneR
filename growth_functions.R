#####################################################################
#####           Definimos la funcion de Lotka-volterra          #####
#####################################################################

LotkaVolt <- function(t, state, parameters){
  with(as.list(c(state, parameters)),{
    # rate of change
    dx1 <- r1*x1*(1-((x1*(genome1^mutation1) + a12 * x2)/k1))
    dx2 <- r2*x2*(1-((x2*(genome2^mutation2) + a21 * x1)/k2))
    # return the rate of change
    list(c(dx1, dx2))
  })
}

