
#' params <- prepare_parameters()
#'
#' simulationLV <- game(type="Lotka",
#'                      play1="Count_defective", #strategy of player 1#
#'                      play2="Count_defective", #strategy of player 2,#
#'                      parameters=params #list of parameters to pass to ode solver
#' )
#'
#'plot_relative(simulationLV)
