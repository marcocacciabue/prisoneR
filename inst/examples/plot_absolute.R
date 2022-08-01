
#' params <- prepare_parameters()
#'
#' simulationLV <- game(type="Lotka",
#'                      play1=count_def, #strategy of player 1#
#'                      play2=count_def, #strategy of player 2,#
#'                      parameters=params #list of parameters to pass to ode solver
#' )
#'
#'plot_absolute(simulationLV)
