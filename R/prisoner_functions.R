library(deSolve)
library(ggplot2)



#' game simulation
#'
#' `game()` returns a data.frame with the Fst values for the simulated
#' interactions between two populations of viral particles.
#'
#'
#' @param type string: method for which to run the function: `"May"`, `"Lotka"`, or
#'   `"Custom"` (Must be set).
#' @param funGrowth function: simulates the growth dynamics.
#' @param play1 function: simulates the population 1 decision making (defaults to always cooperative).
#' @param play2 function: simulates the population 2 decision making (defaults to always cooperative).
#' @param interaction function: handles the changes in growth parameters passed to
#'  `funGrowth` according to the interaction matrix.
#' @param input1 numeric: the initial condition of population 1 (defaults to 0.1).
#' @param input2 numeric: the initial condition of population 2 (defaults to 0.1).
#' @param generations numeric: the number of rounds (generations) to simulate.
#' @param k1 numeric: the maximum system capacity for population 1. Only relevant for Count_defective method (default=1).
#' @param k2 numeric: the maximum system capacity for population 2. Only relevant for Count_defective method (default=1).
#' @param d1 numeric: proportion of k to evaluate score. Only relevant for Count_defective method (default=1).
#' @param d2 numeric: proportion of k to evaluate score. Only relevant for Count_defective method (default=1).
#' @param parameters list: all the arguments ALL the necessary for `funGrowth`, `interaction`, `play1` and `play2` functions.
#'
#' @returns A [data.frame()] with the obtained scores. The output has the following columns:
#'
#' @export
#'
#' @example inst/examples/game.R
#'
game <- function(type = c("May", "Lotka", "Custom"),
                 funGrowth, # growth function
                 play1 = c(
                   "Alwaysfunctional", "Alwaysdefectiveinterfering",
                   "Randommodification", "Randomeffective",
                   "Randomdefectiveinterfering", "Count_defective"
                 ), # strategy of player 1
                 play2 = c(
                   "Alwaysfunctional", "Alwaysdefectiveinterfering",
                   "Randommodification", "Randomeffective",
                   "Randomdefectiveinterfering", "Count_defective"
                 ), # strategy of player 2
                 interaction, # interaction function
                 input1 = NULL, # initial condition for player 1
                 input2 = NULL, # initial condition for player 2
                 generations = NULL, # number of generations
                 k1 = NULL,
                 k2 = NULL,
                 d1 = NULL,
                 d2 = NULL,
                 parameters # list of parameters to pass to ode solver
) {

  # check type parameter
  if (is.null(type) | missing(type)) {
    stop("'play1' must be set and must be one of the following: May Lotka or Custom")
  }
  type <- match.arg(type)

  # handle functions in case type = "Custom"
  if (type == "Custom") {
    if (missing(funGrowth) | is.null(funGrowth) | !is.function(funGrowth)) {
      stop("'funGrowth' must be given as a function.You can try preloaded LV or MAY accordingly")
    }
    if (missing(interaction) | is.null(interaction) | !is.function(interaction)) {
      stop("'interaction' must be given as a function.You can try preloaded interaction_dynamic_MAY or interaction_dynamic_LV accordingly")
    }

    if (is.null(play1) | missing(play1) | !is.function(play1)) {
      stop("'play1' must be given as a function.")
    }

    if (is.null(play2) | missing(play2) | !is.function(play2)) {
      stop("'play2' must be given as a function.")
    }

  } else {

    # check play1 parameter
    if (is.null(play1) | missing(play1)) {
      stop("'play1' must be set and must be one of the following: Alwaysfunctional, Alwaysdefectiveinterfering,
                             Randommodification, Randomeffective,
                             Randomdefectiveinterfering, Count_defective")
    }
    play1 <- match.arg(play1)

    # check play2 parameter
    if (is.null(play2) | missing(play2)) {
      stop("'play2' must be set and must be one of the following: Alwaysfunctional, Alwaysdefectiveinterfering,
                             Randommodification, Randomeffective,
                             Randomdefectiveinterfering, Count_defective")
    }
    play2 <- match.arg(play2)

    play1 <- check_strategy(play1)
    play2 <- check_strategy(play2)

  }



  funGrowth <- switch(type,
    "May" = MAY,
    "Lotka" = LV,
    "Custom" = funGrowth
  )
  interaction <- switch(type,
    "May" = interaction_dynamic_MAY,
    "Lotka" = interaction_dynamic_LV,
    "Custom" = interaction
  )


  # check if parameters is a list
  if (!is.list(parameters) | is.null(parameters)) stop("'parameters' should be given as a list of values")

  # check if no duplication of arguments are given in parameters and individuals arguments
  if (!is.null(generations) & "generations" %in% names(parameters)) {
    stop("If 'parameters' is a list that contains generations, argument 'generations' should be NULL")
  }
  if (!is.null(input1) & "input1" %in% names(parameters)) {
    stop("If 'parameters' is a list that contains input1, argument 'input1' should be NULL")
  }
  if (!is.null(input2) & "input2" %in% names(parameters)) {
    stop("If 'parameters' is a list that contains input2, argument 'input2' should be NULL")
  }
  if (!is.null(k1) & "k1" %in% names(parameters)) {
    stop("If 'parameters' is a list that contains k1, argument 'k1' should be NULL")
  }
  if (!is.null(k2) & "k2" %in% names(parameters)) {
    stop("If 'parameters' is a list that contains k2, argument 'k2' should be NULL")
  }

  if (!is.null(d1) & "d1" %in% names(parameters)) {
    stop("If 'parameters' is a list that contains d1, argument 'd1' should be NULL")
  }
  if (!is.null(d2) & "d2" %in% names(parameters)) {
    stop("If 'parameters' is a list that contains d2, argument 'd2' should be NULL")
  }


  # define corresponding arguments given in parameters
  if (is.null(parameters$input1)) parameters$input1 <- input1
  if (is.null(parameters$input2)) parameters$input2 <- input2
  if (is.null(parameters$generations)) parameters$generations <- generations
  if (is.null(parameters$k1)) parameters$k1 <- k1
  if (is.null(parameters$k2)) parameters$k2 <- k2
  if (is.null(parameters$d1)) parameters$d1 <- d1
  if (is.null(parameters$d2)) parameters$d2 <- d2

  # Define general values in case no arguments are given.
  if (is.null(input1) & !("input1" %in% names(parameters))) parameters$input1 <- 0.1
  if (is.null(input2) & !("input2" %in% names(parameters))) parameters$input2 <- 0.1
  if (is.null(generations) & !("generations" %in% names(parameters))) parameters$generations <- 50
  if (is.null(k1) & !("k1" %in% names(parameters))) parameters$k1 <- 1
  if (is.null(k2) & !("k2" %in% names(parameters))) parameters$k2 <- 1
  if (is.null(d1) & !("d1" %in% names(parameters))) parameters$d1 <- 1
  if (is.null(d2) & !("d2" %in% names(parameters))) parameters$d2 <- 1


  # check if relevant integers are given
  if (!is.numeric(parameters$input1)) {
    stop("'input1' should be given as an numeric")
  }

  if (!is.numeric(parameters$input2)) {
    stop("'input2' should be given as an numeric")
  }

  if ((!parameters$generations == round(parameters$generations)) | (parameters$generations > 10000) | (parameters$generations < 2)) {
    stop("'generations' should be given as an interger of values between 2 and 1000")
  }

  if (!is.numeric(parameters$k1)) {
    stop("'k1' should be given as an numeric")
  }
  if (!is.numeric(parameters$k2)) {
    stop("'k2' should be given as an numeric")
  }

  cat(
    "Running ", type, " simulation for ", parameters$generations, " generations with the following parameters \n",
    "population 1 initial condition ", parameters$input1, "\n",
    "population 2 initial condition ", parameters$input2
  )

  with(as.list(c(
    funGrowth,
    play1,
    play2,
    parameters
  )), {


    # TODO
    # pass generation number to interaction function in other to be able
    # to respond to generation number (for specific experiments)

    # TODO
    # add check step to control that funGrowth`s parameters
    # are included in parameters


    # TODO
    # Discuss possible checks for parameters (How to deal
    # with unknown parameters the user would include?)
    # set a maximum number possible?

    ## initial values of the two populations that
    ## compose deserters (need help) and helpers (not deserters)

    p1Score <- input1
    p2Score <- input2
    p3Score <- 0
    p4Score <- 1
    p5Score <- (input1 / input2)



    # establish first interaction according to the strategy of each player
    # prev=NULL because this is the first interaction. If strategy is dependent
    # on the previous interaction the corresponding function should deal
    # correctly with the first value)



    p1Move <- play1(prev = NULL,
                    Score = p1Score,
                    k = k1,
                    delta = d1,
                    parameters)
    p2Move <- play2(prev = NULL,
                    Score = p2Score,
                    k = k2,
                    delta = d2,
                    parameters)


    # Genero los vectores que guardaran los valores de cada ciclo
    xn1 <- p1Score
    xn2 <- p2Score
    Relative_fst <- p3Score
    Absolute_fst <- p4Score
    Total_fst <- p5Score
    Move1 <- p1Move
    Move2 <- p2Move

    ## Se define el número de veces que se repiten las interacciones##
    for (i in 1:generations) {

      # change parameter according to the interaction in this generation
      parameters <- interaction(
        p1Move,
        p2Move,
        parameters
      )

      out <- funGrowth(p1Score, p2Score, parameters)

      #  change parameters for the next generation
      p1Score <- out[1]
      p2Score <- out[2]
      p3Score <- out[1] / out[2]
      p4Score <- ((out[1] / out[2]) / (input1 / input2))
      p5Score <- (Total_fst[length(Total_fst)] / (out[1] / out[2]))

      # Vuelvo a calcular los move para el siguiente ciclo
      prevP1Move <- p1Move
      prevP2Move <- p2Move


      p1Move <- play1(prev = prevP1Move,
                      Score = p1Score,
                      k = k1,
                      delta = d1,
                      parameters)
      p2Move <- play2(prev = prevP2Move,
                      Score = p2Score,
                      k = k2,
                      delta = d2,
                      parameters)
      # p1Move <- play1(prev = prevP1Move, score = p1Score, k = k1, delta = d1, parameters)
      # p2Move <- play2(prev = prevP2Move, score = p2Score, k = k2, delta = d2, parameters)

      # Guardo el valor del ciclo
      xn1 <- append(x = xn1, values = p1Score)
      xn2 <- append(x = xn2, values = p2Score)
      Relative_fst <- append(x = Relative_fst, values = p3Score)
      Absolute_fst <- append(x = Absolute_fst, values = p4Score)
      Total_fst <- append(x = Total_fst, values = p5Score)
      Move1 <- append(x = Move1, values = p1Move)
      Move2 <- append(x = Move2, values = p2Move)
    }
    # La funcion devuelve los vectores de todos los ciclos
    return(data.frame(
      xn1 = xn1,
      xn2 = xn2,
      Relative_fst = Relative_fst,
      Absolute_fst = Absolute_fst,
      Total_fst = Total_fst,
      Move1 = Move1,
      Move2 = Move2
    ))
  })
}
