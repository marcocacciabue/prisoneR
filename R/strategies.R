


#' Random Effective function
#'
#' @param ... list: extra parameters passed from `game()` function
#'
#' @return Numeric: 1 or 2 for cooperation or not.
#'
randeff <- function(...) {
  val <- sample(1:5, 1)
  if (val > 3) {
    2
  } else {
    1
  }
}






#' Random deffective strategy
#'
#' Inverts Randeff
#'
#' @param ... list: extra parameters passed from `game()` function
#'
#' @return Numeric: 1 or 2 for cooperation or not.
#'
randdeff <- function(...) {
  (tirada <- randeff())
  if (tirada == 1) {
    2
  } else {
    1
  }
}




#' Random basic strategy
#'
#' @param ... list: extra parameters passed from `game()` function
#'
#' @return Numeric: 1 or 2 for cooperation or not.
#'
randbasic <- function(...) {
  sample(1:2, 1)
}



#' Always defective strategy
#'
#' @param ... list: extra parameters passed from `game()` function
#'
#' @return Numeric: 2 for NO cooperation
#'
alwdeff <- function(...) {
  2
}


#' Always functional strategy
#'
#' @param ... list: extra parameters passed from `game()` function
#'
#' @return Numeric: 1 for cooperation
#'
alwfunc <- function(...) {
  1
}




#' Particle counter strategy
#'
#' @param prev Numeric: previous decision.
#' @param score Numeric: previous condition.
#' @param k Numeric: the maximum system capacity for population
#' @param delta Numeric: proportion of k to evaluate score. (score >= k * delta)
#' @param ... list: extra parameters passed from `game()` function
#'
#' @return Numeric: 1 or 2 for cooperation or not.
count_def <- function(prev, score, k, delta, ...) {
  with(as.list(c(
    prev,
    score,
    k,
    delta
  )), {
    if (is.na(score)){
      return(1)
    }else{
    if (score >= k * delta) {
      2
    } else {
      1
    }
    }
  })
}


#' Check strategy
#'
#' @param strategy String: must be one of the following:
#' 'Alwaysfunctional', 'Alwaysdefectiveinterfering',
#'  'Randommodification', 'Randomeffective',
#'  'Randomdefectiveinterfering' or 'Count_defective'
#'
#' @return function: strategy to define next movement
#'
check_strategy <- function(strategy = "Alwaysfunctional") {
  if (strategy == "Alwaysfunctional") {
    play <- alwfunc
  }
  if (strategy == "Alwaysdefectiveinterfering") {
    play <- alwdeff
  }
  if (strategy == "Randommodification") {
    play <- randbasic
  }
  if (strategy == "Randomeffective") {
    play <- randeff
  }
  if (strategy == "Randomdefectiveinterfering") {
    play <- randdeff
  }

  if (strategy == "Count_defective") {
    play <- count_def
  }
  return(play)
}
