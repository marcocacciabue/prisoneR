


#' Random Effective function
#'
#' @param ... list: extra parameters passed from `game()` function
#'
#' @return Numeric: 1 or 2 for cooperation or not.
#'
#' @export
#' @examples
#' nextmove<-randeff()
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
#' @export
#' @examples
#' nextmove<-randdeff()
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
#' @export
#' @examples
#' nextmove<-randbasic()
#'
randbasic <- function(...) {
  sample(1:2, 1)
}



#' Always defective strategy
#'
#' @param ... list: extra parameters passed from `game()` function
#'
#' @return Numeric: 2 for NO cooperation
#' @export
#' @examples
#' nextmove<-alwdeff()
#'
alwdeff <- function(...) {
  2
}


#' Always functional strategy
#'
#' @param ... list: extra parameters passed from `game()` function
#'
#' @return Numeric: 1 for cooperation
#' @export
#' @examples
#' nextmove<-alwfunc()
#' nextmove
alwfunc <- function(...) {
  1
}




#' Particle counter strategy
#'
#'
#' @param prev logical: previous population move
#' @param Score Numeric: previous condition
#' @param k Numeric: system capacity
#' @param delta Numeric: factor
#' @param ... list: extra parameters passed from `game()` function
#'
#' @return Numeric: 1 or 2 for cooperation or not.
#' @export
#' @examples
#' nextmove<-count_def(prev=1,
#' Score=0.5,
#' k=1,
#' delta=0.3,
#' )
#' nextmove
count_def <- function(prev,
                      Score,
                      k,
                      delta,
                      ...) {
  with(as.list(c(
    prev,
    Score,
    k,
    delta
  )), {
    if (is.na(Score)){
      return(1)
    }else{
    if (Score <= k * delta) {
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
