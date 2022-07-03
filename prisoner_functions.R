library(deSolve)




#################################
#

## Random effective strategy
randeff <- function(val = sample(1:5, 1),...){
  if(val>3) {
    2}
  else {1}}




## Random deffective strategy 
## Inverts Randeff
randdeff <- function(...){
  (tirada = randeff())
  if (tirada == 1)
  {return(2)}
  else{return(1)}}



## Random basic strategy
randbasic <- function(...){
  sample(1:2,1)
}



## Always deffective strategy
alwdeff <- function(...){
  2
}


## Always funtional strategy

alwfunc <- function(...){
  1
}



## Particle counter strategy

count_def <- function(score,k,...){
  
  if (score >= k*0.5) {
    2
  }else {
    1
  }
}

