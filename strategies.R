

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

# this function is optional, the user can pass directly the strategy
# of choice

check_strategy<-function(strategy='Alwaysfunctional'){
  if (strategy == 'Alwaysfunctional') {
    play <- alwfunc
  }
  if (strategy == 'Alwaysdefectiveinterfering') {
    play <- alwdeff
  }
  if (strategy == 'Randommodification') {
    play <- randbasic
  }
  if (strategy== 'Randomeffective') {
    play
  }
  if (strategy == 'Randomdefectiveinterfering') {
    play <- randdeff
  }
  
  if (strategy == 'Count_defective') {
    play <- count_def
  }
  return(play)
}

# define a new function that handles the interaction dynamics
