library(deSolve)


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


interaction_dynamic_LV<-function(p1Move,
                              p2Move,
                              parameters){
  with(as.list(c(p1Move,
                 p2Move,
                 parameters)),{
  
  if (p1Move == 1 & p2Move ==1) {
    
    #Definimos los parametros de interaccion
    parameters$a12 <- a12_1_1
    parameters$a21 <- a21_1_1
    parameters$r1 <- r1_1_1
    parameters$r2 <- r2_1_1
    
  }
  
  
  ##Escenario complementar/interferir##
  if (p1Move == 1 & p2Move ==2) {
    
    #Definimos los parametros de interaccion
    parameters$a12 <- a12_1_2
    parameters$a21 <- a21_1_2
    parameters$r1 <- r1_1_2
    parameters$r2 <- r2_1_2
    
   
  }
  
  
  ##Escenario interferir/complementar##
  if (p1Move == 2 & p2Move ==1) {
    
    #Definimos los parametros de interaccion
    parameters$a12 <- a12_2_1
    parameters$a21 <- a21_2_1
    parameters$r1 <- r1_2_1
    parameters$r2 <- r2_2_1
    
 
  }
  
  
  ##Escenario interferir/interferir##
  if (p1Move == 2 & p2Move ==2) {
    
    #Definimos los parametros de interaccion
    parameters$a12 <- a12_2_2
    parameters$a21 <- a21_2_2
    parameters$r1 <- r1_2_2
    parameters$r2 <- r2_2_2
   
  }
  return(parameters)
}) 
}


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

#####################################################################
###      Definimos la funcion game que hara la simulacion.      #####
#####################################################################


##Defino 'game' que integra todos los elementos seleccionados durante 'main' para que el programa realicé las interacciones##
game_LV <- function(funGrowth,
                    play1,
                    play2,
                    parameters){
  
  
  k1<-parameters$k1
  k2<-parameters$k2
  mutation1_LV<-parameters$mutation1_LV
  mutation2_LV<-parameters$mutation2_LV
  genome1_LV<-parameters$genome1_LV
  genome2_LV<-parameters$genome2_LV
  r1_1_1<-parameters$r1_1_1
  r2_1_1<-parameters$r2_1_1
  a12_1_1<-parameters$a12_1_1
  a21_1_1<-parameters$a21_1_1
  r1_1_2<-parameters$r1_1_2
  r2_1_2<-parameters$r2_1_2
  a12_1_2<-parameters$a12_1_2
  a21_1_2<-parameters$a21_1_2
  r1_2_1<-parameters$r1_2_1
  r2_2_1<-parameters$r2_2_1
  a12_2_1<-parameters$a12_2_1
  a21_2_1<-parameters$a21_2_1
  r1_2_2<-parameters$r1_2_2
  r2_2_2<-parameters$r2_2_2
  a12_2_2<-parameters$a12_2_2
  a21_2_2<-parameters$a21_2_2
  population1<-parameters$population1
  population2<-parameters$population2
  rounds_LV<-parameters$rounds_LV
  
  ##Valores iniciales de las dos poblaciones que componene a la cuasiespecies clasificando a los desertores como a todos aquellos virus que solo exiten teniendo que ser complementados y el reso siendo cooperadores##
  p1Score <- population1
  p2Score <- population2
  p3Score <- 0#population1/population2
  p4Score <- 1
  p5Score <- (population1/population2)
  
  
  ##Cinética de las interacciones quien mueve primero y quien despues##
  p1Move <- play1(p1Score, k1)
  p2Move <- play2(p2Score=p2Score, k2=k2)
  
  #Genero los vectores que guardaran los valores de cada ciclo
  xn1_LV <- p1Score
  xn2_LV <- p2Score
  Relative_fst_LV <- p3Score
  Absolute_fst_LV <- p4Score
  Total_fst_LV <- p5Score 
  
  #Cargamos los parametros del modelo

  parametro <- parameters[1:6]
  
  ##Se define el número de veces que se repiten las interacciones##
  for (i in 1:rounds_LV) {
    
    if (p1Move == 1 & p2Move ==1) {
      
      #Definimos los parametros de interaccion
      parametro$a12 <- a12_1_1
      parametro$a21 <- a21_1_1
      parametro$r1 <- r1_1_1
      parametro$r2 <- r2_1_1
      
      state <- c(x1 = as.numeric(p1Score), x2 = as.numeric(p2Score))
      tiempos <- seq(0, 1, by = 1) # sequence for the passage of time
      out <- deSolve::ode(func = funGrowth, y = state, times = tiempos, parms = parametro)
      
      #Guardamos las frecuencias de la siguiente generacion
      p1Score <- out[2,2]
      p2Score <- out[2,3]
      p3Score <- out[2,2]/out[2,3]
      p4Score <- ((out[2,2]/out[2,3])/(population1/population2))
      p5Score <- (Total_fst_LV[length(Total_fst_LV)]/(out[2,2]/out[2,3]))
    }
    
    
    ##Escenario complementar/interferir##
    if (p1Move == 1 & p2Move ==2) {
      
      #Definimos los parametros de interaccion
      parametro$a12 <- a12_1_2
      parametro$a21 <- a21_1_2
      parametro$r1 <- r1_1_2
      parametro$r2 <- r2_1_2
      
      state <- c(x1 = as.numeric(p1Score), x2 = as.numeric(p2Score))
      tiempos <- seq(0, 1, by = 1) # sequence for the passage of time
      out <- deSolve::ode(func = funGrowth, y = state, times = tiempos, parms = parametro)
      
      #Guardamos las frecuencias de la siguiente generacion
      p1Score <- out[2,2]
      p2Score <- out[2,3]
      p3Score <- out[2,2]/out[2,3]
      p4Score <- ((out[2,2]/out[2,3])/(population1/population2))
      p5Score <- (Total_fst_LV[length(Total_fst_LV)]/(out[2,2]/out[2,3]))
    }
    
    
    ##Escenario interferir/complementar##
    if (p1Move == 2 & p2Move ==1) {
      
      #Definimos los parametros de interaccion
      parametro$a12 <- a12_2_1
      parametro$a21 <- a21_2_1
      parametro$r1 <- r1_2_1
      parametro$r2 <- r2_2_1
      
      state <- c(x1 = as.numeric(p1Score), x2 = as.numeric(p2Score))
      tiempos <- seq(0, 1, by = 1) # sequence for the passage of time
      out <- deSolve::ode(func = funGrowth, y = state, times = tiempos, parms = parametro)
      
      #Guardamos las frecuencias de la siguiente generacion
      p1Score <- out[2,2]
      p2Score <- out[2,3]
      p3Score <- out[2,2]/out[2,3]
      p4Score <- ((out[2,2]/out[2,3])/(population1/population2))
      p5Score <- (Total_fst_LV[length(Total_fst_LV)]/(out[2,2]/out[2,3]))
    }
    
    
    ##Escenario interferir/interferir##
    if (p1Move == 2 & p2Move ==2) {
      
      #Definimos los parametros de interaccion
      parametro$a12 <- a12_2_2
      parametro$a21 <- a21_2_2
      parametro$r1 <- r1_2_2
      parametro$r2 <- r2_2_2
      parametro$k2 <- k2
      state <- c(x1 = as.numeric(p1Score), x2 = as.numeric(p2Score))
      tiempos <- seq(0, 1, by = 1) # sequence for the passage of time
      out <- deSolve::ode(func = funGrowth, y = state, times = tiempos, parms = parametro)
      
      #Guardamos las frecuencias de la siguiente generacion
      p1Score <- out[2,2]
      p2Score <- out[2,3]
      p3Score <- out[2,2]/out[2,3]
      p4Score <- ((out[2,2]/out[2,3])/(population1/population2))
      p5Score <- (Total_fst_LV[length(Total_fst_LV)]/(out[2,2]/out[2,3]))
    }
    
    #Vuelvo a calcular los move para el siguiente ciclo
    prevP1Move <- p1Move
    prevP2Move <- p2Move
    p1Move <- play1(prevP1Move,p1Score,k1 )
    p2Move <- play2(prevP2Move, p2Score,k2)
    
    #Guardo el valor del ciclo
    xn1_LV <- append(x = xn1_LV, values = p1Score)
    xn2_LV <- append(x = xn2_LV, values = p2Score)
    Relative_fst_LV <- append(x = Relative_fst_LV, values = p3Score)
    Absolute_fst_LV <- append(x = Absolute_fst_LV, values = p4Score)
    Total_fst_LV <- append(x = Total_fst_LV, values = p5Score)
    
  }
  #La funcion devuelve los vectores de todos los ciclos
  return(data.frame(xn1_LV = xn1_LV,xn2_LV = xn2_LV,
                    Relative_fst_LV = Relative_fst_LV, Absolute_fst_LV=Absolute_fst_LV, Total_fst_LV=Total_fst_LV))
}



##Defino 'game' que integra todos los elementos seleccionados durante 'main' para que el programa realicé las interacciones##
game_LV2 <- function(funGrowth,
                     play1,
                     play2,
                     population1,
                     population2,
                     generations,
                     parameters){
  
  
  k1<-parameters$k1
  k2<-parameters$k2
  mutation1_LV<-parameters$mutation1_LV
  mutation2_LV<-parameters$mutation2_LV
  genome1_LV<-parameters$genome1_LV
  genome2_LV<-parameters$genome2_LV
  r1_1_1<-parameters$r1_1_1
  r2_1_1<-parameters$r2_1_1
  a12_1_1<-parameters$a12_1_1
  a21_1_1<-parameters$a21_1_1
  r1_1_2<-parameters$r1_1_2
  r2_1_2<-parameters$r2_1_2
  a12_1_2<-parameters$a12_1_2
  a21_1_2<-parameters$a21_1_2
  r1_2_1<-parameters$r1_2_1
  r2_2_1<-parameters$r2_2_1
  a12_2_1<-parameters$a12_2_1
  a21_2_1<-parameters$a21_2_1
  r1_2_2<-parameters$r1_2_2
  r2_2_2<-parameters$r2_2_2
  a12_2_2<-parameters$a12_2_2
  a21_2_2<-parameters$a21_2_2
  

  ##Valores iniciales de las dos poblaciones que componene a la cuasiespecies clasificando a los desertores como a todos aquellos virus que solo exiten teniendo que ser complementados y el reso siendo cooperadores##
  p1Score <- population1
  p2Score <- population2
  p3Score <- 0#population1/population2
  p4Score <- 1
  p5Score <- (population1/population2)
  
  
  ##Cinética de las interacciones quien mueve primero y quien despues##
  p1Move <- play1(p1Score, k1)
  p2Move <- play2(p2Score=p2Score, k2=k2)
  
  #Genero los vectores que guardaran los valores de cada ciclo
  xn1_LV <- p1Score
  xn2_LV <- p2Score
  Relative_fst_LV <- p3Score
  Absolute_fst_LV <- p4Score
  Total_fst_LV <- p5Score 
  
  #Cargamos los parametros del modelo
  

  
  ##Se define el número de veces que se repiten las interacciones##
  for (i in 1:generations) {
    
    if (p1Move == 1 & p2Move ==1) {
      
      #Definimos los parametros de interaccion
      parameters$a12 <- a12_1_1
      parameters$a21 <- a21_1_1
      parameters$r1 <- r1_1_1
      parameters$r2 <- r2_1_1
      
      state <- c(x1 = as.numeric(p1Score), x2 = as.numeric(p2Score))
      tiempos <- seq(0, 1, by = 1) # sequence for the passage of time
      out <- deSolve::ode(func = funGrowth, y = state, times = tiempos, parms = parameters)
      
      #Guardamos las frecuencias de la siguiente generacion
      p1Score <- out[2,2]
      p2Score <- out[2,3]
      p3Score <- out[2,2]/out[2,3]
      p4Score <- ((out[2,2]/out[2,3])/(population1/population2))
      p5Score <- (Total_fst_LV[length(Total_fst_LV)]/(out[2,2]/out[2,3]))
    }
    
    
    ##Escenario complementar/interferir##
    if (p1Move == 1 & p2Move ==2) {
      
      #Definimos los parametros de interaccion
      parameters$a12 <- a12_1_2
      parameters$a21 <- a21_1_2
      parameters$r1 <- r1_1_2
      parameters$r2 <- r2_1_2
      
      state <- c(x1 = as.numeric(p1Score), x2 = as.numeric(p2Score))
      tiempos <- seq(0, 1, by = 1) # sequence for the passage of time
      out <- deSolve::ode(func = funGrowth, y = state, times = tiempos, parms = parameters)
      
      #Guardamos las frecuencias de la siguiente generacion
      p1Score <- out[2,2]
      p2Score <- out[2,3]
      p3Score <- out[2,2]/out[2,3]
      p4Score <- ((out[2,2]/out[2,3])/(population1/population2))
      p5Score <- (Total_fst_LV[length(Total_fst_LV)]/(out[2,2]/out[2,3]))
    }
    
    
    ##Escenario interferir/complementar##
    if (p1Move == 2 & p2Move ==1) {
      
      #Definimos los parametros de interaccion
      parameters$a12 <- a12_2_1
      parameters$a21 <- a21_2_1
      parameters$r1 <- r1_2_1
      parameters$r2 <- r2_2_1
      
      state <- c(x1 = as.numeric(p1Score), x2 = as.numeric(p2Score))
      tiempos <- seq(0, 1, by = 1) # sequence for the passage of time
      out <- deSolve::ode(func = funGrowth, y = state, times = tiempos, parms = parameters)
      
      #Guardamos las frecuencias de la siguiente generacion
      p1Score <- out[2,2]
      p2Score <- out[2,3]
      p3Score <- out[2,2]/out[2,3]
      p4Score <- ((out[2,2]/out[2,3])/(population1/population2))
      p5Score <- (Total_fst_LV[length(Total_fst_LV)]/(out[2,2]/out[2,3]))
    }
    
    
    ##Escenario interferir/interferir##
    if (p1Move == 2 & p2Move ==2) {
      
      #Definimos los parametros de interaccion
      parameters$a12 <- a12_2_2
      parameters$a21 <- a21_2_2
      parameters$r1 <- r1_2_2
      parameters$r2 <- r2_2_2
      parameters$k2 <- k2
      state <- c(x1 = as.numeric(p1Score), x2 = as.numeric(p2Score))
      tiempos <- seq(0, 1, by = 1) # sequence for the passage of time
      out <- deSolve::ode(func = funGrowth, y = state, times = tiempos, parms = parameters)
      
      #Guardamos las frecuencias de la siguiente generacion
      p1Score <- out[2,2]
      p2Score <- out[2,3]
      p3Score <- out[2,2]/out[2,3]
      p4Score <- ((out[2,2]/out[2,3])/(population1/population2))
      p5Score <- (Total_fst_LV[length(Total_fst_LV)]/(out[2,2]/out[2,3]))
    }
    
    #Vuelvo a calcular los move para el siguiente ciclo
    prevP1Move <- p1Move
    prevP2Move <- p2Move
    p1Move <- play1(prevP1Move,p1Score,k1 )
    p2Move <- play2(prevP2Move, p2Score,k2)
    
    #Guardo el valor del ciclo
    xn1_LV <- append(x = xn1_LV, values = p1Score)
    xn2_LV <- append(x = xn2_LV, values = p2Score)
    Relative_fst_LV <- append(x = Relative_fst_LV, values = p3Score)
    Absolute_fst_LV <- append(x = Absolute_fst_LV, values = p4Score)
    Total_fst_LV <- append(x = Total_fst_LV, values = p5Score)
    
  }
  #La funcion devuelve los vectores de todos los ciclos
  return(data.frame(xn1_LV = xn1_LV,xn2_LV = xn2_LV,
                    Relative_fst_LV = Relative_fst_LV, Absolute_fst_LV=Absolute_fst_LV, Total_fst_LV=Total_fst_LV))
}


##Defino 'game' que integra todos los elementos seleccionados durante 'main' para que el programa realicé las interacciones##
game_LV3 <- function(funGrowth,
                     play1,
                     play2,
                     population1,
                     population2,
                     generations,
                     parameters){
  
  with(as.list(c(funGrowth,
                 play1,
                 play2,
                 population1,
                 population2,
                 generations,
                 parameters)),{  
  
  
  ##Valores iniciales de las dos poblaciones que componene a la cuasiespecies clasificando a los desertores como a todos aquellos virus que solo exiten teniendo que ser complementados y el reso siendo cooperadores##
  p1Score <- population1
  p2Score <- population2
  p3Score <- 0#population1/population2
  p4Score <- 1
  p5Score <- (population1/population2)
  
  
  ##Cinética de las interacciones quien mueve primero y quien despues##
  p1Move <- play1(p1Score, k1)
  p2Move <- play2(p2Score=p2Score, k2=k2)
  
  #Genero los vectores que guardaran los valores de cada ciclo
  xn1_LV <- p1Score
  xn2_LV <- p2Score
  Relative_fst_LV <- p3Score
  Absolute_fst_LV <- p4Score
  Total_fst_LV <- p5Score 
  
  #Cargamos los parametros del modelo
  
  
  
  ##Se define el número de veces que se repiten las interacciones##
  for (i in 1:generations) {
    
    if (p1Move == 1 & p2Move ==1) {
      
      #Definimos los parametros de interaccion
      parameters$a12 <- a12_1_1
      parameters$a21 <- a21_1_1
      parameters$r1 <- r1_1_1
      parameters$r2 <- r2_1_1
      
      state <- c(x1 = as.numeric(p1Score), x2 = as.numeric(p2Score))
      tiempos <- seq(0, 1, by = 1) # sequence for the passage of time
      out <- deSolve::ode(func = funGrowth, y = state, times = tiempos, parms = parameters)
      
      #Guardamos las frecuencias de la siguiente generacion
      p1Score <- out[2,2]
      p2Score <- out[2,3]
      p3Score <- out[2,2]/out[2,3]
      p4Score <- ((out[2,2]/out[2,3])/(population1/population2))
      p5Score <- (Total_fst_LV[length(Total_fst_LV)]/(out[2,2]/out[2,3]))
    }
    
    
    ##Escenario complementar/interferir##
    if (p1Move == 1 & p2Move ==2) {
      
      #Definimos los parametros de interaccion
      parameters$a12 <- a12_1_2
      parameters$a21 <- a21_1_2
      parameters$r1 <- r1_1_2
      parameters$r2 <- r2_1_2
      
      state <- c(x1 = as.numeric(p1Score), x2 = as.numeric(p2Score))
      tiempos <- seq(0, 1, by = 1) # sequence for the passage of time
      out <- deSolve::ode(func = funGrowth, y = state, times = tiempos, parms = parameters)
      
      #Guardamos las frecuencias de la siguiente generacion
      p1Score <- out[2,2]
      p2Score <- out[2,3]
      p3Score <- out[2,2]/out[2,3]
      p4Score <- ((out[2,2]/out[2,3])/(population1/population2))
      p5Score <- (Total_fst_LV[length(Total_fst_LV)]/(out[2,2]/out[2,3]))
    }
    
    
    ##Escenario interferir/complementar##
    if (p1Move == 2 & p2Move ==1) {
      
      #Definimos los parametros de interaccion
      parameters$a12 <- a12_2_1
      parameters$a21 <- a21_2_1
      parameters$r1 <- r1_2_1
      parameters$r2 <- r2_2_1
      
      state <- c(x1 = as.numeric(p1Score), x2 = as.numeric(p2Score))
      tiempos <- seq(0, 1, by = 1) # sequence for the passage of time
      out <- deSolve::ode(func = funGrowth, y = state, times = tiempos, parms = parameters)
      
      #Guardamos las frecuencias de la siguiente generacion
      p1Score <- out[2,2]
      p2Score <- out[2,3]
      p3Score <- out[2,2]/out[2,3]
      p4Score <- ((out[2,2]/out[2,3])/(population1/population2))
      p5Score <- (Total_fst_LV[length(Total_fst_LV)]/(out[2,2]/out[2,3]))
    }
    
    
    ##Escenario interferir/interferir##
    if (p1Move == 2 & p2Move ==2) {
      
      #Definimos los parametros de interaccion
      parameters$a12 <- a12_2_2
      parameters$a21 <- a21_2_2
      parameters$r1 <- r1_2_2
      parameters$r2 <- r2_2_2
      parameters$k2 <- k2
      state <- c(x1 = as.numeric(p1Score), x2 = as.numeric(p2Score))
      tiempos <- seq(0, 1, by = 1) # sequence for the passage of time
      out <- deSolve::ode(func = funGrowth, y = state, times = tiempos, parms = parameters)
      
      #Guardamos las frecuencias de la siguiente generacion
      p1Score <- out[2,2]
      p2Score <- out[2,3]
      p3Score <- out[2,2]/out[2,3]
      p4Score <- ((out[2,2]/out[2,3])/(population1/population2))
      p5Score <- (Total_fst_LV[length(Total_fst_LV)]/(out[2,2]/out[2,3]))
    }
    
    #Vuelvo a calcular los move para el siguiente ciclo
    prevP1Move <- p1Move
    prevP2Move <- p2Move
    p1Move <- play1(prevP1Move,p1Score,k1 )
    p2Move <- play2(prevP2Move, p2Score,k2)
    
    #Guardo el valor del ciclo
    xn1_LV <- append(x = xn1_LV, values = p1Score)
    xn2_LV <- append(x = xn2_LV, values = p2Score)
    Relative_fst_LV <- append(x = Relative_fst_LV, values = p3Score)
    Absolute_fst_LV <- append(x = Absolute_fst_LV, values = p4Score)
    Total_fst_LV <- append(x = Total_fst_LV, values = p5Score)
    
  }
  #La funcion devuelve los vectores de todos los ciclos
  return(data.frame(xn1_LV = xn1_LV,xn2_LV = xn2_LV,
                    Relative_fst_LV = Relative_fst_LV, Absolute_fst_LV=Absolute_fst_LV, Total_fst_LV=Total_fst_LV))
                 })
}



##Defino 'game' que integra todos los elementos seleccionados durante 'main' para que el programa realicé las interacciones##
game_LV4 <- function(funGrowth,
                     play1,
                     play2,
                     population1,
                     population2,
                     generations,
                     interaction,
                     parameters){
  
  with(as.list(c(funGrowth,
                 play1,
                 play2,
                 population1,
                 population2,
                 generations,
                 parameters)),{  
                   
                   # TODO
                   # add check step to control that funGrowth`s parameters
                   # are included in parameters
                   
                   # TODO
                   # add check step of population1,population2, generations
                   
                   # TODO
                   # Discuss possible checks for parameters (How to deal
                   # with unknown parameters the user would include?)
                   # set a maximum number possible?
                   
                   ## initial values of the two populations that
                   ## compose deserters (need help) and helpers (not deserters) 
                   
                   p1Score <- population1
                   p2Score <- population2
                   p3Score <- 0#population1/population2
                   p4Score <- 1
                   p5Score <- (population1/population2)
                   
                   
                   ##Cinética de las interacciones quien mueve primero y quien despues##
                   p1Move <- play1(p1Score, k1)
                   p2Move <- play2(p2Score, k2)
                   
                   #Genero los vectores que guardaran los valores de cada ciclo
                   xn1_LV <- p1Score
                   xn2_LV <- p2Score
                   Relative_fst_LV <- p3Score
                   Absolute_fst_LV <- p4Score
                   Total_fst_LV <- p5Score 
                   
                   
                   ##Se define el número de veces que se repiten las interacciones##
                   for (i in 1:generations) {
                     
                     # change parameter according to the interaction inthis generation
                     parameters<-interaction_dynamic(p1Move,
                                         p2Move,
                                         parameters)  
                     #  Compute population dynamics for this parameters
                     state <- c(x1 = as.numeric(p1Score), x2 = as.numeric(p2Score))
                     tiempos <- seq(0, 1, by = 1) # sequence for the passage of time
                     out <- deSolve::ode(func = funGrowth, y = state, times = tiempos, parms = parameters)
                       
                     #  change parameters for the next generation
                     p1Score <- out[2,2]
                     p2Score <- out[2,3]
                     p3Score <- out[2,2]/out[2,3]
                     p4Score <- ((out[2,2]/out[2,3])/(population1/population2))
                     p5Score <- (Total_fst_LV[length(Total_fst_LV)]/(out[2,2]/out[2,3]))
                    
                     #Vuelvo a calcular los move para el siguiente ciclo
                     prevP1Move <- p1Move
                     prevP2Move <- p2Move
                     p1Move <- play1(prevP1Move,p1Score,k1 )
                     p2Move <- play2(prevP2Move, p2Score,k2)
                     
                     #Guardo el valor del ciclo
                     xn1_LV <- append(x = xn1_LV, values = p1Score)
                     xn2_LV <- append(x = xn2_LV, values = p2Score)
                     Relative_fst_LV <- append(x = Relative_fst_LV, values = p3Score)
                     Absolute_fst_LV <- append(x = Absolute_fst_LV, values = p4Score)
                     Total_fst_LV <- append(x = Total_fst_LV, values = p5Score)
                     
                   }
                   #La funcion devuelve los vectores de todos los ciclos
                   return(data.frame(xn1_LV = xn1_LV,xn2_LV = xn2_LV,
                                     Relative_fst_LV = Relative_fst_LV, Absolute_fst_LV=Absolute_fst_LV, Total_fst_LV=Total_fst_LV))
                 })
}
##########################################################################
#####         Definimos los parametros del modelo LV                ######
##########################################################################

parametros <- list(k1 = 1, # population 1 carrying capacity
                   k2 = 1,# population 2 carrying capacity
                   mutation1 = 10**-6,
                   mutation2 = 10**-6,
                   genome1 = 5000,
                   genome2 = 8500,
                   #(1:1) cooperate/cooperate
                   r1_1_1=0.9,
                   r2_1_1=0.19,
                   a12_1_1=0.012,
                   a21_1_1=-0.2,
                   #(1:2) cooperate/defective
                   r1_1_2=0.1,
                   r2_1_2=0.0018,
                   a12_1_2=-1,
                   a21_1_2=0.1,
                   #(2:1) defective/cooperate
                   r1_2_1=0.15,#0.04-0.03-0.08-0.1
                   r2_2_1=0.18,#0.750.028--0.4
                   a12_2_1=-1,#0.98
                   a21_2_1=-0.001,#-0.5
                   #(2:2) defective/defective
                   r1_2_2=0.8,
                   r2_2_2=-0.8,
                   a12_2_2=-0.1,
                   a21_2_2=-0.1
) 

#####################################################################
#                    Funcion play para LV                     #######
#####################################################################


##########################################################################
########              Ejecutamos la simulacion                      ######
##########################################################################
#Lotka-volterra

stgr1<-check_strategy("Count_defective")
stgr2<-check_strategy("Count_defective")

# simulation_lv1 <- game_LV3(funGrowth=LotkaVolt, #Growth function to pass to ode solver
#                           play1=alwfunc_LV, #strategy of player 1
#                           play2=alwfunc_LV, #strategy of player 2
#                           generations= 480, #number of generations
#                           population1= 0.1,
#                           population2= 0.22,
#                           parameters=parametros #list of parameters to pass to ode solver
# )  
# simulation_lv2 <- game_LV3(funGrowth=LotkaVolt, #Growth function to pass to ode solver
#                            play1=stgr1, #strategy of player 1
#                            play2=alwfunc_LV, #strategy of player 2
#                            generations= 480, #number of generations
#                            population1= 0.1,
#                            population2= 0.22,
#                            parameters=parametros #list of parameters to pass to ode solver
# ) 


simulation_lv4 <- game_LV4(funGrowth=LotkaVolt, #Growth function to pass to ode solver
                           play1=alwdeff, #strategy of player 1
                           play2=alwdeff, #strategy of player 2
                           interaction=interaction_dynamic, # interaction function
                           generations= 480, #number of generations
                           population1= 0.1, #proportion of population1
                           population2= 0.1, #proportion of population2
                           parameters=parametros #list of parameters to pass to ode solver
) 

(simulation_lv1==simulation_lv4)

head(simulation_lv4)








