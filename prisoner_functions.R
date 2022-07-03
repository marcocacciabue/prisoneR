library(deSolve)




#####################################################################
#####           Definimos la funcion de Lotka-volterra          #####
#####################################################################

##Defino 'game' que integra todos los elementos seleccionados durante 'main' para que el programa realicé las interacciones##
game <- function(funGrowth,
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
                   
                   
                   
                   
                   
                   # TODO
                   #ask if population here is the same as frequency in may
                   
                   ## initial values of the two populations that
                   ## compose deserters (need help) and helpers (not deserters) 
                   
                   p1Score <- population1
                   p2Score <- population2
                   
                   
                   # TODO
                   # ask difference of p3Score here vs in may function
                   p3Score <- 0#population1/population2
                   p4Score <- 1
                   p5Score <- (population1/population2)
                   

                   # TODO  
                   #'* IMPORTANT!  * 
                   # decide what form of parameters we will pass to the 
                   # strategy function in order to make it the more general and flexible
                   # as possible.
                   # passing all parameters is a viable option??
                   # the same goes to growth function
                   
 

                   ##Cinética de las interacciones quien mueve primero y quien despues##
                   p1Move <- play1(p1Score, k1)
                   p2Move <- play2(p2Score, k2)
                   
                   #Genero los vectores que guardaran los valores de cada ciclo
                   xn1 <- p1Score
                   xn2 <- p2Score
                   Relative_fst <- p3Score
                   Absolute_fst <- p4Score
                   Total_fst <- p5Score 
                   
                   
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
                     p5Score <- (Total_fst[length(Total_fst)]/(out[2,2]/out[2,3]))
                     
                     #Vuelvo a calcular los move para el siguiente ciclo
                     prevP1Move <- p1Move
                     prevP2Move <- p2Move
                     p1Move <- play1(prevP1Move,p1Score,k1 )
                     p2Move <- play2(prevP2Move, p2Score,k2)
                     
                     #Guardo el valor del ciclo
                     xn1 <- append(x = xn1, values = p1Score)
                     xn2 <- append(x = xn2, values = p2Score)
                     Relative_fst <- append(x = Relative_fst, values = p3Score)
                     Absolute_fst <- append(x = Absolute_fst, values = p4Score)
                     Total_fst <- append(x = Total_fst, values = p5Score)
                     
                   }
                   #La funcion devuelve los vectores de todos los ciclos
                   return(data.frame(xn1 = xn1,
                                     xn2 = xn2,
                                     Relative_fst = Relative_fst, 
                                     Absolute_fst = Absolute_fst, 
                                     Total_fst = Total_fst))
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



##########################################################################
########              Ejecutamos la simulacion                      ######
##########################################################################
#Lotka-volterra

stgr1<-check_strategy("Count_defective")
stgr2<-check_strategy("Count_defective")

simulation <- game(funGrowth=LotkaVolt, #Growth function to pass to ode solver
                           play1=alwdeff, #strategy of player 1
                           play2=alwdeff, #strategy of player 2
                           interaction=interaction_dynamic, # interaction function
                           generations= 480, #number of generations
                           population1= 0.1, #proportion of population1
                           population2= 0.1, #proportion of population2
                           parameters=parametros #list of parameters to pass to ode solver
) 



head(simulation_lv4)

