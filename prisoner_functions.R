library(deSolve)




#####################################################################
#####           Definimos la funcion de Lotka-volterra          #####
#####################################################################

#' game simulation
#' 
#' `game()` returns a data.frame with the Fst values for the simulated
#' interactions between defective and helper viral particles.
#'
#' @param funGrowth 
#' @param play1 
#' @param play2 
#' @param population1 
#' @param population2 
#' @param generations 
#' @param interaction 
#' @param parameters 
#'
#' @return A data frame with the obtained scores. The output has the following columns:
#' * 
#'   
#' 
#' 
#' @export
#'
#' @examples
game <- function(    funGrowth, #growth function
                     play1, #strategy of player 1
                     play2, #strategy of player 2
                     population1, #proportion of population1
                     population2, #proportion of population2
                     generations, #number of generations
                     interaction, # interaction function
                     parameters #list of parameters to pass to ode solver
                     ){
  
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
                   p3Score <- 0
                   p4Score <- 1
                   p5Score <- (population1/population2)
                   

                   
                   # establish first interaction according to the strategy of each player
                   # prev=NULL because this is the first interaction. If strategy is dependent 
                   # on the previous interaction the corresponding function should deal 
                   # correctly with the first value)
                   
                   p1Move <- play1(prev=NULL,score=p1Score,k=k1)
                   p2Move <- play2(prev=NULL,score=p2Score,k=k2)
                   
                   
                   #Genero los vectores que guardaran los valores de cada ciclo
                   xn1 <- p1Score
                   xn2 <- p2Score
                   Relative_fst <- p3Score
                   Absolute_fst <- p4Score
                   Total_fst <- p5Score 
                   
                   
                   ##Se define el número de veces que se repiten las interacciones##
                   for (i in 1:generations) {
                     
                     # change parameter according to the interaction inthis generation
                     parameters<-interaction(p1Move,
                                             p2Move,
                                             parameters)  
                     
                     out<-funGrowth(p1Score,p2Score,parameters)
                     
                     #  change parameters for the next generation
                     p1Score <- out[1]
                     p2Score <- out[2]
                     p3Score <- out[1]/out[2]
                     p4Score <- ((out[1]/out[2])/(population1/population2))
                     p5Score <- (Total_fst[length(Total_fst)]/(out[1]/out[2]))
                     
                     #Vuelvo a calcular los move para el siguiente ciclo
                     prevP1Move <- p1Move
                     prevP2Move <- p2Move
                     p1Move <- play1(prev=prevP1Move,score=p1Score,k=k1)
                     p2Move <- play2(prev=prevP2Move,score=p2Score,k=k2)
                     
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
                   a21_2_2=-0.1,
                   H1=NULL,
                   H2=NULL
) 



##########################################################################
########              Ejecutamos la simulacion                      ######
##########################################################################
#Lotka-volterra

stgr1<-check_strategy("Count_defective")
stgr2<-check_strategy("Count_defective")
set.seed(20)
simulationLV <- game(        funGrowth=LV_general, #Growth function to pass to ode solver
                           play1=count_def, #strategy of player 1
                           play2=randdeff, #strategy of player 2
                           interaction=interaction_dynamic_LV, # interaction function
                           generations= 480, #number of generations
                           population1= 0.2, #proportion of population1
                           population2= 0.1, #proportion of population2
                           parameters=parametros #list of parameters to pass to ode solver
) 

simulationMAY <- game(        funGrowth=MAY_general, #Growth function to pass to ode solver
                             play1=count_def, #strategy of player 1
                             play2=count_def, #strategy of player 2
                             interaction=interaction_dynamic_MAY, # interaction function
                             generations= 480, #number of generations
                             population1= 0.2, #proportion of population1
                             population2= 0.1, #proportion of population2
                             parameters=parametros #list of parameters to pass to ode solver
) 


head(simulationMAY)
