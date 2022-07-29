library(deSolve)




#####################################################################
#####           Definimos la funcion de Lotka-volterra          #####
#####################################################################

#' game simulation
#' 
#' `game()` returns a data.frame with the Fst values for the simulated
#' interactions between two populations of viral particles.
#'
#'
#' @param type 
#' @param funGrowth 
#' @param play1 
#' @param play2 
#' @param interaction 
#' @param input1 
#' @param input2 
#' @param generations 
#' @param k1 
#' @param k2 
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
game <- function(    type=c("May","Lotka","Custom"),
                     funGrowth, #growth function
                     play1=alwdeff, #strategy of player 1
                     play2=alwdeff, #strategy of player 2
                     interaction, # interaction function
                     input1 = NULL, #initial condition for player 1
                     input2 = NULL, #initial condition for player 2
                     generations = NULL, #number of generations
                     k1=NULL,
                     k2=NULL,
                     parameters #list of parameters to pass to ode solver
                     ){
  
  #check type parameter
  if(is.null(type)|missing(type))
    stop("'type' must be set and must be one of the following: May Lotka or Custom")
  type<-match.arg(type)
  
  #handle functions in case type = "Custom"
  if(type== "Custom"){
    if(missing(funGrowth)|is.null(funGrowth)|!is.function(funGrowth)){
      stop("'funGrowth' should be given as a function.You can try preloaded LV or MAY accordingly")
    }
    if(missing(interaction)|is.null(interaction)|!is.function(interaction))
      stop("'interaction' should be given as a function.You can try preloaded interaction_dynamic_MAY or interaction_dynamic_LV accordingly")
  }
  
  funGrowth<- switch(type, 
                     "May" = MAY, 
                     "Lotka" = LV, 
                     "Custom" = funGrowth)
  interaction<- switch(type, 
                       "May" = interaction_dynamic_MAY, 
                       "Lotka" = interaction_dynamic_LV, 
                       "Custom" = interaction)
  
  if(is.null(play1))
    print("Player 1 strategy is set to Alwaysfunctional")
    play1<-alwfunc
  if(!is.function(play1))
    stop("'play1' should be given as a function")
  if(is.null(play2))
    print("Player 2 strategy is set to Alwaysfunctional")
    play2<-alwfunc
  if(!is.function(play2))
    stop("'play2' should be given as a function")
  

  #check if parameters is a list
  if(!is.list(parameters)|is.null(parameters)) stop("'parameters' should be given as a list of values")
  
  #check if no duplication of arguments are given in parameters and individuals arguments
  if (!is.null(generations) & "generations" %in% names(parameters))
    stop("If 'parameters' is a list that contains generations, argument 'generations' should be NULL")
  if (!is.null(input1) & "input1" %in% names(parameters))
    stop("If 'parameters' is a list that contains input1, argument 'input1' should be NULL")
  if (!is.null(input2) & "input2" %in% names(parameters))
    stop("If 'parameters' is a list that contains input2, argument 'input2' should be NULL")
  if (!is.null(k1) & "k1" %in% names(parameters))
    stop("If 'parameters' is a list that contains k1, argument 'k1' should be NULL")
  if (!is.null(k2) & "k2" %in% names(parameters))
    stop("If 'parameters' is a list that contains k2, argument 'k2' should be NULL")
  
  
  # define corresponding arguments given in parameters 
  if (!is.null(parameters$input1)) input1 <- parameters$input1
  if (!is.null(parameters$input2)) input2 <- parameters$input2
  if (!is.null(parameters$generations)) generations <- parameters$generations
  if (!is.null(parameters$k1)) k1 <- parameters$k1
  if (!is.null(parameters$k2))  k2 <- parameters$k2
  
  # Define general values in case no arguments are given.  
  if (is.null(input1) & !("input1" %in% names(parameters))) input1 <- 0.1 
  if (is.null(input2) & !("input2" %in% names(parameters))) input2 <- 0.1 
  if (is.null(generations) & !("generations" %in% names(parameters))) generations <- 50
  if (is.null(k1) & !("k1" %in% names(parameters))) k1 <- 1 
  if (is.null(k2) & !("k2" %in% names(parameters))) k2 <- 1  
  
  
    
  #check if relevant integers are given  
  if(!is.numeric(input1))
    stop("'input1' should be given as an numeric")
  
  if(!is.numeric(input2))
    stop("'input2' should be given as an numeric")
    
  if((!generations == round(generations))|(generations>1000)|(generations<2))
    stop("'generations' should be given as an interger of values between 2 and 1000")
    
  if(!is.numeric(k1))
    stop("'k1' should be given as an numeric")
  if(!is.numeric(k2))
    stop("'k2' should be given as an numeric")
  
  with(as.list(c(funGrowth,
                 play1,
                 play2,
                 input1,
                 input2,
                 k1,
                 k2,
                 generations,
                 parameters)),{    

                  print(k1)
                  print(parameters$k1)
                  print(input1)
                  print(input2)
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
                   p5Score <- (input1/input2)
                   

                   
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
                     
                     # change parameter according to the interaction in this generation
                     parameters<-interaction(p1Move,
                                             p2Move,
                                             parameters)  
                     
                     out<-funGrowth(p1Score,p2Score,parameters)
                     
                     #  change parameters for the next generation
                     p1Score <- out[1]
                     p2Score <- out[2]
                     p3Score <- out[1]/out[2]
                     p4Score <- ((out[1]/out[2])/(input1/input2))
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

parametros <- list(mutation1 = 10**-6,
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
                   H2=NULL,
                   k1=20,
                   input1=30
) 



##########################################################################
########              Ejecutamos la simulacion                      ######
##########################################################################
#Lotka-volterra

stgr1<-check_strategy("Count_defective")
stgr2<-check_strategy("Count_defective")
set.seed(20)
L<-"a"
simulationLV <- game(      type="May",
                           #Growth function to pass to ode solver
                           play1=NULL, #strategy of player 1
                           play2=NULL, #strategy of player 2
                           interaction=interaction_dynamic_MAY, # interaction function
                           parameters=parametros #list of parameters to pass to ode solver
) 


simulationMAY <- game(       type="May",
                             funGrowth=MAY, #Growth function to pass to ode solver
                             play1=count_def, #strategy of player 1
                             play2=count_def, #strategy of player 2
                             interaction=interaction_dynamic_MAY, # interaction function
                             generations= 480, #number of generations
                             parameters=parametros #list of parameters to pass to ode solver
) 

as.character(LV)

head(simulationMAY)

try(LV(p1Score=1,p2Score=3,as.list(parametros))
    )

environment(simulationLV)
trace(.last_env)
ls()
.last_env
base::eval(MAY)
interaction_dynamic_MAY()
print(as.list(environment(), all=TRUE))

attr(LV,"function")

eval(substitute(LV),parametros) 
  objeto 'a21_2_2' no encontrado 
  
  deSolve::ode
assign
#> y <- x * 10