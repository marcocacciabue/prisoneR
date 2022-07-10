

interaction_dynamic_MAY<-function(p1Move,
                              p2Move,
                              parameters){
  
  with(as.list(c(p1Move,
                 p2Move,
                 parameters)),{
                   
                   if (p1Move == 1 & p2Move ==1) {
                     
                     parameters$H1 <- 0.57
                     parameters$H2 <- 0.50
                     parameters$r1 <- r1_1_1
                     parameters$r2 <- r2_1_1
                   }
                   
                   
                   ##Escenario complementar/interferir##
                   if (p1Move == 1 & p2Move ==2) {
                     parameters$H1 <-0.58 
                     parameters$H2 <-0.509
                     parameters$r1 <- r1_1_2
                     parameters$r2 <- r2_1_2
                     
                     
                   }
                   
                   
                   ##Escenario interferir/complementar##
                   if (p1Move == 2 & p2Move ==1) {
                     parameters$H1 <-0.57
                     parameters$H2 <-0.50

                     parameters$r1 <- r1_2_1
                     parameters$r2 <- r2_2_1
                     
                     
                   }
                   
                   
                   ##Escenario interferir/interferir##
                   if (p1Move == 2 & p2Move ==2) {
                     parameters$H1 <- 0.68
                     parameters$H2 <- 0.6

                     parameters$r1 <- r1_2_2
                     parameters$r2 <- r2_2_2
                     
                   }
                   return(parameters)
                 }) 
}




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
