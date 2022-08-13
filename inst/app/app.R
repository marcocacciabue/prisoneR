
# Cargamos las librerias
library(deSolve)
# library(tidyverse)
library(ggplot2)
library(shiny)
library(prisoneR)
library(DT)
# library(tidyr)



ui <- fluidPage(
  titlePanel("The prisioner dilema simulation strategy"),
  sidebarLayout(
    sidebarPanel(
      selectInput('Model', 'Model', c('Lokta_Volterra','logistic_May')),
      sliderInput('rounds', 'Generations', 10, min = 1, max = 10000, step=1),
      selectInput('strategy1', 'strategy1', c('Alwaysfunctional','Alwaysdefectiveinterfering','titForTat','Randommodification','Randomeffective','Randomdefectiveinterfering')),
      selectInput('strategy2', 'strategy2', c('Alwaysfunctional','Alwaysdefectiveinterfering','titForTat','Randommodification','Randomeffective','Randomdefectiveinterfering')),
      sliderInput('input1', 'Initial condition population 1', 1, min = 0.0001, max = 1000,step=0.1),
      sliderInput('input2', 'Initial condition population 2', 2, min = 0.0001, max = 1000,step=0.1),
      sliderInput('seed', 'change seed. Only affects random strategies (optional)', 20, min = 1, max = 300,step=1),
      selectInput('mutation1', 'Mutation rate 1', c(10**0,10**-1,10**-2,10**-3,10**-4,10**-5,10**-6)),
      selectInput('mutation2', 'Mutation rate 2', c(10**0,10**-1,10**-2,10**-3,10**-4,10**-5,10**-6)),
      sliderInput('genome1', 'Genome size 1', 8500, min = 100, max = 10000000,step=100),
      sliderInput('genome2', 'Genome size 2', 8500, min = 100, max = 10000000,step=100),


      # Only show this panel if the model is MAY
      conditionalPanel(
        condition = "input.Model == 'logistic_May'",
        sliderInput('r', 'R (The growth rate)', 2, min = 0, max = 4,step=0.01)
        ),
      # Only show this panel if the model is LV
      conditionalPanel(
        condition = "input.Model == 'Lokta_Volterra'",
        sliderInput('k1', 'k1', 1000, min = 100, max = 100000,step=100),
        sliderInput('k2', 'k2', 1000, min = 100, max = 100000,step=100),
        sliderInput('r1', 'r1', 0.9, min = 0, max = 4,step=0.01),
        sliderInput('r2', 'r2', 0.9, min = 0, max = 4,step=0.01),
        sliderInput('a12', 'a12', 0.9, min = 0, max = 4,step=0.01),
        sliderInput('a21', 'a21', 0.9, min = 0, max = 4,step=0.01)      )
      ),
    mainPanel(checkboxGroupInput("Plotting", "Please select which population you wish to display",
                                 c("fst_population_1",
                                   "fst_population_2",
                                   "Total_fst"),
                                 selected= c("fst_population_1",
                                             "fst_population_2",
                                             "Total_fst"),
                                 inline=TRUE),
              plotOutput("plot"),
              plotOutput("plot2"),
              DT::dataTableOutput("table2"),
              DT::dataTableOutput("table1")

    )))

server <- shinyServer(function(input, output, session) {




  data_reactive<- reactive({
    set.seed(input$seed)
    params<-prisoneR::prepare_parameters()

    # params<-list(    a12=input$a12,
    #                  a21=input$a21,
    #                  r1=input$r1,
    #                  r2=input$r2,
    #                  k1=input$k1,
    #                  k2=input$k2,
    #                  genome1=input$genome1,
    #                  genome2=input$genome2,
    #                  mutation1=as.numeric(input$mutation1),
    #                  mutation2=as.numeric(input$mutation2))


    simulation<- game(
      type = "May",
      generations = input$generations,
      play1 = "Count_defective", # strategy of player
      play2 = "Count_defective", # strategy of player 2,#
      parameters = params # list of parameters to pass to ode solver
    )

    list(simulation=simulation)
  })



  output$table2 <- DT::renderDataTable({
    data<-data_reactive()$simulation
    datatable(data)


  })

  output$plot <- renderPlot({
    data<-as.data.frame(data_reactive()$simulation)
    p <- prisoneR::plot_absolute(data)
    p
  })

  output$plot2 <- renderPlot({
    data<-as.data.frame(data_reactive()$simulation)
    p <- prisoneR::plot_relative(data)
    p
  })



})



shinyApp(ui, server)
