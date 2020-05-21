#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(readxl)
library(phaseR)


Liberia <- read_excel("ebola_2014_2016.xlsx", 
                      sheet = "Liberia")
SierraLeona <- read_excel("ebola_2014_2016.xlsx", 
                          sheet = "Sierra Leona")
Guinea <- read_excel("ebola_2014_2016.xlsx", 
                     sheet = "Guinea")

pobLiberia = 4360000
pobSierraLn = 7017000
pobGuinea = 11150000

library(shinydashboard)

ciudades=c("Liberia","SierraLeona","Guinea","Personalizado")


# Define UI for application that draws a histogram
ui <- fluidPage(
    
    
    tags$head(tags$style(
        HTML('
         .well {
            background-color: #EEEBB4;
        }

        body, label, input, button, select { 
          font-family: "italic";
        }')
    )),
    
    
    # Application title
    titlePanel("Modelos Epidemiologicos: Ebola de 2014-2016 "),
    sidebarLayout(
        sidebarPanel( 
            h3(span("Pontifica Universidad Javeriana", style = "color:darkblue")),
            h4("Facultad de ciencias"),
            h4("Analisis numerico"),
            h5("Marlon Esteban Linares"),
            h5("Juan Felipe Marin"),
            h5(" Andres Jose Mora"),
            h5 ("Brian Hortua")
            
        ), mainPanel(
            
        )  
        
    ),
    
    
    # Sidebar with a slider input for number of bins 
    
    sidebarLayout(
        
        sidebarPanel(
            # Copy the line below to make a select box 
            
            # Copy the line below to make a select box 
            selectInput("select", label = h3("Seleccione pais"), 
                        choices= ciudades
            ),
            
            
            hr(),

            sliderInput("gamma",
                        "Variacion de gamma:",
                        min = 0,
                        max = 1,
                        value = 0.0914,
                        step=0.0001),
            sliderInput("beta",
                        "Variacion de beta:",
                        min = 0,
                        max = 1,
                        value = 0.4247,
                        step=0.0001),
            sliderInput("pobTotal",
                        "Variacion de Poblacion Total:",
                        min = 0,
                        max = 12000000,
                        value = 4360000,
                        step=1),
            sliderInput("pobInfInicial",
                        "Variacion de inicial infectada:",
                        min = 0,
                        max = 3000,
                        value = as.numeric(unlist(Liberia[1,2])), #Primer valor de infeccion en archivo CSV
                        step=1)
            
        ),
        
        # Show a plot of the generated distribution
        mainPanel(
            tabsetPanel(type = "tabs",
                        tabPanel("Metodo de Euler (S.I.R.)", plotOutput("sirEuler")),
                        tabPanel("Metodo de Runge-Kutta de 4to orden (S.I.R.)",  plotOutput("sirRk4")),
                        tabPanel("Error Runge-Kutta (S.I.R.)", plotOutput("errorRK4")),
                        tabPanel("Error  Euler (S.I.R.)", plotOutput("errorEuler")),
                        tabPanel("Campo de Pendientes", plotOutput("PendientesOde")),
                        tabPanel("Metodo de Euler (S.I.)", plotOutput("siEuler")),
                        tabPanel("Metodo de Runge-Kutta de 4to orden (S.I.)",  plotOutput("siRk4")),
                        tabPanel("Metodo de Euler (S.I.S)", plotOutput("sisEuler")),
                        tabPanel("Metodo de Runge-Kutta de 4to orden (S.I.S)",  plotOutput("sisRk4"))
                        
            )
            
            
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    output$sirEuler <- renderPlot({
        ## Create an SIR function
        sir <- function(time, state, parameters) {
            
            with(as.list(c(state, parameters)), {
                
                dS <- -beta * S * I
                dI <-  beta * S * I - gamma * I
                dR <-                 gamma * I
                
                return(list(c(dS, dI, dR)))
            })
        }
        
        ### Set parameters
        
        if(input$select== "Liberia"){
            init       <- c(S = 1-(as.numeric(unlist(Liberia[1,2])) /pobLiberia)-((as.numeric(unlist(Liberia[1,4])))/pobLiberia), I = as.numeric(unlist(Liberia[1,2]))/pobLiberia, R = ((as.numeric(unlist(Liberia[1,4])))/pobLiberia) )  
            parameters <- c(beta = 0.4247, gamma = 0.0914)
        }
        else if (input$select== "SierraLeona")
        {
            init       <- c(S = 1-(as.numeric(unlist(SierraLeona[1,2]))/pobSierraLn)-((as.numeric(unlist(SierraLeona[1,4])))/pobSierraLn), I = as.numeric(unlist(SierraLeona[1,2])) /pobSierraLn, R = ((as.numeric(unlist(SierraLeona[1,4])))/pobSierraLn) )   
            parameters <- c(beta = 0.5705, gamma = 0.0638)
        }
        else if(input$select== "Guinea"){
            init       <- c(S = 1-(as.numeric(unlist(Guinea[1,2])) /pobGuinea)-((as.numeric(unlist(Guinea[1,4])))/pobGuinea), I = as.numeric(unlist(Guinea[1,2]))/pobGuinea, R = ((as.numeric(unlist(Guinea[1,4])))/pobGuinea) )  
            parameters <- c(beta = 0.7407, gamma = 0.1055)
        }
        else if(input$select == "Personalizado"){
            init       <- c(S = 1-(input$pobInfInicial/input$pobTotal)-((as.numeric(unlist(Liberia[1,4])))/input$pobTotal), I = input$pobInfInicial/input$pobTotal, R = ((as.numeric(unlist(Liberia[1,4])))/input$pobTotal) )
            parameters <- c(beta = input$beta, gamma = input$gamma)
        }
        ## beta: infection parameter; gamma: recovery parameter
        
        ## Time frame
        times      <- seq(0, 60, by = 1)
        ## Load deSolve package
        library(deSolve)
        
        ## Solve using ode (General Solver for Ordinary Differential Equations)
        out <- ode(y = init, times = times, func = sir, parms = parameters, method="euler")
        ## change to data frame
        out <- as.data.frame(out)
        ## Delete time variable
        out$time <- NULL
        ## Show data
        head(out, 30)
        ## Plot
        matplot(x = times, y = out, type = "l",
                xlab = "Tiempo (Dias)", ylab="Poblacion", main = "SIR con Euler",
                lwd = 1, lty = 1, bty = "l", col = 2:4)
        
        ## Add legend
        legend(40, 0.7, c("Susceptibles", "Infectados", "Recuperados"), pch = 1, col = 2:4, bty = "n")
    })
    
    
    output$sirRk4 <-renderPlot({
        
        ## Create an SIR function
        sir <- function(time, state, parameters) {
            
            with(as.list(c(state, parameters)), {
                
                dS <- -beta * S * I
                dI <-  beta * S * I - gamma * I
                dR <-                 gamma * I
                
                return(list(c(dS, dI, dR)))
            })
        }
        
        ### Set parameters
        
        if(input$select== "Liberia"){
            init       <- c(S = 1-(as.numeric(unlist(Liberia[1,2])) /pobLiberia)-((as.numeric(unlist(Liberia[1,4])))/pobLiberia), I = as.numeric(unlist(Liberia[1,2]))/pobLiberia, R = ((as.numeric(unlist(Liberia[1,4])))/pobLiberia) )  
            parameters <- c(beta = 0.4247, gamma = 0.0914)
        }
        else if (input$select== "SierraLeona")
        {
            init       <- c(S = 1-(as.numeric(unlist(SierraLeona[1,2]))/pobSierraLn)-((as.numeric(unlist(SierraLeona[1,4])))/pobSierraLn), I = as.numeric(unlist(SierraLeona[1,2])) /pobSierraLn, R = ((as.numeric(unlist(SierraLeona[1,4])))/pobSierraLn) )   
            parameters <- c(beta = 0.5705, gamma = 0.0638)
        }
        else if(input$select== "Guinea"){
            init       <- c(S = 1-(as.numeric(unlist(Guinea[1,2])) /pobGuinea)-((as.numeric(unlist(Guinea[1,4])))/pobGuinea), I = as.numeric(unlist(Guinea[1,2]))/pobGuinea, R = ((as.numeric(unlist(Guinea[1,4])))/pobGuinea) )  
            parameters <- c(beta = 0.7407, gamma = 0.1055)
        }
        else if(input$select == "Personalizado"){
            init       <- c(S = 1-(input$pobInfInicial/input$pobTotal)-((as.numeric(unlist(Liberia[1,4])))/input$pobTotal), I = input$pobInfInicial/input$pobTotal, R = ((as.numeric(unlist(Liberia[1,4])))/input$pobTotal) )
            parameters <- c(beta = input$beta, gamma = input$gamma)
        }
        ## Time frame
        times      <- seq(0, 60, by = 1)
        
        library(deSolve)
        
        ## Solve using runge kutta 4 (General Solver for Ordinary Differential Equations)
        out <- ode(y = init, times = times, func = sir, parms = parameters, method="rk4")
        ## change to data frame
        out <- as.data.frame(out)
        ## Delete time variable
        out$time <- NULL
        ## Show data
        head(out, 30)
        ## Plot
        matplot(x = times, y = out, type = "l",
                xlab = "Tiempo (Dias)", ylab="Poblacion", main = "SIR con Runge-Kutta",
                lwd = 1, lty = 1, bty = "l", col = 2:4)
        
        ## Add legend
        legend(40, 0.7, c("Susceptibles", "Infectados", "Recuperados"), pch = 1, col = 2:4, bty = "n")
        
    })
    
    output$errorRK4 <- renderPlot({
        ## Create an SIR function
        sir <- function(time, state, parameters) {
            
            with(as.list(c(state, parameters)), {
                
                dS <- -beta * S * I
                dI <-  beta * S * I - gamma * I
                dR <-                 gamma * I
                
                return(list(c(dS, dI, dR)))
            })
        }
        
        ### Set parameters
        
        if(input$select== "Liberia"){
            init       <- c(S = 1-(as.numeric(unlist(Liberia[1,2])) /pobLiberia)-((as.numeric(unlist(Liberia[1,4])))/pobLiberia), I = as.numeric(unlist(Liberia[1,2]))/pobLiberia, R = ((as.numeric(unlist(Liberia[1,4])))/pobLiberia) )  
            parameters <- c(beta = 0.4247, gamma = 0.0914)
        }
        else if (input$select== "SierraLeona")
        {
            init       <- c(S = 1-(as.numeric(unlist(SierraLeona[1,2]))/pobSierraLn)-((as.numeric(unlist(SierraLeona[1,4])))/pobSierraLn), I = as.numeric(unlist(SierraLeona[1,2])) /pobSierraLn, R = ((as.numeric(unlist(SierraLeona[1,4])))/pobSierraLn) )   
            parameters <- c(beta = 0.5705, gamma = 0.0638)
        }
        else if(input$select== "Guinea"){
            init       <- c(S = 1-(as.numeric(unlist(Guinea[1,2])) /pobGuinea)-((as.numeric(unlist(Guinea[1,4])))/pobGuinea), I = as.numeric(unlist(Guinea[1,2]))/pobGuinea, R = ((as.numeric(unlist(Guinea[1,4])))/pobGuinea) )  
            parameters <- c(beta = 0.7407, gamma = 0.1055)
        }
        else if(input$select == "Personalizado"){
            init       <- c(S = 1-(input$pobInfInicial/input$pobTotal)-((as.numeric(unlist(Liberia[1,4])))/input$pobTotal), I = input$pobInfInicial/input$pobTotal, R = ((as.numeric(unlist(Liberia[1,4])))/input$pobTotal) )
            parameters <- c(beta = input$beta, gamma = input$gamma)
        }
        ## Time frame
        times      <- seq(0, 60, by = 1)
        
        library(deSolve)
        
        ## Solve using runge kutta 4 (General Solver for Ordinary Differential Equations)
        sirRK4 <- as.data.frame(ode(y = init, times = times, func = sir, parms = parameters, method="rk4"))
        sirEuler <- as.data.frame(ode(y = init, times = times, func = sir, parms = parameters, method="euler"))

  
        # starting error
        i = 1
        error1 <- c()
        for (x in sirRK4$S){
            error1 <- c(error1, ( (abs(sirEuler$S[i]-x)) / sirEuler$S[i] ) * 100 )
        i = i+1
        }
        x <- seq(0,60)
        plot(sirRK4$time, error1, col="red", type="l")
    })
    
    output$errorEuler <- renderPlot({
        ## Create an SIR function
        sir <- function(time, state, parameters) {
            
            with(as.list(c(state, parameters)), {
                
                dS <- -beta * S * I
                dI <-  beta * S * I - gamma * I
                dR <-                 gamma * I
                
                return(list(c(dS, dI, dR)))
            })
        }
        
        ### Set parameters
        
        if(input$select== "Liberia"){
            init       <- c(S = 1-(as.numeric(unlist(Liberia[1,2])) /pobLiberia)-((as.numeric(unlist(Liberia[1,4])))/pobLiberia), I = as.numeric(unlist(Liberia[1,2]))/pobLiberia, R = ((as.numeric(unlist(Liberia[1,4])))/pobLiberia) )  
            parameters <- c(beta = 0.4247, gamma = 0.0914)
        }
        else if (input$select== "SierraLeona")
        {
            init       <- c(S = 1-(as.numeric(unlist(SierraLeona[1,2]))/pobSierraLn)-((as.numeric(unlist(SierraLeona[1,4])))/pobSierraLn), I = as.numeric(unlist(SierraLeona[1,2])) /pobSierraLn, R = ((as.numeric(unlist(SierraLeona[1,4])))/pobSierraLn) )   
            parameters <- c(beta = 0.5705, gamma = 0.0638)
        }
        else if(input$select== "Guinea"){
            init       <- c(S = 1-(as.numeric(unlist(Guinea[1,2])) /pobGuinea)-((as.numeric(unlist(Guinea[1,4])))/pobGuinea), I = as.numeric(unlist(Guinea[1,2]))/pobGuinea, R = ((as.numeric(unlist(Guinea[1,4])))/pobGuinea) )  
            parameters <- c(beta = 0.7407, gamma = 0.1055)
        }
        else if(input$select == "Personalizado"){
            init       <- c(S = 1-(input$pobInfInicial/input$pobTotal)-((as.numeric(unlist(Liberia[1,4])))/input$pobTotal), I = input$pobInfInicial/input$pobTotal, R = ((as.numeric(unlist(Liberia[1,4])))/input$pobTotal) )
            parameters <- c(beta = input$beta, gamma = input$gamma)
        }
        ## Time frame
        times      <- seq(0, 60, by = 1)
        
        library(deSolve)
        
        ## Solve using runge kutta 4 (General Solver for Ordinary Differential Equations)
        sirRK4 <- as.data.frame(ode(y = init, times = times, func = sir, parms = parameters, method="rk4"))
        sirEuler <- as.data.frame(ode(y = init, times = times, func = sir, parms = parameters, method="euler"))
        
        
        # starting error
        i = 1
        error1 <- c()
        for (x in sirEuler$S){
            error1 <- c(error1, ( (abs(sirRK4$S[i]-x)) / sirRK4$S[i] ) * 100 )
            i = i+1
        }
        x <- seq(0,60)
        plot(sirEuler$time, error1, col="blue", type="l")
    })
    
    output$PendientesOde <- renderPlot({ 
        scopeField <- function(t, p, parameters){
            k <- parameters[1]
            n <- parameters[2]
            dp <- k*(p*(n-p))
            list(dp)
        }
        scopeField.flowField <- flowField(scopeField, xlim = c(0,60),
                                          ylim = c(0,1), parameters = c(0.1,2),
                                          system = "one.dim",
                                          add = FALSE, xlab = "Tiempo (en dias)", ylab = "Contagios", 
                                          main = "Campo de pendientes")
    })
    
    output$siEuler <- renderPlot({
        ## Create an SIR function
        si <- function(time, state, parameters) {
            
            with(as.list(c(state, parameters)), {
                
                dS <- -beta * S * I
                dI <-  beta * S * I
                
                return(list(c(dS, dI, 0)))
            })
        }
        
        ### Set parameters
        
        if(input$select== "Liberia"){
            init       <- c(S = 1-(as.numeric(unlist(Liberia[1,2])) /pobLiberia)-((as.numeric(unlist(Liberia[1,4])))/pobLiberia), I = as.numeric(unlist(Liberia[1,2]))/pobLiberia, R = ((as.numeric(unlist(Liberia[1,4])))/pobLiberia) )  
            parameters <- c(beta = 0.4247, gamma = 0.0914)
        }
        else if (input$select== "SierraLeona")
        {
            init       <- c(S = 1-(as.numeric(unlist(SierraLeona[1,2]))/pobSierraLn)-((as.numeric(unlist(SierraLeona[1,4])))/pobSierraLn), I = as.numeric(unlist(SierraLeona[1,2])) /pobSierraLn, R = ((as.numeric(unlist(SierraLeona[1,4])))/pobSierraLn) )   
            parameters <- c(beta = 0.5705, gamma = 0.0638)
        }
        else if(input$select== "Guinea"){
            init       <- c(S = 1-(as.numeric(unlist(Guinea[1,2])) /pobGuinea)-((as.numeric(unlist(Guinea[1,4])))/pobGuinea), I = as.numeric(unlist(Guinea[1,2]))/pobGuinea, R = ((as.numeric(unlist(Guinea[1,4])))/pobGuinea) )  
            parameters <- c(beta = 0.7407, gamma = 0.1055)
        }
        else if(input$select == "Personalizado"){
            init       <- c(S = 1-(input$pobInfInicial/input$pobTotal)-((as.numeric(unlist(Liberia[1,4])))/input$pobTotal), I = input$pobInfInicial/input$pobTotal, R = ((as.numeric(unlist(Liberia[1,4])))/input$pobTotal) )
            parameters <- c(beta = input$beta, gamma = input$gamma)
        }
        ## beta: infection parameter; gamma: recovery parameter
        
        ## Time frame
        times      <- seq(0, 60, by = 1)
        ## Load deSolve package
        library(deSolve)
        
        ## Solve using ode (General Solver for Ordinary Differential Equations)
        out <- ode(y = init, times = times, func = si, parms = parameters, method="euler")
        ## change to data frame
        out <- as.data.frame(out)
        ## Delete time variable
        out$time <- NULL
        ## Show data
        head(out, 30)
        ## Plot
        matplot(x = times, y = out, type = "l",
                xlab = "Tiempo (Dias)", ylab="Poblacion", main = "SIR con Euler",
                lwd = 1, lty = 1, bty = "l", col = 2:4)
        
        ## Add legend
        legend(40, 0.7, c("Susceptibles", "Infectados"), pch = 1, col = 2:4, bty = "n")
    })
    
    output$siRk4 <- renderPlot({
        ## Create an SIR function
        si <- function(time, state, parameters) {
            
            with(as.list(c(state, parameters)), {
                
                dS <- -beta * S * I
                dI <-  beta * S * I
                
                return(list(c(dS, dI, 0)))
            })
        }
        
        ### Set parameters
        
        if(input$select== "Liberia"){
            init       <- c(S = 1-(as.numeric(unlist(Liberia[1,2])) /pobLiberia)-((as.numeric(unlist(Liberia[1,4])))/pobLiberia), I = as.numeric(unlist(Liberia[1,2]))/pobLiberia, R = ((as.numeric(unlist(Liberia[1,4])))/pobLiberia) )  
            parameters <- c(beta = 0.4247, gamma = 0.0914)
        }
        else if (input$select== "SierraLeona")
        {
            init       <- c(S = 1-(as.numeric(unlist(SierraLeona[1,2]))/pobSierraLn)-((as.numeric(unlist(SierraLeona[1,4])))/pobSierraLn), I = as.numeric(unlist(SierraLeona[1,2])) /pobSierraLn, R = ((as.numeric(unlist(SierraLeona[1,4])))/pobSierraLn) )   
            parameters <- c(beta = 0.5705, gamma = 0.0638)
        }
        else if(input$select== "Guinea"){
            init       <- c(S = 1-(as.numeric(unlist(Guinea[1,2])) /pobGuinea)-((as.numeric(unlist(Guinea[1,4])))/pobGuinea), I = as.numeric(unlist(Guinea[1,2]))/pobGuinea, R = ((as.numeric(unlist(Guinea[1,4])))/pobGuinea) )  
            parameters <- c(beta = 0.7407, gamma = 0.1055)
        }
        else if(input$select == "Personalizado"){
            init       <- c(S = 1-(input$pobInfInicial/input$pobTotal)-((as.numeric(unlist(Liberia[1,4])))/input$pobTotal), I = input$pobInfInicial/input$pobTotal, R = ((as.numeric(unlist(Liberia[1,4])))/input$pobTotal) )
            parameters <- c(beta = input$beta, gamma = input$gamma)
        }
        ## beta: infection parameter; gamma: recovery parameter
        
        ## Time frame
        times      <- seq(0, 60, by = 1)
        ## Load deSolve package
        library(deSolve)
        
        ## Solve using ode (General Solver for Ordinary Differential Equations)
        out <- ode(y = init, times = times, func = si, parms = parameters, method="rk4")
        ## change to data frame
        out <- as.data.frame(out)
        ## Delete time variable
        out$time <- NULL
        ## Show data
        head(out, 30)
        ## Plot
        matplot(x = times, y = out, type = "l",
                xlab = "Tiempo (Dias)", ylab="Poblacion", main = "SI con Euler",
                lwd = 1, lty = 1, bty = "l", col = 2:4)
        
        ## Add legend
        legend(40, 0.7, c("Susceptibles", "Infectados"), pch = 1, col = 2:4, bty = "n")
    })
    
    output$sisRk4 <- renderPlot({
      ## Create an SIR function
      si <- function(time, state, parameters) {
        
        with(as.list(c(state, parameters)), {
          
          dS <- -beta * S * I + (gamma * I)
          dI <-  beta * S * I - (gamma * I)
          
          return(list(c(dS, dI, 0)))
        })
      }
      
      ### Set parameters
      
      if(input$select== "Liberia"){
        init       <- c(S = 1-(as.numeric(unlist(Liberia[1,2])) /pobLiberia)-((as.numeric(unlist(Liberia[1,4])))/pobLiberia), I = as.numeric(unlist(Liberia[1,2]))/pobLiberia, R = ((as.numeric(unlist(Liberia[1,4])))/pobLiberia) )  
        parameters <- c(beta = 0.4247, gamma = 0.0914)
      }
      else if (input$select== "SierraLeona")
      {
        init       <- c(S = 1-(as.numeric(unlist(SierraLeona[1,2]))/pobSierraLn)-((as.numeric(unlist(SierraLeona[1,4])))/pobSierraLn), I = as.numeric(unlist(SierraLeona[1,2])) /pobSierraLn, R = ((as.numeric(unlist(SierraLeona[1,4])))/pobSierraLn) )   
        parameters <- c(beta = 0.5705, gamma = 0.0638)
      }
      else if(input$select== "Guinea"){
        init       <- c(S = 1-(as.numeric(unlist(Guinea[1,2])) /pobGuinea)-((as.numeric(unlist(Guinea[1,4])))/pobGuinea), I = as.numeric(unlist(Guinea[1,2]))/pobGuinea, R = ((as.numeric(unlist(Guinea[1,4])))/pobGuinea) )  
        parameters <- c(beta = 0.7407, gamma = 0.1055)
      }
      else if(input$select == "Personalizado"){
        init       <- c(S = 1-(input$pobInfInicial/input$pobTotal)-((as.numeric(unlist(Liberia[1,4])))/input$pobTotal), I = input$pobInfInicial/input$pobTotal, R = ((as.numeric(unlist(Liberia[1,4])))/input$pobTotal) )
        parameters <- c(beta = input$beta, gamma = input$gamma)
      }
      ## beta: infection parameter; gamma: recovery parameter
      
      ## Time frame
      times      <- seq(0, 60, by = 1)
      ## Load deSolve package
      library(deSolve)
      
      ## Solve using ode (General Solver for Ordinary Differential Equations)
      out <- ode(y = init, times = times, func = si, parms = parameters, method="rk4")
      ## change to data frame
      out <- as.data.frame(out)
      ## Delete time variable
      out$time <- NULL
      ## Show data
      head(out, 30)
      ## Plot
      matplot(x = times, y = out, type = "l",
              xlab = "Tiempo (Dias)", ylab="Poblacion", main = "SIS con Runge-Kutta",
              lwd = 1, lty = 1, bty = "l", col = 2:4)
      
      ## Add legend
      legend(40, 0.7, c("Susceptibles", "Infectados"), pch = 1, col = 2:4, bty = "n")
    })
    
    output$sisEuler <- renderPlot({
      ## Create an SIR function
      si <- function(time, state, parameters) {
        
        with(as.list(c(state, parameters)), {
          
          dS <- -beta * S * I + (gamma * I)
          dI <-  beta * S * I - (gamma * I)
          
          return(list(c(dS, dI, 0)))
        })
      }
      
      ### Set parameters
      
      if(input$select== "Liberia"){
        init       <- c(S = 1-(as.numeric(unlist(Liberia[1,2])) /pobLiberia)-((as.numeric(unlist(Liberia[1,4])))/pobLiberia), I = as.numeric(unlist(Liberia[1,2]))/pobLiberia, R = ((as.numeric(unlist(Liberia[1,4])))/pobLiberia) )  
        parameters <- c(beta = 0.4247, gamma = 0.0914)
      }
      else if (input$select== "SierraLeona")
      {
        init       <- c(S = 1-(as.numeric(unlist(SierraLeona[1,2]))/pobSierraLn)-((as.numeric(unlist(SierraLeona[1,4])))/pobSierraLn), I = as.numeric(unlist(SierraLeona[1,2])) /pobSierraLn, R = ((as.numeric(unlist(SierraLeona[1,4])))/pobSierraLn) )   
        parameters <- c(beta = 0.5705, gamma = 0.0638)
      }
      else if(input$select== "Guinea"){
        init       <- c(S = 1-(as.numeric(unlist(Guinea[1,2])) /pobGuinea)-((as.numeric(unlist(Guinea[1,4])))/pobGuinea), I = as.numeric(unlist(Guinea[1,2]))/pobGuinea, R = ((as.numeric(unlist(Guinea[1,4])))/pobGuinea) )  
        parameters <- c(beta = 0.7407, gamma = 0.1055)
      }
      else if(input$select == "Personalizado"){
        init       <- c(S = 1-(input$pobInfInicial/input$pobTotal)-((as.numeric(unlist(Liberia[1,4])))/input$pobTotal), I = input$pobInfInicial/input$pobTotal, R = ((as.numeric(unlist(Liberia[1,4])))/input$pobTotal) )
        parameters <- c(beta = input$beta, gamma = input$gamma)
      }
      ## beta: infection parameter; gamma: recovery parameter
      
      ## Time frame
      times      <- seq(0, 60, by = 1)
      ## Load deSolve package
      library(deSolve)
      
      ## Solve using ode (General Solver for Ordinary Differential Equations)
      out <- ode(y = init, times = times, func = si, parms = parameters, method="euler")
      ## change to data frame
      out <- as.data.frame(out)
      ## Delete time variable
      out$time <- NULL
      ## Show data
      head(out, 30)
      ## Plot
      matplot(x = times, y = out, type = "l",
              xlab = "Tiempo (Dias)", ylab="Poblacion", main = "SIS con Euler",
              lwd = 1, lty = 1, bty = "l", col = 2:4)
      
      ## Add legend
      legend(40, 0.7, c("Susceptibles", "Infectados"), pch = 1, col = 2:4, bty = "n")
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
