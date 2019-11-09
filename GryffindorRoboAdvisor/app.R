library(shiny)
library(leaflet)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(RJSONIO)
library(rjson)
library(githubinstall)
library(rCharts)
library(readr)

ui <- dashboardPage(
  
  dashboardHeader(title = "Greetings to Gryffindor RoboAdvisor page!", 
                  titleWidth = 1480), 
  ## Sidebar content
  dashboardSidebar(width = 280,
                   sidebarMenu(
                     id = "tabs",
                     menuItem("Identify your goals", tabName = "risk",
                              icon = icon("list-ol")),
                     menuItem("Map your interests", tabName = "Map",
                              icon = icon("globe-americas")),
                     menuItem("Investment preferences",tabName = "Portfolio",
                              icon = icon("file-invoice-dollar"))
                               )
                   ),
  
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "risk",
              fluidRow(
                column(width = 4,
                       box(
                         title = "How much money would you like to invest?",
                         width = 0.25,
                         numericInput("initial_wealth", "Euros:",  value = 10000,
                                      min = 0, max = NA)
                       ),
                       box(
                         title = "What is your investment horizon?",
                         width = 0.25,
                         sliderInput("inv_horizon","Years: ",
                                     value = 10, min = 1, max = 50, step = 0.5)
                       ),
                       box(
                         title = "Estimate your risk preference",
                         width = 0.25,
                         sliderInput("rpref","Risk: ",
                                     value = 4, min = 1, max = 7, step = 1),
                         "Estimate your risk on the scale from 1 to 7,
                                   where 1 is the lowest tolerance towards risk,
                                   and 7 is the highest"
                       ),
                       hr(style="border-color: grey;"),
                       
                       box(
                         title = "Simulate possible outcomes",
                         width = 0.25,
                         fluidRow(
                           column(7,uiOutput("resetbutton")),
                           column(3,uiOutput("startbutton"))),
                         fluidRow(
                           column(7,actionButton("stop","Stop")),
                           column(3,actionButton("play","Play"))),
                         "Press the ‘Start‘ button in order to initiate the simulation.
                         Press ‘Next draw‘ in order to see one possible realization.
                         Use the ‘Play‘ and ‘Stop‘ button in order run the simulation
                         automatically.
                         Finally use ‘Reset‘ in order to start anew."
                       ),
                       
                       # start / reset buttons on same row
                       # fluidRow(
                       #     column(7,uiOutput("resetbutton")),
                       #     column(3,uiOutput("startbutton"))
                       # ),
                       # fluidRow(
                       #     column(7,actionButton("stop","Stop")),
                       #     column(3,actionButton("play","Play"))
                       # ),
                       #actionButton("finish","Finish"),
                       
                       actionButton("button", "Next")
                ),
                
                column(width = 8,
                       mainPanel(
                         tabsetPanel(
                           tabPanel("Draws",plotOutput('distPlot')),
                           tabPanel("Distribution",plotOutput('distPlotFinish'))
                         )
                       )
                )
                #fluidRow(
                # column(width = 7, plotOutput(outputId = "distPlot")),
                # column(width = 7, plotOutput(outputId = "distPlotFinish"))
                #)
              )
      ),
      
      
      # Second Tab Content
      
      tabItem(tabName = "Map",
              leafletOutput("mymap")),

      # Third Tab Content
      tabItem(tabName = "Portfolio", actionButton("button32", "Previous")
              )
      )
    )
  )



server <- function(input, output, session) {
  
  #Teststuff for the map 
  
  output$mymap <- renderLeaflet({geojson_read("countries.geojson", what = "sp") %>%
    
  leaflet() %>%
      
      setView(lng = 0, lat = 30, zoom = 2) %>%
      
      addProviderTiles(providers$Stamen.TonerLite,
                       options = providerTileOptions(noWrap = TRUE)) %>%
      
    addPolygons(
      weight = 1,
      color = "blue",
      dashArray = "3",
      highlight = highlightOptions(
        weight = 5,
        color = "#666",
        dashArray = "",
        fillOpacity = 0.7,
        bringToFront = TRUE),
      
      label = countries$ISO_A3,
      labelOptions = labelOptions(
        style = list("font-weight" = "normal", padding = "3px 8px"),
        textsize = "15px",
        direction = "auto"))
  })
  
  
  ###---###---###---###---###---###---###---###---###---###---###---###---###---###
  # Histogram Simulation
  
  sim <- reactiveValues() # reactive to store all reactive variables
  sim$resetindicator<-0   # used to change button labels
  sim$numb <- c()
  sim$data <- c()
  diffusion <- c(0.01, 0.02, 0.05, 0.1, 0.15, 0.25, 0.3)
  drift <- c(0.001,0.005,0.015,0.02, 0.03, 0.06, 0.1)
  draws <- 1000
  
  
  # dynamic reset button label
  output$resetbutton<-renderUI({
    if(sim$resetindicator==0){
      lbl<-"Set Parameters"
    }else{
      lbl<-"Reset"
    }
    actionButton("reset",label=lbl)
  })
  
  # dynamic start button label
  output$startbutton<-renderUI({
    if(sum(sim$data)==0){
      lbl<-"Start"
    }else{
      lbl<-"Next Draw"
    }
    actionButton("nextdraw",label=lbl)
  })
  
  # Random draw function for individual draws
  rand_draw <- function() {
    req(input$initial_wealth)
    req(input$rpref)
    req(input$inv_horizon)
    
    sim$resetindicator<-1 # change button label
    
    sim$numb <- input$initial_wealth * exp((drift[input$rpref]-(1/2)*(diffusion[input$rpref])^2)*input$inv_horizon + diffusion[input$rpref]*sqrt(input$inv_horizon)*rnorm(1))
    sim$data <<- c(sim$data, sim$numb)
    
    # sim$numb <- c(rnorm(1))
    # sim$data <<- c(sim$data, sim$numb)
    sim$data
  }
  
  ## when nextweek button is pressed
  observeEvent(input$nextdraw,{
    rand_draw()
  })
  
  
  ###
  session<-reactiveValues()
  session$timer<-reactiveTimer(Inf)
  
  observeEvent(input$play,{
    session$timer<-reactiveTimer(100)
    observeEvent(session$timer(),{
      rand_draw()
    })
  })
  
  
  observeEvent(input$stop,{
    session$timer<-reactiveTimer(Inf)
  })
  
  
  ## when reset button is pressed (set everything to original values, plus set seed)
  observeEvent(input$reset,{
    
    sim$resetindicator<-0
    
    sim$numb <- c(0)
    sim$data <- c(0)
  })
  
  
  ## main plot output
  output$distPlot <- renderPlot({
    if(sum(sim$data)==0){
      return() # no plot if reset
    }
    hist(sim$data, breaks = seq(from = 0, to = (input$initial_wealth*5), by = (input$initial_wealth*5)/30),
         xlim = c(0,input$initial_wealth*5), ylim = c(0,100), xlab = "Terminal Wealth", main = "Potential Evolvement of Wealth")
    grid()
    points(x = input$initial_wealth, y = 0, pch = 24, bg = "grey", cex = 2)
    abline(v = mean(sim$data), col = "blue", lwd = 2, lty = 2)
    
    abline(v = sim$data[order(sim$data)[length(sim$data)*0.9]], col = "red", lwd = 2, lty = 2)
    abline(v = sim$data[order(sim$data)[length(sim$data)*0.1]], col = "green", lwd = 2, lty = 2)
    
    legend("topright", legend = c("90 out of 100 boundary", "10 out of 100 boundary", "Average Terminal Wealth", "Initial Investment"),
           col=c("green", "red", "blue", "grey"), lty = c(2, 2, 2, NA), pch = c(NA, NA, NA, 24), box.lty=0)
  })
  
  ## main plot output Finish
  output$distPlotFinish <- renderPlot({
    terminal_wealth <- input$initial_wealth * exp((drift[input$rpref]-(1/2)*(diffusion[input$rpref])^2)*input$inv_horizon + diffusion[input$rpref]*sqrt(input$inv_horizon)*rnorm(1:draws))
    
    hist(terminal_wealth[terminal_wealth >= 0 & terminal_wealth < input$initial_wealth*5],
         breaks = seq(from = 0, to = (input$initial_wealth*5), by = (input$initial_wealth*5)/30),
         xlim = c(0,input$initial_wealth*5),
         xlab = "Terminal Wealth", main = "Potential Evolvement of Wealth")
    grid()
    #abline(v = input$initial_wealth, col = "blue", lwd = 2)
    points(x = input$initial_wealth, y = 0, pch = 24, bg = "grey", cex = 2)
    abline(v = mean(terminal_wealth), col = "blue", lwd = 2, lty = 2)
    
    abline(v = terminal_wealth[order(terminal_wealth)[draws*0.9]], col = "red", lwd = 2, lty = 2)
    abline(v = terminal_wealth[order(terminal_wealth)[draws*0.1]], col = "green", lwd = 2, lty = 2)
    
    legend("topright", legend = c("90 out of 100 boundary", "10 out of 100 boundary", "Average Terminal Wealth", "Initial Investment"),
           col=c("green", "red", "blue", "grey"), lty = c(2, 2, 2, NA), pch = c(NA, NA, NA, 24), box.lty=0)
  })
  
  
  
  
  
  # Switch Tabs
  observeEvent(
    input$button, {
      newtab <- switch(input$tabs, "risk" = "Map", "Map" = "risk")
      updateTabItems(session, "tabs", newtab)
    }
  )
  observeEvent(
    input$button21, {
      newtab <- switch(input$tabs, "Map" = "risk", "risk" = "Map")
      updateTabItems(session, "tabs", newtab)
    }
  )
  observeEvent(
    input$button22, {
      newtab <- switch(input$tabs, "Map" = "Portfolio", "Portfolio" = "Map")
      updateTabItems(session, "tabs", newtab)
    }
  )
   
   
   
   
}


#shinyApp(ui, server)
runApp(shinyApp(ui,server),launch.browser = TRUE)

