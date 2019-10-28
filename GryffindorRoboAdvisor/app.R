library(shiny)
library(shinydashboard)
library(shinyjs)
library(ggplot2)

app <- shinyApp(
ui <- dashboardPage(
  
  dashboardHeader(title = "Greetings to Gryffindor RoboAdvisor page!", titleWidth = 1480), 
  ## Sidebar content
  dashboardSidebar(useShinyjs(),width = 280,
                   sidebarMenu(
                     id = "tabs",
                     menuItem("Your Investment goals", tabName = "Goals", icon = icon("list-ol")),
                     menuItem("Rsik Simulation", tabName = "Simulation", icon = icon("globe-americas")),
                     menuItem("Investment preferences", tabName = "Preferences", icon = icon("file-invoice-dollar")),
                     menuItem("Historical Performance", tabName = "Backtest", icon = icon("file-invoice-dollar")),
                     menuItem("Investment Summary", tabName = "Summary", icon = icon("file-invoice-dollar"))
                   )
  ),
  
  ## Body content
  dashboardBody(
    tabItems(
      # First tab content
      tabItem(tabName = "Goals",
              fluidRow(
                column(width = 5,
                       box(
                         title = "How much money would you like to invest?",
                         width = 0.25,
                         numericInput("capital", "Euros:",  value = 10000, 
                                      min = 0, max = NA)
                         ),
                       box(
                         title = "What is your investment horizon?",
                         width = 0.25,
                         sliderInput("horizon","Years: ",
                                     value = 10, min = 1, max = 50, step = 0.5)
                       ),
                       box(
                         title = "Estimate your risk preference",
                         width = 0.25,
                         sliderInput("rpref","Risk: ",
                                     value = 4, min = 1, max = 7, step = 1),
                         "Estimate your risk on the scale from 1 to 7, where 1 is the lowest tolerance towards risk, and 7 is the highest"
                       ),

                       actionButton(inputId ="Previous", label = icon("arrow-left")),
                       actionButton(inputId ="Next", label = icon("arrow-right"))
                       
                       
                       
                       ),
                column(width = 7, plotOutput(outputId = "distPlot")
                       )
                )
              ),
      tabItem(tabName = "Simulation"),
      tabItem(tabName = "Map your interests"),
      tabItem(tabName = "Preferences"),
      tabItem(tabName = "Summary")
     
      
      
    )
  )
)







#server <- function(input, output, session) {
  
  # risks <- read.csv("risks.csv", sep = ';')
  # var <- reactive(input$rpref)
  # risks <- risks[risks$Risk == var,][2]

#  output$distPlot <- renderPlot({
    
#    returns = rnorm(1:1000, mean = input$capital, sd = 7)
    
#    ggplot() + aes(returns) + geom_histogram() + geom_vline(aes(xintercept= input$capital),
#                                                  color="red", linetype="solid", size=2) + 
#      labs(title = "Possible returns on investment" , x = "Return (Euros)", y = "Frequency")
    
    
#  })
  
#}

server = 
  shinyServer(function(input, output, session){
    global <- reactiveValues(tab_id = "")
    tab_id <- c("Goals", "Simulation", "Preferences", "Backtest", "Summary")
    
    Current <- reactiveValues(
      Tab = "Home"
    )
    
    observeEvent(
      input[["tabs"]],
      {
        Current$Tab <- input[["tabs"]]
      }
    )
    
    observeEvent(
      input[["Previous"]],
      {
        tab_id_position <- match(Current$Tab, input$tabNames) - 1
        if (tab_id_position == 0) tab_id_position <- length(input$tabNames)
        Current$Tab <- input$tabNames[tab_id_position]
        updateTabItems(session, "tabs", input$tabNames[tab_id_position]) 
      }
    )
    
    observeEvent(
      input[["Next"]],
      {
        tab_id_position <- match(Current$Tab, input$tabNames) + 1
        if (tab_id_position > length(input$tabNames)) tab_id_position <- 1
        Current$Tab <- input$tabNames[tab_id_position]
        updateTabItems(session, "tabs", input$tabNames[tab_id_position]) 
      }
    )
    
    observe({
      runjs("
          function getAllElementsWithAttribute(attribute){
             var matchingElements = [];
             var allElements = document.getElementsByTagName('*');
             for (var i = 0, n = allElements.length; i < n; i++){
                if (allElements[i].getAttribute(attribute) !== null){
                   matchingElements.push(allElements[i]);
                }
             }
             return matchingElements;
          };

          ahref = getAllElementsWithAttribute('data-toggle');
          var tabNames = [];
          var tabName = '';
          for (var nr = 0, n = ahref.length; nr < n; nr++){
             tabName = ahref[nr].hash.split('-')[2]
             if(tabName != 'Toggle navigation') tabNames.push(tabName)
          }
          Shiny.onInputChange('tabNames', tabNames);
          ")
    })
    
    
  })
)

shinyApp(app)
