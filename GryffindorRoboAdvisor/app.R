library(shiny)
library(shinydashboard)
library(ggplot2)

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
                         "Estimate your risk on the scale from 1 to 7,
                         where 1 is the lowest tolerance towards risk, 
                         and 7 is the highest"
                         ),

                       actionButton("button", "Next")
                       ),
                
                column(width = 7, plotOutput(outputId = "distPlot")
                       )
                )
              ),

      tabItem(tabName = "Map", actionButton("button21", "Previous"),
              actionButton("button22", "Next")),
      tabItem(tabName = "Portfolio")
      )
    )
  )


server <- function(input, output, session) {
  
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

   risks <- c(0.007, 0.02, 0.05, 0.1, 0.15, 0.25, 0.3)
   
   
   output$distPlot <- renderPlot({
   returns = rnorm(1:1000, mean = input$capital, 
                   sd = input$capital*risks[input$rpref]*sqrt(input$horizon))
   
   
     ggplot() + aes(returns) + geom_histogram() + 
       geom_vline(aes(xintercept= input$capital), color="red", 
                  linetype="solid", size=2) + labs(
                    title = "Possible returns on investment" ,
                    x = "Return (Euros)", y = "Frequency")
   })
   }



shinyApp(ui, server)
