# Find out more about building applications with Shiny here:
# http://shiny.rstudio.com/

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Gryffindor RoboAdvisor"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            numericInput("capital",
                         "Capital to invest",
                         value = 10000,
                         min = 0,
                         max = NA),

            sliderInput("horizon",
                        "Your investment horizon",
                        min = 1,
                        max = 50,
                        value = 30,
                        step = 1)
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {}

# Run the application 
shinyApp(ui = ui, server = server)
