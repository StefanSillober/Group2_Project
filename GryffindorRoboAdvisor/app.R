library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(RJSONIO)
library(rjson)
library(rCharts)
library(readr)
library(shinythemes)
library(devtools)
library(githubinstall)
#install_github("nik01010/dashboardthemes")
library(dashboardthemes)
library(geojsonio)
library(rgdal)

header <- dashboardHeader(
    title = shinyDashboardLogoDIY(boldText =  tagList(shiny::icon("robot"), "Gryffindor"),
                                  mainText = "Robo-Advisor",
                                  badgeText = "Group 2",
                                  badgeBackColor = "#40E0D0",
                                  badgeTextColor = "white"),
    titleWidth = 300
)


sidebar <- dashboardSidebar(width = 280,
                            sidebarMenu(id = "tabs",
                                        menuItem("Identify your goals", tabName = "tab1"),
                                        menuItem("Verify Risk Preference", tabName = "tab2"),
                                        menuItem("Geographical Preferences", tabName = "tab3"),
                                        menuItem("Industry Preferences", tabName = "tab4"),
                                        menuItem("Portfolio Construction", tabName = "tab5")
                            ), disable = TRUE
)


body <- dashboardBody(
    # shinyDashboardThemes(
    #     theme = "purple_gradient"
    #     #theme = "grey_dark"
    # ),
        
    tabItems(
        # First Tab: Risk Evaluation
        tabItem(tabName = "tab1", h2("Risk Evaluation"), # tab item header
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
                                       value = 10, min = 1, max = 30, step = 0.5)
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
                           tags$p("Press the", tags$em("Start"), 
                                  "button in order to initiate the simulation.", br(),
                                  "Press", tags$em("Next Draw"), "in order to see one possible realization.", br(),
                                  "Use the", tags$em("Play"), "and", tags$em("Stop"), "button",
                                  "in order run the simulation automatically.", br(),
                                  "Finally use", tags$em("Reset"), "in order to start anew."),
                       ),
                       
                       actionButton("button1", "Next")
                       
                ), # end of first column object
                
                column(width = 8,
                       tabBox(
                           title = tagList(shiny::icon("dice"), "Portfolio Simulation"), width = 12,
                           # The id lets us use input$tabset1 on the server to find the current tab
                           id = "tabset1", height = "550",
                           tabPanel(title = tagList(shiny::icon("chart-bar"), "Histogram"), plotOutput('distPlot', height = "500")),
                           tabPanel(title = tagList(shiny::icon("info"), "Details"), "Explanation to the return sampling, i.e. GBM, 90% & 10% VaR ....")
                       ),
                       
                       # Dynamic valueBoxes
                       valueBoxOutput("horizonBox"),
                       valueBoxOutput("returnBox"),
                       valueBoxOutput("stdBox"),
                       valueBoxOutput("avgBox"),
                       valueBoxOutput("uplimBox"),
                       valueBoxOutput("lowlimBox")
                )
                
            ) # end of fluid row
        ), # end of first tab item
        
        
        # Second Tab: Risk Evaluation
        tabItem(tabName = "tab2", h2("Risk Evaluation"), # tab item header
            fluidRow(
                column(width = 4,
                       box(
                           title = "Estimate your risk preference",
                           width = 0.25,
                           sliderInput("rpref2","Risk: ",
                                       value = 4, min = 1, max = 7, step = 1),
                           "Estimate your risk on the scale from 1 to 7,
                                   where 1 is the lowest tolerance towards risk,
                                   and 7 is the highest"
                       ),
                       
                       fluidRow(
                           column(7,actionButton("button2","Back")),
                           column(1,actionButton("button3","Next")))
                       
                ), # end of first column object
                
                column(width = 8,
                       tabBox(
                           title = tagList(shiny::icon("dice"), "Portfolio Simulation"), width = 12,
                           # The id lets us use input$tabset1 on the server to find the current tab
                           id = "tabset1", height = "550",
                           tabPanel(title = tagList(shiny::icon("chart-bar"), "Histogram"), plotOutput('distPlotFinish', height = "500")),
                           tabPanel(title = tagList(shiny::icon("info"), "Details"), "Explanations ...")
                           ),

                       # Dynamic valueBoxes
                       valueBoxOutput("horizonBox1"),
                       valueBoxOutput("returnBox1"),
                       valueBoxOutput("stdBox1"),
                       valueBoxOutput("avgBox1"),
                       valueBoxOutput("uplimBox1"),
                       valueBoxOutput("lowlimBox1")
                       
                )
                
            ) # end of fluidrow
        ), # end of second tab item
        
        
        # Third Tab: Geographical Preferences
        tabItem(tabName = "tab3", h2("Geographical Preferences"), # tab item header
                tabBox(
                    title = tagList(shiny::icon("map-pin"), "Geographical Preferences"), width = 12,
                    id = "tabset1", height = "550",
                    tabPanel(title = tagList(shiny::icon("globe-americas"), "Map"), leafletOutput("mymap", height = "500")),
                    tabPanel(title = tagList(shiny::icon("info"), "Details"), "Explanations about the map and how to use it ....")
                ),
                
                
                #leafletOutput("mymap"),
                hr(style="border-color: grey;"),
                
                fluidRow(
                    column(1,actionButton("button4","Back")),
                    column(2,actionButton("button5","Next"))),
                
        ), # end of third tab item
        
        
        # Fourth Tab: Industry Preferences
        tabItem(tabName = "tab4", h2("Industry Preferences"), # tab item header
                fluidRow(
                    column(1,actionButton("button6","Back")),
                    column(2,actionButton("button7","Next")))
                
        ), # end of fourth tab item
        
        
        # Fifth Tab: Portfolio Construction
        tabItem(tabName = "tab5", h2("Portfolio Construction"), # tab item header
                fluidRow(
                    column(7,actionButton("button8","Back")))
                
        ) # end of fifth tab item
        
        
    ) # bracket from tab items
) # End of the dashboardBody


ui <- dashboardPage(header, sidebar, body)


server <- function(input, output, session) {
    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
                            ##---Risk Preferences---##
    
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
            lbl2<-"Start"
        }else{
            lbl2<-"Next Draw"
        }
        actionButton("nextdraw",label=lbl2)
    })
    
    # Random draw function for individual draws
    rand_draw <- function() {
        req(input$initial_wealth)
        req(input$rpref)
        req(input$inv_horizon)
        
        sim$resetindicator<-1 # change button label
        
        sim$numb <- input$initial_wealth * exp((drift[input$rpref]-(1/2)*(diffusion[input$rpref])^2)*input$inv_horizon + diffusion[input$rpref]*sqrt(input$inv_horizon)*rnorm(1))
        sim$data <<- c(sim$data, sim$numb)
        
        sim$data
    }
    
    ## when nextweek button is pressed
    observeEvent(input$nextdraw,{
        rand_draw()
    })
    
    
    ###
    session1 <- reactiveValues()
    session1$timer <- reactiveTimer(Inf)
    
    observeEvent(input$play,{
        session1$timer<-reactiveTimer(100)
        observeEvent(session1$timer(),{
            rand_draw()
        })
    })
    
    
    observeEvent(input$stop,{
        session1$timer<- reactiveTimer(Inf)
    })
    
    
    ## when reset button is pressed (set everything to original values, plus set seed)
    observeEvent(input$reset,{
        
        sim$resetindicator<-0
        
        sim$numb <- c(0)
        sim$data <- c(0)
    })
    

    ## main plot output
    output$distPlot <- renderPlot({
        if (sum(sim$data)==0){
            #hist(0, xlim = c(0,input$initial_wealth*5), ylim = c(0,100), xlab = "Terminal Wealth", main = "Potential Evolvement of Wealth")
            return() # no plot if reset
         } else if (length(sim$data)==300) { # automatically reset after # draws
            sim$resetindicator<-0
            sim$numb <- c(0)
            sim$data <- c(0)
            #session1$timer<-reactiveTimer(Inf)
        }
        
        hist(sim$data[sim$data < input$initial_wealth*5], breaks = seq(from = 0, to = (input$initial_wealth*5), by = (input$initial_wealth*5)/30),
             xlim = c(0,input$initial_wealth*5), ylim = c(0,100), xlab = "Terminal Wealth", main = "Potential Evolvement of Wealth")
        # hist(sim$data, breaks = seq(from = 0, to = (input$initial_wealth*5), by = (input$initial_wealth*5)/30),
        #      xlim = c(0,input$initial_wealth*5), ylim = c(0,100), xlab = "Terminal Wealth", main = "Potential Evolvement of Wealth")
        grid()
        points(x = input$initial_wealth, y = 0, pch = 24, bg = "grey", cex = 2)
        abline(v = mean(sim$data), col = "blue", lwd = 2, lty = 2)
        
        abline(v = sim$data[order(sim$data)[length(sim$data)*0.9]], col = "green", lwd = 2, lty = 2)
        abline(v = sim$data[order(sim$data)[length(sim$data)*0.1]], col = "red", lwd = 2, lty = 2)
        
        legend("topright", legend = c("90 out of 100 boundary", "10 out of 100 boundary", "Average Terminal Wealth", "Initial Investment"),
               col=c("green", "red", "blue", "grey"), lty = c(2, 2, 2, NA), pch = c(NA, NA, NA, 24), box.lty=0, cex = 1.2)
    })
    
    ## main plot output Finish
    output$distPlotFinish <- renderPlot({
        terminal_wealth <<- input$initial_wealth * exp((drift[input$rpref2]-(1/2)*(diffusion[input$rpref2])^2)*input$inv_horizon + diffusion[input$rpref2]*sqrt(input$inv_horizon)*rnorm(1:draws))
        
        hist(terminal_wealth[terminal_wealth >= 0 & terminal_wealth < input$initial_wealth*5],
             breaks = seq(from = 0, to = (input$initial_wealth*5), by = (input$initial_wealth*5)/30),
             xlim = c(0,input$initial_wealth*5),
             xlab = "Terminal Wealth", main = "Potential Evolvement of Wealth")
        grid()
        #abline(v = input$initial_wealth, col = "blue", lwd = 2)
        points(x = input$initial_wealth, y = 0, pch = 24, bg = "grey", cex = 2)
        abline(v = mean(terminal_wealth), col = "blue", lwd = 2, lty = 2)
        
        abline(v = terminal_wealth[order(terminal_wealth)[draws*0.9]], col = "green", lwd = 2, lty = 2)
        abline(v = terminal_wealth[order(terminal_wealth)[draws*0.1]], col = "red", lwd = 2, lty = 2)
        
        legend("topright", legend = c("90 out of 100 boundary", "10 out of 100 boundary", "Average Terminal Wealth", "Initial Investment"),
               col=c("green", "red", "blue", "grey"), lty = c(2, 2, 2, NA), pch = c(NA, NA, NA, 24), box.lty=0, cex = 1.2)
    })
    
    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
                            ##---Map---##

    # ------------------------ 
    # Read multiple shape files with standardized names
    # all available countries are grouped by continent
    
    region <- c("africa", "antarctica", "asia", "europe", "northamerica", "oceania", "southamerica")
    groups <- c("Africa", "Antarctica", "Asia", "Europe", "North America", "Oceania", "South America")
    colors <- c("red", "blue", "green", "yellow", "purple", "turquoise", "grey")
    
    for (i in region) {
        filestest.i <- geojson_read(as.character(paste(i, "geo.json", sep = ".")), what = "sp")
        assign(as.character(paste("files", i, sep = ".")), filestest.i)
    }
    
    rm(filestest.i)
    
    # ------------------------
    # initiate the map built with leaflet
    
    
    
    
            
        mymap <- leaflet() %>%
        
        setView(lng = 0, lat = 30, zoom = 2) %>%
        
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE))
    #---------------------------------
    # add multiple the several layers to combine the single polygons 
        
        for (reg.N in 1:length(region)) {
            reg <- region[reg.N] # gives the region "code"
            tmp <- get(paste("files", reg, sep = ".")) #gives the file name
            
            mymap <- mymap %>%
                addPolygons(data = tmp, 
                            fillColor = colors[reg.N], 
                            color = "#000000", 
                            opacity = 1, 
                            fillOpacity = 0.7,
                            dashArray = "3",
                            stroke = TRUE,
                            weight = 1.5, 
                            smoothFactor = 0.2,
                            #highlight = highlightOptions(
                                #weight = 1,
                                #color = "#000000",
                                #dashArray = "3",
                                #fillOpacity = 0.8,
                                #bringToFront = TRUE),
                            
                            label = paste(groups[reg.N]),
                            group = paste(groups[reg.N])
                ) 
        }
        
        #---------------------------------
        # set up layer controls
        
        mymap <- mymap %>%
            addLayersControl(overlayGroups = groups,
                             options = layersControlOptions(collapsed = FALSE))
    
    # ------------------------
    # integrate the map into shiny
    
    output$mymap <- renderLeaflet({mymap})

    
    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
                                ##---ValueBoxes---##
    output$horizonBox <- renderValueBox({
        valueBox(
            paste0(input$inv_horizon, " years"), "Investment Horizon", icon = icon("hourglass-half"),
            color = "blue"
        )
    })
    
    output$returnBox <- renderValueBox({
        valueBox(
            paste0(drift[input$rpref] * 100, "%"), "Return", icon = icon("chart-line"),
            color = "green"
        )
    })
    
    output$stdBox <- renderValueBox({
        valueBox(
            paste0(diffusion[input$rpref] * 100, "%"), "Standard Deviation", icon = icon("square-root-alt"),
            color = "red"
        )
    })
    
    output$avgBox <- renderValueBox({
        valueBox(
            paste0(round(mean(sim$data)), "$"), "Average Value", icon = icon("hand-holding-usd"),
            color = "blue"
        )
    })
    
    output$uplimBox <- renderValueBox({
        valueBox(
            paste0(round(sim$data[order(sim$data)[length(sim$data)*0.9]] - input$initial_wealth), "$"), "90% Limit Profit", icon = icon("greater-than"),
            color = "green"
        )
    })
    
    output$lowlimBox <- renderValueBox({
        valueBox(
            paste0(round(sim$data[order(sim$data)[length(sim$data)*0.1]] - input$initial_wealth), "$"), "10% Limit Loss", icon = icon("less-than"),
            color = "red"
        )
    })
    
    output$horizonBox1 <- renderValueBox({
        valueBox(
            paste0(input$inv_horizon, " years"), "Investment Horizon", icon = icon("hourglass-half"),
            color = "blue"
        )
    })
    
    output$returnBox1 <- renderValueBox({
        valueBox(
            paste0(drift[input$rpref2] * 100, "%"), "Return", icon = icon("chart-line"),
            color = "green"
        )
    })
    
    output$stdBox1 <- renderValueBox({
        valueBox(
            paste0(diffusion[input$rpref2] * 100, "%"), "Standard Deviation", icon = icon("square-root-alt"),
            color = "red"
        )
    })
    
    output$avgBox1 <- renderValueBox({
        valueBox(
            paste0(round(mean(terminal_wealth)), "$"), "Average Value", icon = icon("hand-holding-usd"),
            color = "blue"
        )
    })
    
    output$uplimBox1 <- renderValueBox({
        valueBox(
            paste0(round(terminal_wealth[order(terminal_wealth)[draws*0.9]] - input$initial_wealth), "$"), "90% Limit Profit", icon = icon("greater-than"),
            color = "green"
        )
    })
    
    output$lowlimBox1 <- renderValueBox({
        valueBox(
            paste0(round(terminal_wealth[order(terminal_wealth)[draws*0.1]] - input$initial_wealth), "$"), "10% Limit Loss", icon = icon("less-than"),
            color = "red"
        )
    })
    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
                        ##---Switch Buttons---##
    
    # Switch Tabs with action buttons
    observeEvent(
        input$button1, {
            newtab <- switch(input$tabs, "tab1" = "tab2")
            updateTabItems(session, "tabs", newtab)
          }
        )
    
    observeEvent(
        input$button2, {
            newtab <- switch(input$tabs, "tab2" = "tab1")
            updateTabItems(session, "tabs", newtab)
        }
    )
    
    observeEvent(
        input$button3, {
            newtab <- switch(input$tabs, "tab2" = "tab3")
            updateTabItems(session, "tabs", newtab)
        }
    )
    
    observeEvent(
        input$button4, {
            newtab <- switch(input$tabs, "tab3" = "tab2")
            updateTabItems(session, "tabs", newtab)
        }
    )
    
    observeEvent(
        input$button5, {
            newtab <- switch(input$tabs, "tab3" = "tab4")
            updateTabItems(session, "tabs", newtab)
        }
    )
    
    observeEvent(
        input$button6, {
            newtab <- switch(input$tabs, "tab4" = "tab3")
            updateTabItems(session, "tabs", newtab)
        }
    )
    
    observeEvent(
        input$button7, {
            newtab <- switch(input$tabs, "tab4" = "tab5")
            updateTabItems(session, "tabs", newtab)
        }
    )
    
    observeEvent(
        input$button8, {
            newtab <- switch(input$tabs, "tab5" = "tab4")
            updateTabItems(session, "tabs", newtab)
        }
    )
    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
} # end of server

runApp(shinyApp(ui,server),launch.browser = TRUE)
#shinyApp(ui,server)