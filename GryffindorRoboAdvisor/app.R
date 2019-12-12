library(shiny)
library(shinydashboard)
library(ggplot2)
library(leaflet)
library(RJSONIO)
library(rjson)
library(githubinstall)
#library(rCharts)
library(readr)
library(shinythemes)
library(devtools)
#install_github("nik01010/dashboardthemes")
library(dashboardthemes)
library(geojsonio)
#devtools::install_github("RinteRface/shinydashboardPlus")
library(shinyWidgets)
library(rgdal)
#install.packages("tinytex")
#tinytex::install_tinytex()
library(markdown)
library(rmarkdown)
library(shinycssloaders)


header <- dashboardHeader(
    title = shinyDashboardLogoDIY(boldText =  tagList(shiny::icon("robot"),
                                                      "Gryffindor"),
                                  mainText = "Robo-Advisor",
                                  badgeText = "Group 2",
                                  badgeBackColor = "#40E0D0",
                                  badgeTextColor = "white"),
    titleWidth = 300

)


sidebar <- dashboardSidebar(width = 280,
                            sidebarMenu(id = "tabs",
                                        menuItem("Identify your goals",
                                                 tabName = "tab1"),
                                        menuItem("Verify Risk Preference",
                                                 tabName = "tab2"),
                                        menuItem("Geographical Preferences",
                                                 tabName = "tab3"),
                                        menuItem("Industry Preferences",
                                                 tabName = "tab4"),
                                        menuItem("Portfolio Construction",
                                                 tabName = "tab5")
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
                       box(title = "How much money would you like to invest?",
                           width = 0.25,
                           numericInput("initial_wealth",
                                        "Euros:",
                                        value = 10000,
                                        min = 0,
                                        max = NA)
                       ),
                       
                       box(
                           title = "What is your investment horizon?",
                           width = 0.25,
                           knobInput(
                               inputId = "inv_horizon",
                               label = "Years:",
                               value = 10,
                               min = 1,
                               max = 30,
                               displayPrevious = TRUE, 
                               lineCap = "round",
                               fgColor = "#428BCA",
                               inputColor = "#428BCA")
                       ),
                       
                       box(title = "Estimate your risk preference",
                           width = 0.25,
                           sliderInput("rpref",
                                       "Risk: ",
                                       value = 4,
                                       min = 1,
                                       max = 7,
                                       step = 1),
                           "Estimate your risk on the scale from 1 to 7,
                                   where 1 is the lowest tolerance towards risk,
                                   and 7 is the highest"
                       ),
                       
                       hr(style="border-color: grey;"),

                       box(title = "Simulate possible outcomes",
                           width = 0.25,
                           fluidRow(
                               column(7,uiOutput("resetbutton")),
                               column(3,uiOutput("startbutton"))),
                           fluidRow(
                               column(7,
                                      actionBttn(
                                        inputId = "stop",
                                        label = NULL,
                                        style = "material-circle", 
                                        color = "warning",
                                        icon = icon("pause")
                                        )#actionButton("stop","Stop")
                                      ),
                               
                               column(3,
                                      actionBttn(
                                        inputId = "play",
                                        label = NULL,
                                        style = "material-circle", 
                                        color = "primary",
                                        icon = icon("play")
                                        )#actionButton("play","Play")
                                      )
                               ),
                           
                           sliderTextInput(
                             inputId = "speed",
                             label = "Simulation Speed:", 
                             #grid = TRUE,
                             #force_edges = TRUE,
                             choices = c("Very Slow", 
                                         "Slow",
                                         "Moderate",
                                         "Fast",
                                         "Very Fast"),
                             selected = "Moderate"
                           ),
                           
                           tags$p("Press the", tags$em("Start"),
                                  "button in order to initiate the simulation.",
                                  br(),
                                  "Press", tags$em("Next Draw"),
                                  "in order to see one possible realization.",
                                  br(),
                                  "Use the", tags$em("Play"), "and",
                                  tags$em("Stop"), "button",
                                  "in order run the simulation automatically.",
                                  br(),
                                  "Finally use", tags$em("Reset"),
                                  "in order to start anew."),
                       ),
                       
                       actionBttn(
                           inputId = "button1",
                           label = "Next",
                           style = "unite", 
                           color = "success"
                       )
                       

                ), 
################ end of first column object#####################################
################################################################################

                column(width = 8,
                       tabBox(
                         id = "tabset1",
                         title = tagList(shiny::icon("dice"),
                                         "Portfolio Simulation"),
                         width = 12,
                         height = "550",
                         tabPanel(
                           title = tagList(shiny::icon("chart-bar"),
                                           "Histogram"),
                           plotOutput('distPlot', height = "500")
                         ),
                         
                         tabPanel(
                           title = tagList(shiny::icon("info"), "Details"),
                           "Explanation to the return sampling, i.e. GBM, 90% & 10% VaR ...."
                         )
                       ), 

                       # Dynamic valueBoxes
                       withSpinner(valueBoxOutput("horizonBox")),
                       valueBoxOutput("returnBox"),
                       valueBoxOutput("stdBox"),
                       valueBoxOutput("avgBox"),
                       valueBoxOutput("uplimBox"),
                       valueBoxOutput("lowlimBox")
                )
            )
###################### end of fluid row ########################################
        ),
###################### end of first tab item####################################
################################################################################


######## Second Tab: Risk Evaluation ###########################################
        tabItem(tabName = "tab2", h2("Risk Evaluation"), # tab item header
            fluidRow(
                column(width = 4,
                       box(
                           title = "Estimate your risk preference",
                           width = 0.25,
                           sliderInput("rpref2","Risk: ",
                                       value = 4,
                                       min = 1,
                                       max = 7,
                                       step = 1),
                           "Estimate your risk on the scale from 1 to 7,
                                   where 1 is the lowest tolerance towards risk,
                                   and 7 is the highest"
                       ),

                       fluidRow(
                           actionBttn(
                               inputId = "button2",
                               label = "Back",
                               style = "unite", 
                               color = "danger"
                           ),
                           
                           actionBttn(
                               inputId = "button3",
                               label = "Next",
                               style = "unite", 
                               color = "success"
                           ))

                ),
################# end of first column object ###################################
################################################################################

                column(width = 8,
                       tabBox(
                         id = "tabset1",
                         title = tagList(shiny::icon("dice"),
                                         "Portfolio Simulation"),
                         width = 12,
                         height = "550",
                         tabPanel(title = tagList(shiny::icon("chart-bar"),
                                                  "Histogram"),
                                  plotOutput('distPlotFinish',
                                             height = "500")),
                         tabPanel(title = tagList(shiny::icon("info"),
                                                  "Details"),
                                  "Explanations ...")
                       ), 

                       # Dynamic valueBoxes
                       withSpinner(valueBoxOutput("horizonBox1")),
                       valueBoxOutput("returnBox1"),
                       valueBoxOutput("stdBox1"),
                       valueBoxOutput("avgBox1"),
                       valueBoxOutput("uplimBox1"),
                       valueBoxOutput("lowlimBox1")

                )

            )
############# end of fluidrow ##################################################
################################################################################
        ),
######### end of second tab item ###############################################
################################################################################

        # Third Tab: Geographical Preferences
        tabItem(tabName = "tab3",
                h2("Geographical Preferences"), # tab item header
                
                ######## Inputs to let the user choose regions via map
                tabBox(
                  id = "tabset1",
                  height = "550",
                  title = tagList(shiny::icon("map-pin"),
                                  "Geographical Preferences"),
                  width = 8,
                  tabPanel(
                    title = tagList(shiny::icon("globe-americas"),"Map"),
                    withSpinner(leafletOutput("mymap", height = "500"))
                  ),
                  
                  tabPanel(
                    title = tagList(shiny::icon("info"), "Details"),
                    "Explanations about the map and how to use it ...."
                  )
                ), 
                
                ######## Inputs to let the user switch on and of industries
                tabBox(
                  title = tagList(shiny::icon("industry"), "Industry Preferences"),
                  width = 4,
                  id = "tabset1",
                  height = "550",
                  tabPanel(
                    title = tagList(shiny::icon("clipboard-check"), "Sectors"),
                    multiInput(
                      inputId = "industry1",
                      label = "Industries",
                      choices = c(
                        "Banks" = "banks",
                        "Resources" = "resources",
                        "Chemicals" = "chemicals",
                        "Construction" = "construction",
                        "Financials" = "financials",
                        "Food" = "food",
                        "Health" = "health",
                        "Industrial" = "industrial",
                        "Insurance" = "insurance",
                        "Media" = "media",
                        "Energy" = "energy",
                        "Personal" = "personal",
                        "Retail" = "retail",
                        "Tech" = "tech",
                        "Telecom" = "telecom",
                        "Travel" = "travel",
                        "Utilities" = "utilities"
                      ),
                      
                      width = "100%",
                      options = list(
                        selected_header = "I don't want to invest in:",
                        non_selected_header = "Industries",
                        limit = 16 ####the user should't deselect all industries
                      )
                    )
                  ),
                  
                  
                  tabPanel(title = tagList(shiny::icon("info"), "Details"),
                           "Explanations about the map and how to use it ....")
                ),
                
                hr(style = "border-color: grey;"),
                
                fluidRow(
                  actionBttn(
                    inputId = "button4",
                    label = "Back",
                    style = "unite",
                    color = "danger"
                  ),
                  
                  actionBttn(
                    inputId = "button5",
                    label = "Next",
                    style = "unite",
                    color = "royal"
                    )
                ), 
                
                tableOutput("table"),
                plotOutput("testgraph"),
                textOutput("Error_regions"),
                useSweetAlert()

        ),
######### end of third tab item ################################################
################################################################################


        # Fourth Tab: Industry Preferences
        tabItem(tabName = "tab4",
                h2("Portfolio Construction"), # tab item header
                fluidRow(
                  plotOutput("ourPF"),
                  radioButtons('format',
                               'Document format',
                               c('PDF','HTML', 'Word'),
                               inline = TRUE),
                  
                  downloadButton('downloadReport'),
                    actionBttn(
                        inputId = "button6",
                        label = "Back",
                        style = "unite", 
                        color = "danger"
                    )
                  )

        )
######### end of fourth tab item ###############################################
################################################################################
    )
##### end of all tab items #####################################################
################################################################################
)
# End of the dashboardBody######################################################
################################################################################

ui <- dashboardPage(header, sidebar, body,tags$head(
                    tags$style(
                    HTML(".shiny-notification {
                    height: 100px;
                    width: 800px;
                    position:fixed;
                    top: calc(50% - 50px);;
                    left: calc(50% - 400px);;
                    }
                    "
                    )))
                    
)



server <- function(input, output, session) {

############################ Risk Preference ###################################
################################################################################
          

### Histogram Simulation ######################################################

    sim <- reactiveValues() # reactive to store all reactive variables
    sim$resetindicator <- 0 # used to change button labels
    sim$numb <- c()
    sim$data <- c()
    sim$terminal_wealth <- c()
    diffusion <- c(0.01, 0.02, 0.05, 0.1, 0.15, 0.25, 0.3)
    drift <- c(0.001,0.005,0.015,0.02, 0.03, 0.06, 0.1)
    draws <- 1000
    speed <- seq(1000, 100, length.out = 5)

### dynamic reset button label #################################################
    
    output$resetbutton <- renderUI({
      if (sim$resetindicator == 0) {
        lbl <- "Set Parameters"
        
      } else {
        lbl <- "Reset"
      }
      actionButton("reset", label = lbl)
    })

### dynamic start button label #################################################
    
    output$startbutton <- renderUI({
      if (sum(sim$data) == 0) {
        lbl2 <- "Start"
        
      } else {
        lbl2 <- "Next Draw"
      }
      actionButton("nextdraw", label = lbl2)
    })

### Random draw function for individual draws ##################################
    
    rand_draw <- function() {
        req(input$initial_wealth)
        req(input$rpref)
        req(input$inv_horizon)

        sim$resetindicator <- 1 # change button label

        sim$numb <- input$initial_wealth * exp((drift[input$rpref] - (1 / 2) *
                                                  (diffusion[input$rpref]) ^ 2)*
                                                 input$inv_horizon +
                                                 diffusion[input$rpref] *
                                                 sqrt(input$inv_horizon)* 
                                                 rnorm(1))
        sim$data <<- c(sim$data, sim$numb)

        sim$data
    }

### when next-draw button is pressed ###########################################

        observeEvent(input$nextdraw,{
        rand_draw()
          })
    
        session1 <- reactiveValues()
        session1$timer <- reactiveTimer(Inf)

    
        observeEvent(
          input$play,
          {session1$timer <- reactiveTimer(speed[which(c("Very Slow",
                                                         "Slow",
                                                         "Moderate",
                                                         "Fast",
                                                         "Very Fast") ==
                                                         input$speed)]) # 100
          observeEvent(session1$timer(), {
          rand_draw()
            })
######### end of subevent ######################################################
          })


        observeEvent(input$stop, {
          session1$timer<- reactiveTimer(Inf)
          })


## when reset button is pressed, set everything to original values plus set seed
        observeEvent(input$reset, {
            
          sim$resetindicator <- 0
          sim$numb <- c(0)
          sim$data <- c(0)
          })


#### main plot output - histogram first tab ####################################
################################################################################
        
        output$distPlot <- renderPlot({
          if (sum(sim$data) == 0) {
            return() # no plot if reset everything was reset
            
          } else if (length(sim$data) == 300) {
            # automatically reset after # draws to exit
            sim$resetindicator <- 0
            sim$numb <- c(0)
            sim$data <- c(0)
            #session1$timer<-reactiveTimer(Inf)
          }
          
          hist(
            sim$data[sim$data < input$initial_wealth * 5],
            breaks = seq(
              from = 0,
              to = (input$initial_wealth * 5),
              by = (input$initial_wealth * 5) / 30
            ),
            
            xlim = c(0, input$initial_wealth * 5),
            ylim = c(0, 100),
            xlab = "Terminal Wealth",
            main = "Potential Evolvement of Wealth"
          )
          
          grid()
          
          points(
            x = input$initial_wealth,
            y = 0,
            pch = 24,
            bg = "grey",
            cex = 2
          )
          
######## include a vertical line indicating the mean ###########################
          abline(
            v = mean(sim$data),
            col = "blue",
            lwd = 2,
            lty = 2
          )
          
######## include a vertical line indicating the 90% percentile #################
          abline(
            v = sim$data[order(sim$data)[length(sim$data) * 0.9]],
            col = "green",
            lwd = 2,
            lty = 2
          )
          
######## inlude a vertical line indicating the 10% percentile ##################
          abline(
            v = sim$data[order(sim$data)[length(sim$data) * 0.1]],
            col = "red",
            lwd = 2,
            lty = 2
          )
          
          legend(
            "topright",
            legend = c(
              "90 out of 100 boundary",
              "10 out of 100 boundary",
              "Average Terminal Wealth",
              "Initial Investment"
            ),
            
            col = c("green",
                    "red",
                    "blue",
                    "grey"),
            
            lty = c(2, 2, 2, NA),
            pch = c(NA, NA, NA, 24),
            box.lty = 0,
            cex = 1.2
          )
        })
        
### end of main plot - histogram first tab #####################################
################################################################################
        
### start of main plot - histogram second tab ##################################
################################################################################
        
        output$distPlotFinish <- renderPlot({
          sim$terminal_wealth <- input$initial_wealth *
            exp((drift[input$rpref2] - (1 / 2) *
                   (diffusion[input$rpref2]) ^ 2) *
                  input$inv_horizon + diffusion[input$rpref2] *
                  sqrt(input$inv_horizon) * rnorm(1:draws)
            )
          
          hist(
            sim$terminal_wealth[sim$terminal_wealth >= 0 &
                                  sim$terminal_wealth <
                                  input$initial_wealth * 5],
            breaks = seq(
              from = 0,
              to = (input$initial_wealth * 5),
              by = (input$initial_wealth * 5) / 30
            ),
            xlim = c(0, input$initial_wealth * 5),
            xlab = "Terminal Wealth",
            main = "Potential Evolvement of Wealth"
          )
          grid()
          
          points(
            x = input$initial_wealth,
            y = 0,
            pch = 24,
            bg = "grey",
            cex = 2
          )

######## include a vertical line indicating the mean ###########################
          abline(
            v = mean(sim$terminal_wealth),
            col = "blue",
            lwd = 2,
            lty = 2
          )
          
######## include a vertical line indicating the 90% percentile #################
          abline(
            v = sim$terminal_wealth[order(sim$terminal_wealth)[draws * 0.9]],
            col = "green",
            lwd = 2,
            lty = 2
          )
          
######## include a vertical line indicating the 10% percentile #################
          abline(
            v = sim$terminal_wealth[order(sim$terminal_wealth)[draws * 0.1]],
            col = "red",
            lwd = 2,
            lty = 2
          )
          
          legend(
            "topright",
            legend = c(
              "90 out of 100 boundary",
              "10 out of 100 boundary",
              "Average Terminal Wealth",
              "Initial Investment"
            ),
            
            col = c("green", "red", "blue", "grey"),
            lty = c(2, 2, 2, NA),
            pch = c(NA, NA, NA, 24),
            box.lty = 0,
            cex = 1.2
          )
        })
############ end of mainplot - histogram second tab ############################
################################################################################
          
# Set the selected input of the first slider equal to the second et vice versa
        observe({
          updateSliderInput(session, "rpref2", value = input$rpref)
          })
          
        observe({
          updateSliderInput(session, "rpref", value = input$rpref2)
          })
    


############ Country and Industry Subsetting ###################################
################################################################################
        
######## To make the code better readable, the webscrapping process is placed #
######## in a seperate file ###################################################
        
        source("robodata.R")
        
####### The output file of the webscraping script is called "OVR" and contains #
####### all available information in one data frame. This is split up into the #
####### dates, the commodity index, the two debt indices to be left with the ###
####### different equity indices. ##############################################
        
        dates <- ovr[, 1]
        commodities <- ovr[, 56]
        longbond <- ovr[, 57]
        shortbond <- ovr[, 58]
        data <- ovr[, -c(1, 56:58)]
        
####### in order to make the data frame subsettable, it must be in an reactive #
####### enviornment. ###########################################################
        
        makeReactiveBinding("data")
        
####### show the subsetted data frame in the app ###############################
        output$table <- renderTable({
          
######### due to syntax of reactive datas, the subsetting has to happen within #
######### as well as outside the reactive enviornment ##########################
          
          data <- ovr[, -c(1, 56:58)]
          
######### Subsetting by region - the user chooses the region(s) he or she ######
######### does not want to invest in ###########################################
          if (!("NorthAmerica" %in% input$mymap_groups)) {
            data <- data[ , -which(names(data) %in%
                                     grep("US", names(data), value = TRUE))]
            }
          
          if (!("Europe" %in% input$mymap_groups)) {
            data <- data[ , -which(names(data) %in%
                                     grep("EU", names(data), value = TRUE))]
            }
          
          if (!("Asia" %in% input$mymap_groups)) {
            data <- data[ , -which(names(data) %in%
                                     grep("AS", names(data), value = TRUE))]
            }
          
          if (!("Africa" %in% input$mymap_groups)) {
            data <- data[ , -which(names(data) %in%
                                     grep("Africa", names(data), value = TRUE))]
            }
          
          if (!("Australia" %in% input$mymap_groups)) {
            data <- data[ , -which(names(data) %in%
                                     grep("Australia", names(data),
                                          value = TRUE))]
            }
          
          if (!("Latinamerica" %in% input$mymap_groups)) {
            data <- data[ , -which(names(data) %in%
                                     grep("Latinamerica", names(data),
                                          value = TRUE))]
            }
          
          # if (!("Antarctica" %in% input$mymap_groups)) {
          #   data <- data[ , -which(names(data) %in%
          #                            grep("Antarctica", names(data),
          #                                 value = TRUE))]
          #   }
      

######### Subsetting by industry - the user chooses the indutries he or she ####
######### does not want to invest in ###########################################
          
          if ("banks" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                     grep("banks", names(data), value = TRUE))]
            }
          
          if ("resources" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                     grep("resources", names(data),
                                          value = TRUE))]
            }
          
          if ("chemicals" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                     grep("chemicals", names(data),
                                          value = TRUE))]
            }

          if ("construction" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                     grep("construction", names(data),
                                          value = TRUE))]
            }
          
          if ("financials" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                     grep("financials", names(data),
                                          value = TRUE))]
            }
          
          if ("food" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                     grep("food", names(data), value = TRUE))]
            }
          
          if ("health" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                     grep("health", names(data), value = TRUE))]
            }
          
          if ("industrial" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                     grep("industrial", names(data),
                                          value = TRUE))]
            }
          
          if ("insurance" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                     grep("insurance", names(data),
                                          value = TRUE))]
            }
          
          if ("energy" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                     grep("energy", names(data), value = TRUE))]
            }
          
          if ("personal" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                     grep("personal", names(data),
                                          value = TRUE))]
            }
          
          if ("retail" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                     grep("retail", names(data), value = TRUE))]
            }
          
          if ("tech" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                     grep("tech", names(data), value = TRUE))]
            }
          
          if ("telecom" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                     grep("telecom", names(data),
                                          value = TRUE))]
            }
          
          if ("travel" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                     grep("travel", names(data), value = TRUE))]
            }
          
          if ("utilities" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                     grep("utilities", names(data),
                                          value = TRUE))]
            }
          
          data
          
######### End of subsetting, where the data is used for the table ##############
################################################################################
          })
    
    
####### Actual subsetting, where the data is then used for the PF building #####
################################################################################
        
####### Get webscraped Data (the ovr file from the scrapping script) ###########
        
        data <- ovr[, -c(1, 56:58)]

####### use the data frame in a reactive enviornment ###########################
        
        newData <- reactive({
######### Again, the data has to be used within and outside the reactive #######
######### enviornment. #########################################################
          data <- ovr[, -c(1, 56:58)]

######### Subsetting by region - the user selects the region he or she does not 
######### want to be invested in ###############################################
          
          if (!("NorthAmerica" %in% input$mymap_groups)) {
            data <- data[ , -which(names(data) %in%
                                     grep("US", names(data), value = TRUE))]
            }
          
          if (!("Europe" %in% input$mymap_groups)) {
            data <- data[ , -which(names(data) %in%
                                     grep("EU", names(data), value = TRUE))]
            }
          
          if (!("Asia" %in% input$mymap_groups)) {
            data <- data[ , -which(names(data) %in%
                                     grep("AS", names(data), value = TRUE))]
            }
          
          if (!("Africa" %in% input$mymap_groups)) {
            data <- data[ , -which(names(data) %in%
                                     grep("Africa", names(data), value = TRUE))]
            }
          
          if (!("Australia" %in% input$mymap_groups)) {
            data <- data[ , -which(names(data) %in%
                                     grep("Australia", names(data),
                                          value = TRUE))]
            }
          
          if (!("Latinamerica" %in% input$mymap_groups)) {
            data <- data[ , -which(names(data) %in%
                                     grep("Latinamerica", names(data),
                                          value = TRUE))]
            }
          
          # if (!("Antarctica" %in% input$mymap_groups)) {
          #   data <- data[ , -which(names(data) %in%
          #                            grep("Antarctica", names(data),
          #                                 value = TRUE))]
          #   }
      
      
######### Subsetting by industry - the user chooses the indutries he or she ####
######### does not want to invest in ###########################################
          
          if ("banks" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                     grep("banks", names(data), value = TRUE))]
            }
          
          if ("resources" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                     grep("resources", names(data),
                                          value = TRUE))]
            }
          
          if ("chemicals" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                     grep("chemicals", names(data),
                                          value = TRUE))]
            }
          
          if ("construction" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                     grep("construction", names(data),
                                          value = TRUE))]
            }
          
          if ("financials" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                     grep("financials", names(data),
                                          value = TRUE))]
            }
          
          if ("food" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                     grep("food", names(data),
                                          value = TRUE))]
            }
          
          if ("health" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                     grep("health", names(data), value = TRUE))]
            }
          
          if ("industrial" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                     grep("industrial", names(data),
                                          value = TRUE))]
            }
          
          if ("insurance" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                     grep("insurance", names(data),
                                          value = TRUE))]
            }
          
          if ("energy" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                     grep("energy", names(data), value = TRUE))]
            }
          
          if ("personal" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                     grep("personal", names(data),
                                          value = TRUE))]
            }
          
          if ("retail" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                     grep("retail", names(data), value = TRUE))]
            }
          
          if ("tech" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                     grep("tech", names(data), value = TRUE))]
            }
          
          if ("telecom" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                     grep("telecom", names(data),
                                          value = TRUE))]
            }
          
          if ("travel" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                     grep("travel", names(data), value = TRUE))]
            }
          
          if ("utilities" %in% input$industry1) {
            data <- data[ , -which(names(data) %in%
                                     grep("utilities", names(data),
                                          value = TRUE))]
            }
          
          data
          
######### End of subsetting, where the data is used for PF building ############
################################################################################
          })

    
    # output$testgraph <- renderPlot({
    # 
    #   data <- newData()
    # 
    #   graph <- plot.ts(data[, -1])
    # 
    #   return(graph)
    # 
    # })
    

####### Initiation of the map, where the user can (de)select regions ###########
################################################################################
    
####### Read multiple shape files with standardized names ######################
####### all available countries are grouped by continent #######################
        
        region <- c("africa",
                    "antarctica",
                    "asia", "europe",
                    "northamerica",
                    "oceania",
                    "latinamerica")
        
        groups <- c("Africa",
                    "Antarctica",
                    "Asia",
                    "Europe",
                    "NorthAmerica",
                    "Oceania",
                    "Latinamerica")
        
        colors <- c("red", "blue", "green", "yellow",
                    "purple", "turquoise", "grey")
        
        for (i in region) {
          filestest.i <- geojson_read(as.character(
            paste(getwd(),
                  "Map",
                  paste(i,
                        "geo.json",
                        sep = "."),
                  sep = "/")),
            what = "sp")
          
          assign(as.character(paste("files", i, sep = ".")), filestest.i)
        }
        
####### remove the "dummy"-file ################################################        
        rm(filestest.i)
    
  
####### initiate the map built with leaflet ####################################
################################################################################
        
        foundmap <- leaflet() %>%
        
        setView(lng = 0, lat = 30, zoom = 2) %>%
        
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE))
    

####### add multiple layers to combine the single polygons #####################
    
        for (reg.N in 1:length(region)) {
          reg <- region[reg.N] # gives the region "code"
          tmp <- get(paste("files", reg, sep = ".")) #gives the file name
          
          foundmap <- foundmap %>%
            addPolygons(data = tmp, 
                        fillColor = colors[reg.N], 
                        color = "#000000", 
                        opacity = 1, 
                        fillOpacity = 0.7,
                        dashArray = "3",
                        stroke = TRUE,
                        weight = 1.5, 
                        smoothFactor = 0.2,
                        label = paste(groups[reg.N]),
                        group = paste(groups[reg.N])
                        )
          }
    
####### set up layer controls to make regions selectable #######################
        
        foundmap <- foundmap %>%
          addLayersControl(overlayGroups = groups,
                           options = layersControlOptions(collapsed = FALSE))


####### integrate the map into shiny ###########################################
        
        output$mymap <- renderLeaflet({foundmap})
    
    

####### Portfolio evaluation functions #########################################
################################################################################
    
    
####### Average return - takes one column as an input ##########################
    
        averagereturn <- function(backtest) {
          portfoliologreturns <- data.frame()
      
          for (r in 1:(nrow(backtest) - 1)) {
            portfoliologreturns[r, 1] <- log(backtest[r + 1, 1]/backtest[r, 1])
            }
          
          averagereturn <- mean(portfoliologreturns[, 1]) * 252
          return(averagereturn)
          
          }
    
####### Calculate max drawdown - takes one column as an input ##################
        
        maxdrawdown <- function(backtest){
          trailingmaxdrawdown <- data.frame()
          
          for (r in 1:(nrow(backtest) - 1)){
            trailingmaxdrawdown[r, 1] <- min(tail(backtest[, 1], -r)) /
                                              backtest[r, ] - 1
            }
          
          maxdrawdown <- min(trailingmaxdrawdown[, 1])
          return(maxdrawdown)
          }
    
####### Calculate annual standard deviation ####################################
    
        yearlystd <- function(backtest){
          portfoliologreturns <- data.frame()
          
          for (r in 1:(nrow(backtest) - 1)){
            portfoliologreturns[r, 1] <- log(backtest[r + 1, 1] /
                                               backtest[r, 1])
            }
          
          yearlystd <- sd(portfoliologreturns[, 1]) * sqrt(252)
          return(yearlystd)
          }
    
####### Calculate sharpe Ratio for maximization ################################
    
        sharpe <- function(expectedreturn, covmatrix, par){
      
######### optim, used, later needs the parameters as vector, but we need it as #
######### matrix here - therefore we have to change it #########################
      
          par <- as.matrix(par)
          par <- rbind(par, 1 - sum(par[, 1]))
      
######### Calculate Portfolio return ###########################################
      
          pfreturn <- t(expectedreturn) %*% par
      
######### Calculate Portfolio standard deviation ###############################
      
          pfvar <- t(par) %*% covmatrix %*% par
      
######### Calculate Sharpe Ratio ###############################################
      
          sharperatio <- pfreturn / (pfvar ^ 0.5)
          
          return(sharperatio)
          }
    

######### Within this section, the PF that are later proposed to our clients ###
######### are evaluated, based on the scrapped data set, that is subsetted #####
######### according to the usere's inputs. #####################################
    
    
######## Sharperatio optimized -  Pure Equity Portfolio ########################
######## Takes a dataframe with all equity indices as input ####################
    
        optimpf <- function(data) {
        
          #get past returns
          returns <- data.frame()
        
          for (c in 1:ncol(data)) {
            for (r in 1:nrow(data) - 1) {
              returns[r, c] <- (data[r + 1, c] - data[r, c]) / data[r, c]
              }
            }
        
# make sure, that the sum of the weights of the PF components adds up to 1, ####
          
          startingweights <- as.matrix(rep(1 / ncol(data),
                                           length.out = (ncol(data) - 1)))
          
          expectedreturn <- as.matrix(rep(0, length.out = ncol(data)))
        
######### pac the returns and covariances into  matrices #######################
          
          returnmatrix <- as.matrix(returns[, ])
          covmatrix <- cov(returnmatrix)
        
######### compute the expected returns for every security ######################
        
          for (c in 1:ncol(returns)) {
            expectedreturn[c, 1] <- mean(returns[,c])
            }
        
######### Find Optimal PF-weights given the lower bound of 0 and the ###########
######### Sharpe - Ratio function defined above. ###############################
        
          optweights <- optim(par = as.vector(startingweights),
                              fn = sharpe,
                              expectedreturn = expectedreturn,
                              covmatrix = covmatrix,
                              control = list(fnscale = -1),
                              lower = 0,
                              method = "L-BFGS-B")
          
          optweights <- as.matrix(optweights$par)
          optweights <- rbind(optweights, 1 - sum(optweights))
        
        
######### Function to standardize PF time series to a starting value of 100 ####
          
          indexportfolio <- data.frame()
          
          for (c in 1:ncol(data)) {
            indexportfolio[1, c] <- 100
          }
          
          for (c in 1:ncol(data)) {
            for (r in 1:nrow(returns)) {
              indexportfolio[r + 1, c] <- indexportfolio[r, c] * 
                                            (1 + returns[r, c])
            }
          }
          
          colnames(indexportfolio) <- NULL
          rownames(indexportfolio) <- NULL
        
######### Form Portfolio with the Sharpe ratio optimal weights #################
        
          portfolio <- data.frame()
          for (r in 1:nrow(indexportfolio)) {
            portfolio[r, 1] <- as.numeric(as.matrix(indexportfolio[r, ]) %*%
                                            optweights)
            }
        
        
######### The output of this function is a PF time series indexed to 100 #######
          
          return(portfolio)

######### end of optimpf function ##############################################           
          }
    
    
######### Equity + Debt Portfolio ##############################################
     
        equityanddebtpf <- function(equity, debt, equityaspercent) {
      
          bondindex <- data.frame(matrix(NA,
                                         ncol = 2,
                                         nrow = nrow(equity)))
          bondindex[, 1] <- debt
          bondindex[, 2] <- debt
          
          debt <- optimpf(bondindex)
          portfolio <- data.frame()
      
          for (r in 1:nrow(equity)){
            portfolio[r, 1] <- equity[r, 1] * equityaspercent +
                                debt[r, 1] * (1 - equityaspercent)
            }
          
          return(portfolio)
          }
        
        indexpf <- function(data) {
          
          returns <- data.frame()
          
          for (c in 1:ncol(data)) {
            for (r in 1:nrow(data) - 1) {
              returns[r, c] <- (data[r + 1, c] - data[r, c]) / data[r, c]
            }
          }
          
          indexportfolio <- data.frame()
          
          for (c in 1:ncol(data)) {
            indexportfolio[1, c] <- 100
            }
       
          for (c in 1:ncol(data)){
            for (r in 1:nrow(returns)){
              indexportfolio[r + 1,c] <- indexportfolio[r, c] *
                                          (1 + returns[r , c])
            }
          }
          return(indexportfolio)
          
        }
    
     
     
     ###---###---###---###---###---###---###---###---###---###---###---###---###
                        ##---Test Functions for Markdown file---##
     
     nicegraph <- function(x=1000) {
       plot(sin(1:x), cos(1:x))
     }
    
    piechart <- function(x=c(0.3,0.2,0.1,0.4)) {
      pie(x)
    }

    randperform <- function(x=cumsum(rnorm(1000,1,5))) {
      plot(x)
    }
    
    testplot <- function(x) {
      plot(x)
    }

     
### Risk Parity Portfolio ######################################################
################################################################################
     
    riskparity <- function(covmatrix, par) {
      
      par <- as.matrix(par)
      par <- rbind(par, 1 - sum(par[, 1]))
      startingweights <- par
      
      marginalrisk <- covmatrix %*% startingweights
      contribution <- as.matrix(as.numeric((startingweights * marginalrisk))) /
        sqrt(as.numeric((t(startingweights) %*% covmatrix %*% startingweights)))
      
      portfoliorisk <- sqrt(as.numeric((t(startingweights) %*% covmatrix %*% 
                                          startingweights)))
      
      
      contributionpercent <- matrix(ncol = 1, nrow = 3)
      for (r in 1:nrow(contribution)) {
        contributionpercent[r, 1] <- contribution[r, 1]/sum(contribution)
      }
      
      
###### Workaround to make the optimizer find a stable minimum ##################
      
      if (par[1, 1] + par[2, 1] > 1){
        portfoliorisk <- 3
      }
      
      portfoliorisk <- abs((contributionpercent[1, 1] - 
                              contributionpercent[2, 1])) +
                       abs((contributionpercent[1, 1] -
                              contributionpercent[3, 1]))
      
      return(portfoliorisk)
    }
    
    riskparitypf <- function(equity, debt, commodity) {
      
      data <- as.matrix(cbind(equity, debt, commodity))
      
      startingweights <- as.matrix(rep(1 / ncol(data),
                                       length.out = (ncol(data) - 1)))
      
      returns <- data.frame()
      for (c in 1:ncol(data)) {
        for (r in 1:nrow(data) - 1) {
          returns[r, c] <- (data[r + 1, c] - data[r, c]) / data[r, c]
        }
      }
      
      returnmatrix <- as.matrix(returns[, ])
      covmatrix <- cov(returnmatrix)

      optweights <- optim(par = as.vector(startingweights),
                          fn = riskparity, 
                          covmatrix = covmatrix,
                          lower = 0,
                          method = "L-BFGS-B")
      
      optweights <- as.matrix(optweights$par)
      optweights <- rbind(optweights, 1 - sum(optweights))
      
      portfolio <- data.frame(100)
      
      for (r in 1:nrow(returns)) {
        portfolio[(1 + r), 1] <- portfolio[r, 1] * 
                                  (1 + (returns[r, 1] * optweights[1, 1] +
                                          returns[r, 2] * optweights[2, 1] +
                                          returns[r, 3] * optweights[3, 1]))
        }
      return(portfolio)
##### end of Risk parity function ##############################################
      }
    
  
##### Minimum Variance Portfolio ###############################################
################################################################################
    minvar <- function(covmatrix,par) {
      
      par <- as.matrix(par)
      par <- rbind(par, 1 - sum(par[, 1]))
      
      std <- t(par) %*% covmatrix %*% par
      
      if (par[1, 1] + par[2, 1] > 1){
        std <- 3
      }
      
      return(std)
    }
    
    minvarpf <- function(data) {
      
      returns <- data.frame()
      for (c in 1:ncol(data)) {
        for (r in 1:nrow(data) - 1) {
          returns[r, c] <- (data[(r + 1), c] - data[r, c]) / data[r, c]
        }
      }
      
      returnmatrix <- as.matrix(returns[, ])
      covmatrix <- as.matrix(cov(returnmatrix))
      
      startingweights <- as.matrix(rep(1 / ncol(data),
                                       length.out = (ncol(data) - 1)))
      
      optweights <- optim(par = as.vector(startingweights),
                          fn = minvar,
                          covmatrix = covmatrix,
                          lower = 0,
                          method = "L-BFGS-B")
      
      optweights <- as.matrix(optweights$par)
      optweights <- rbind(optweights, 1 - sum(optweights))
      
      portfolio <- data.frame(100)
      for (r in 1:nrow(returns)) {
        portfolio[(1 + r), 1] <- portfolio[r, 1] *
          (1 + (as.matrix(returns[r, ]) %*% optweights))
      }
      
      return(portfolio)
### End of Minimum Variance Portfolio function #################################
    }
    
##### Dataspliting and optimizing###############################################
################################################################################
    
    datasplit <- function(subdata,updateProgress = NULL){
      flor <- floor(ncol(subdata) / 15)
      remaining <- ncol(subdata) - 15 * flor
      lastdata <- flor + 1
    
      for (n in 1:flor){
      
        data.n  <- subdata[((n - 1) * 15 + 1):(n * 15)]
        assign(as.character(paste("data", as.character(n), sep = "")), data.n)
      }
    
      data.lastdata <- subdata[(flor * 15):ncol(subdata)]
      assign(as.character(paste("data", as.character((flor + 1)), sep="")),
             data.lastdata)
    
      finaldata <- data.frame(matrix(nrow = nrow(subdata)))[, -1]
      
      new_row <- data.frame(x = rnorm(1), y = rnorm(1))
    
      for (n in 1:(flor + 1)){
      
        dataopt<- minvarpf(get(paste("data", n, sep = "")))
        assign(as.character(paste("dataopt", as.character(n), sep="")), dataopt)
      
        if (is.function(updateProgress)) {
          text <- paste0("x:", round(new_row$x, 2), " y:", round(new_row$y, 2))
          updateProgress(detail = text)
        }
        
        
        finaldata <- cbind(finaldata,dataopt)
      }
      return(finaldata)
### End of Dataspliting function #################################
    }
  
    
    
### call the portfolios according to the user's input ##########################
    
################################################################################
##### short bond only ##########################################################

    output$ourPF <- renderPlot({
      
      # Create the Progress object
      progress <- shiny::Progress$new()
      progress$set(message = "Calculating Optimal Portfolio", value = 0)
      # Close the progress when this reactive exits (even if there's an error)
      on.exit(progress$close())
      
      updateProgress <- function(value = NULL, detail = NULL) {
        if (is.null(value)) {
          value <- progress$getValue()
          value <- value + (progress$getMax() - value) / 3
        }
        progress$set(value = value, detail = detail)
      }
        
      if (input$rpref2 == 1 && input$inv_horizon <= 5) {
  
        plot.ts(shortbond)
        title("short bond")
        summary(shortbond)
      }
    
##### long bond only ###########################################################
      
      if ((input$rpref2 == 2 && input$inv_horizon <= 5) ||
          (input$rpref2 == 1 && input$inv_horizon > 5 &&
           input$inv_horizon <= 10)) {
        
        plot.ts(longbond)
        title("long bond")
        summary(longbond)
      }
      
##### minimum variance PF ######################################################
      
      if ((input$rpref2 == 3 && input$inv_horizon <= 5) ||
         #redundant but included so that the number of conitions = number of PFs
          (input$rpref2 == 3 && input$inv_horizon > 5 &&
           input$inv_horizon <= 10) ||
          (input$rpref2 == 2 && input$inv_horizon > 10)) {
      
####### split the required input df into sub-df's to make them optimizable #####
        finaldata <- datasplit(newData(),updateProgress)
        minimumvariancepf <- minvarpf(finaldata)
       
####### include the performance plot in Shiny ##################################
        plot.ts(minimumvariancepf)
        title("minimum variance portfolio")
        }
    
###### Equity + longbond overweight bond #######################################

      if (input$rpref2 == 1 && input$inv_horizon > 10) {
        
        finaldata <- datasplit(newData(),updateProgress)
        
        finalpf <- optimpf(finaldata)
        
        longbondindexed <- indexpf(as.data.frame(longbond))
        equitylongbondpf <- equityanddeptpf(finalpf, longbondindexed, 0.2)
        
        plot.ts(as.matrix(equitylongbondpf))
        title("Equity-Longbond Bond overweight")
      }
      
      ######################### risk parity ############################
      if ((input$rpref2 == 4 && input$inv_horizon <= 5) ||
         (input$rpref2 == 5 && input$inv_horizon <= 5) ||
         (input$rpref2 == 4 && input$inv_horizon > 5 && input$inv_horizon <= 10) ||
         (input$rpref2 == 5 && input$inv_horizon > 5 && input$inv_horizon <= 10) ||
         (input$rpref2 == 3 && input$inv_horizon > 10) ||
         (input$rpref2 == 4 && input$inv_horizon > 10)) {
        
        finaldata <- datasplit(newData(),updateProgress)
        
        finalpf <- optimpf(finaldata)
        
        longbonddf <- as.data.frame(longbond)
        commoditydf <- as.data.frame(commodities)
        
        riskparpf <- riskparitypf(finalpf, longbonddf, commoditydf)
        
        plot.ts(riskparpf)
        title("Risk Parity")
      }
      
      ########################### equity + longbond overweight equity ########
      if ((input$rpref2 == 6 && input$inv_horizon <= 5) ||
         (input$rpref2 == 6 && input$inv_horizon > 5 && input$inv_horizon <= 10) ||
         (input$rpref2 == 5 && input$inv_horizon > 10)) {
        
        finaldata <- datasplit(newData(),updateProgress)
        
        finalpf <- optimpf(finaldata)
        
        longbondindexed <- indexpf(as.data.frame(longbond))
        muchequitybondpf <- equityanddeptpf(finalpf, longbondindexed, 0.8)
        
        plot.ts(as.matrix(muchequitybondpf))
        title("Equity-Bond Equity overweight")
      }
      
      ########################## Pure Equity² #########################
      if ((input$rpref2 == 6 && input$inv_horizon > 10) ||
          (input$rpref2 == 7 && input$inv_horizon <= 5) ||
          (input$rpref2 == 7 && input$inv_horizon > 5 && input$inv_horizon <= 10) ||
          (input$rpref2 == 7 && input$inv_horizon > 10)) {
        
        finaldata <- datasplit(newData(),updateProgress)
        
        finalpf <- optimpf(finaldata)
        plot.ts(as.matrix(finalpf))
        title("Pure Equity")
      }
    })
    
### PDF Download Handler - option for the user to downloas her personal report #
    
    output$downloadReport <- downloadHandler(
      
      filename <- function() {
        paste("my-report",
              sep = '.',
              switch(input$format,
                     PDF = "pdf",
                     HTML = "html",
                     Word = "docx")
              )
        },
      
      content <- function(file) {
        src <- normalizePath("report.Rmd")
        
####### temporarily switch to the temp dir, in case you do not have write ######
####### permission to the current working directory ############################
        
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src,
                  "report.Rmd",
                  overwrite = TRUE)
        
        
        out <- render('report.Rmd',
                      switch(input$format,
                             PDF = pdf_document(),
                             HTML = html_document(),
                             Word = word_document())
                      )
        
        file.rename(out, file)
        
        }
    )

    
#### ValueBoxes ################################################################
################################################################################

    output$horizonBox <- renderValueBox({
        valueBox(
            paste0(input$inv_horizon, " years"),
            "Investment Horizon",
            icon = icon("hourglass-half"),
            color = "blue"
        )
    })

    output$returnBox <- renderValueBox({
        valueBox(
            paste0(drift[input$rpref] * 100, "%"),
            "Return",
            icon = icon("chart-line"),
            color = "green"
        )
    })

    output$stdBox <- renderValueBox({
        valueBox(
            paste0(diffusion[input$rpref] * 100, "%"),
            "Standard Deviation",
            icon = icon("square-root-alt"),
            color = "red"
        )
    })

    output$avgBox <- renderValueBox({
        if (sum(sim$data) == 0) {
            valueBox(
                paste0(NA, "$"),
                "Average Value",
                icon = icon("hand-holding-usd"),
                color = "blue"
            )
          
        } else {
            valueBox(
                paste0(round(mean(sim$data)), "$"),
                "Average Value",
                icon = icon("hand-holding-usd"),
                color = "blue"
            )
        }
    })

    output$uplimBox <- renderValueBox({
        if (sum(sim$data) == 0) {
            valueBox(
                paste0(NA, "$"),
                "90% Limit Profit",
                icon = icon("greater-than"),
                color = "green"
            )
          
        } else {
            valueBox(
                paste0(round(sim$data[order(sim$data)[length(sim$data) * 0.9]] -
                               input$initial_wealth), "$"),
                "90% Limit Profit",
                icon = icon("greater-than"),
                color = "green"
            )
        }
    })

    output$lowlimBox <- renderValueBox({
        if (sum(sim$data) == 0) {
            valueBox(
                paste0(NA, "$"),
                "10% Limit Loss",
                icon = icon("less-than"),
                color = "red"
            )
          
        } else {
            valueBox(
                paste0(round(sim$data[order(sim$data)[length(sim$data) * 0.1]] -
                               input$initial_wealth), "$"),
                "10% Limit Loss",
                icon = icon("less-than"),
                color = "red"
            )
        }
    })

    output$horizonBox1 <- renderValueBox({
        valueBox(
            paste0(input$inv_horizon, " years"),
            "Investment Horizon",
            icon = icon("hourglass-half"),
            color = "blue"
        )
    })

    output$returnBox1 <- renderValueBox({
        valueBox(
            paste0(drift[input$rpref2] * 100, "%"),
            "Return",
            icon = icon("chart-line"),
            color = "green"
        )
    })

    output$stdBox1 <- renderValueBox({
        valueBox(
            paste0(diffusion[input$rpref2] * 100, "%"),
            "Standard Deviation",
            icon = icon("square-root-alt"),
            color = "red"
        )
    })

    output$avgBox1 <- renderValueBox({
        valueBox(
            paste0(round(mean(sim$terminal_wealth)), "$"),
            "Average Value",
            icon = icon("hand-holding-usd"),
            color = "blue"
        )
    })

    output$uplimBox1 <- renderValueBox({
        valueBox( ############ the following line is supposed to be too long ;-)
            paste0(round(sim$terminal_wealth[order(sim$terminal_wealth)[draws * 0.9]] -
                           input$initial_wealth), "$"),
            "90% Limit Profit",
            icon = icon("greater-than"),
            color = "green"
        )
    })

    output$lowlimBox1 <- renderValueBox({
        valueBox( ############ the following line is supposed to be too long ;-)
            paste0(round(sim$terminal_wealth[order(sim$terminal_wealth)[draws * 0.1]] -
                           input$initial_wealth), "$"),
            "10% Limit Loss",
            icon = icon("less-than"),
            color = "red"
        )
    })

#### Switch Buttons ############################################################
################################################################################
    
### Switch Tabs with action buttons ############################################
    
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

    observeEvent( # include error message, when all regions are deselected #####
        input$button5, {
          if (is.null(input$mymap_groups)) {
            sendSweetAlert(
              session = session,
              title = "Error Message",
              text = "Please select at least one region!",
              type = "error"
            )
          } else {
            newtab <- switch(input$tabs, "tab3" = "tab4")
            updateTabItems(session, "tabs", newtab)
          }
        }
    )

    observeEvent(
        input$button6, {
            newtab <- switch(input$tabs, "tab4" = "tab3")
            updateTabItems(session, "tabs", newtab)
        }
    )
    
# end of server function #######################################################
################################################################################

}

runApp(shinyApp(ui,server),launch.browser = TRUE)
#runApp(shinyApp(ui,server),launch.browser = TRUE, display.mode = "showcase")
#shinyApp(ui,server)