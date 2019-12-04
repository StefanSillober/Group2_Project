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
library(markdown)


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
                           knobInput(
                               inputId = "inv_horizon",
                               label = "Years:",
                               value = 10,
                               min = 1,
                               max = 30,
                               displayPrevious = TRUE, 
                               lineCap = "round",
                               fgColor = "#428BCA",
                               inputColor = "#428BCA"
                           )
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
                                         "Slow", "Moderate", "Fast", "Very Fast"),
                             selected = "Moderate"
                           ),
                           tags$p("Press the", tags$em("Start"),
                                  "button in order to initiate the simulation.", br(),
                                  "Press", tags$em("Next Draw"), "in order to see one possible realization.", br(),
                                  "Use the", tags$em("Play"), "and", tags$em("Stop"), "button",
                                  "in order run the simulation automatically.", br(),
                                  "Finally use", tags$em("Reset"), "in order to start anew."),
                       ),
                       
                       actionBttn(
                           inputId = "button1",
                           label = "Next",
                           style = "unite", 
                           color = "success"
                       )
                       

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
                    title = tagList(shiny::icon("map-pin"), "Geographical Preferences"), width = 8,
                    id = "tabset1", height = "550",
                    tabPanel(title = tagList(shiny::icon("globe-americas"), "Map"), leafletOutput("mymap", height = "500")),
                    tabPanel(title = tagList(shiny::icon("info"), "Details"), "Explanations about the map and how to use it ....")
                ),
                
                tabBox(
                    title = tagList(shiny::icon("industry"), "Industry Preferences"), width = 4,
                    id = "tabset1", height = "550",
                    tabPanel(title = tagList(shiny::icon("clipboard-check"), "Sectors"),
                            multiInput(
                                 inputId = "industry1",
                                 label = "Industries", 
                                 choices = c(
                                     "Oil & Gas" = "energy",
                                     "Financials" = "financials",
                                     "Healthcare" = "health",
                                     "Utilities" = "utilities",
                                     "Automobiles" = "auto",
                                     "Basic Resources" = "resources",
                                     "Chemicals" = "chemicals",
                                     "Construction" = "construction",
                                     "Banks" = "banks",
                                     "Food & Beverage" = "food",
                                     "Industrial Goods" = "industrial",
                                     "Insurance" = "insurance",
                                     "Media" = "media",
                                     "Personal Goods" = "personal",
                                     "Real Estate" = "real",
                                     "Retail" = "retail",
                                     "Technology" = "tech",
                                     "Telecommunications" = "telecom",
                                     "Travel & Leisure" = "travel"
                                 ),
                                 width = "100%",
                                 options = list(
                                     selected_header = "I don't want to invest in:",
                                     non_selected_header = "Industries"
                                 )
                                 
                             )
                             
                             ),
                    tabPanel(title = tagList(shiny::icon("info"), "Details"), "Explanations about the map and how to use it ....")
                ),


                #leafletOutput("mymap"),
                hr(style="border-color: grey;"),

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
                        color = "success"
                    )),
                tableOutput("table")

        ), # end of third tab item


        # Fourth Tab: Industry Preferences
        tabItem(tabName = "tab4", h2("Portfolio Construction"), # tab item header
                fluidRow(
                  radioButtons('format', 'Document format', c('PDF', 'HTML', 'Word'),
                               inline = TRUE),
                  downloadButton('downloadReport'),
                    actionBttn(
                        inputId = "button6",
                        label = "Back",
                        style = "unite", 
                        color = "danger"
                    ),
                    actionBttn(
                        inputId = "button7",
                        label = "Finish",
                        style = "unite", 
                        color = "royal"
                    ))

        ), # end of fourth tab item


        # Fifth Tab: Portfolio Construction
        tabItem(tabName = "tab5", h2("Final Result: PDF Output"), # tab item header
                fluidRow(
                    actionBttn(
                        inputId = "button8",
                        label = "Back",
                        style = "unite", 
                        color = "danger"
                    ))

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
    sim$terminal_wealth <- c()
    diffusion <- c(0.01, 0.02, 0.05, 0.1, 0.15, 0.25, 0.3)
    drift <- c(0.001,0.005,0.015,0.02, 0.03, 0.06, 0.1)
    draws <- 1000
    speed <- seq(1000, 100, length.out = 5)

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
        session1$timer<-reactiveTimer(speed[which(c("Very Slow","Slow", "Moderate", "Fast", "Very Fast") == input$speed)]) # 100
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
        sim$terminal_wealth <- input$initial_wealth * exp((drift[input$rpref2]-(1/2)*(diffusion[input$rpref2])^2)*input$inv_horizon + diffusion[input$rpref2]*sqrt(input$inv_horizon)*rnorm(1:draws))

        hist(sim$terminal_wealth[sim$terminal_wealth >= 0 & sim$terminal_wealth < input$initial_wealth*5],
             breaks = seq(from = 0, to = (input$initial_wealth*5), by = (input$initial_wealth*5)/30),
             xlim = c(0,input$initial_wealth*5),
             xlab = "Terminal Wealth", main = "Potential Evolvement of Wealth")
        grid()
        #abline(v = input$initial_wealth, col = "blue", lwd = 2)
        points(x = input$initial_wealth, y = 0, pch = 24, bg = "grey", cex = 2)
        abline(v = mean(sim$terminal_wealth), col = "blue", lwd = 2, lty = 2)

        abline(v = sim$terminal_wealth[order(sim$terminal_wealth)[draws*0.9]], col = "green", lwd = 2, lty = 2)
        abline(v = sim$terminal_wealth[order(sim$terminal_wealth)[draws*0.1]], col = "red", lwd = 2, lty = 2)

        legend("topright", legend = c("90 out of 100 boundary", "10 out of 100 boundary", "Average Terminal Wealth", "Initial Investment"),
               col=c("green", "red", "blue", "grey"), lty = c(2, 2, 2, NA), pch = c(NA, NA, NA, 24), box.lty=0, cex = 1.2)
    })
    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
                    ##---Country and Industry Subsetting---##
    
    output$table <- renderTable({
    load("mydf.RData")
        
        if (!("NorthAmerica" %in% input$mymap_groups)) {
            mydf <- mydf[ , -which(grep("SP", names(mydf), value = TRUE) %in% names(mydf))]
        }
        
        if (!("Europe" %in% input$mymap_groups)) {
            mydf <- mydf[ , -which(grep("STOXX", names(mydf), value = TRUE) %in% names(mydf))]
        }
        
      
        if ("energy" %in% input$industry1) {
            mydf <- mydf[ , -which(grep("Energy", names(mydf), value = TRUE) %in% names(mydf))]
        }

        if ("health" %in% input$industry1) {
            mydf <- mydf[ , -which(grep("Health", names(mydf), value = TRUE) %in% names(mydf))]
        }

        if ("utilities" %in% input$industry1) {
            mydf <- mydf[ , -which(grep("Utilities", names(mydf), value = TRUE) %in% names(mydf))]
        }

        if ("financials" %in% input$industry1) {
            mydf <- mydf[ , -which(grep("Financial", names(mydf), value = TRUE) %in% names(mydf))]
        }

        mydf
    })
    


    ###---###---###---###---###---###---###---###---###---###---###---###---###
                            ##---Map---##
    
    # ------------------------ 
    # Read multiple shape files with standardized names
    # all available countries are grouped by continent
    
    # RV <- reactiveValues(Clicks = list())
    
    region <- c("africa", "antarctica", "asia", "europe", "northamerica", "oceania", "southamerica")
    groups <- c("Africa", "Antarctica", "Asia", "Europe", "NorthAmerica", "Oceania", "SouthAmerica")
    colors <- c("red", "blue", "green", "yellow", "purple", "turquoise", "grey")
    
    for (i in region) {
      filestest.i <- geojson_read(as.character(paste(getwd(), "Map", paste(i, "geo.json", sep = "."), sep = "/")), what = "sp")
      assign(as.character(paste("files", i, sep = ".")), filestest.i)
    }
    
    rm(filestest.i)
    
    # ------------------------
    # initiate the map built with leaflet
    
    foundmap <- leaflet() %>%
        
        setView(lng = 0, lat = 30, zoom = 2) %>%
        
        addProviderTiles(providers$Stamen.TonerLite,
                         options = providerTileOptions(noWrap = TRUE))
    #---------------------------------
    # add multiple the several layers to combine the single polygons 
    
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
                        # highlight = highlightOptions(
                        #         weight = 1,
                        #         color = "#000000",
                        #         dashArray = "3",
                        #         fillOpacity = 0.8,
                        #         bringToFront = TRUE),
                        
                        label = paste(groups[reg.N]),
                        group = paste(groups[reg.N])
            ) 
    }
    
    #---------------------------------
    # set up layer controls
    
    foundmap <- foundmap %>%
        addLayersControl(overlayGroups = groups,
                         options = layersControlOptions(collapsed = FALSE))
    
    # ------------------------
    # integrate the map into shiny
    
    output$mymap <- renderLeaflet({foundmap})
    
    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
    ##---Portfolioevaluation functions
    
    
    # Average return - takes one column as an input
    averagereturn <- function(backtest){
        portfoliologreturns <- data.frame()
        for(r in 1:(nrow(backtest)-1)){
            portfoliologreturns[r,1] <- log(backtest[r+1,1]/backtest[r,1])
        }
        averagereturn <- mean(portfoliologreturns[,1])*252
        return(averagereturn)
    }
    
    # Calculate max drawdown - takes one column as an input
    maxdrawdown <- function(backtest){
        trailingmaxdrawdown = data.frame()
        for(r in 1:(nrow(backtest)-1)){
            trailingmaxdrawdown[r,1] <- min(tail(backtest[,1],-r))/backtest[r,]-1
        }
        maxdrawdown <- min(trailingmaxdrawdown[,1])
        return(maxdrawdown)
    }
    
    # Calculate annual standard deviation
    yearlystd <- function(backtest){
        portfoliologreturns <- data.frame()
        for(r in 1:(nrow(backtest)-1)){
            portfoliologreturns[r,1] <- log(backtest[r+1,1]/backtest[r,1])
        }
        yearlystd <- sd(portfoliologreturns[,1])*sqrt(252)
        return(yearlystd)
    }
    
    # Calculate sharpe Ratio for maximization
    sharpe <- function(expectedreturn, covmatrix, par){
        
        par <- as.matrix(par)
        par <- rbind(par, 1-sum(par[,1]))
        
        #Calculate Portfolioreturn
        pfreturn <- t(expectedreturn) %*% as.matrix(par)
        #Calculate Portfoliostandardeviation
        pfvar <- t(as.matrix(par)) %*% covmatrix %*% as.matrix(par)
        #Calculate sharpe Ratio
        sharperatio <- pfreturn/(pfvar^0.5)
        
        return(sharperatio)
    }
    
    
    ###---###---###---###---###---###---###---###---###---###---###---###---###
    ##---Portfolio Creation---###
    
    
    #read in static data
    staticdata <- read.csv("histstock.csv", header = TRUE, sep = ";",dec = ",")
    rownames(staticdata) <- staticdata[,1] 
    staticdata <-  staticdata[,-1]

    
    ### Sharperatio optimized pure Equity Portfolio
    # Takes a dataframe with all indices as input
    optimpf <- function(data){
        #get past returns
        returns <- data.frame()
        for(c in 1:ncol(data)){
            for(r in 1:nrow(data)-1){
                returns[r,c] <- (data[r+1,c]-data[r,c])/data[r,c]
            }
        }
        #startingweights for optimisation -1 so that weights add up to 1
        startingweights <- as.matrix(rep(1/ncol(data),length.out=(ncol(data)-1)))
        expectedreturn <- as.matrix(rep(0,length.out=ncol(data)))
        #return and covariances as matrixes
        returnmatrix <- as.matrix(returns[,])
        covmatrix <- cov(returnmatrix)
        #expected returns for every stock
        for(c in 1:ncol(returns)){
            expectedreturn[c,1] <- mean(returns[,c])
        }
        #Find Optimal portfolioweights given the lower bound of 0 and the shareratio function defined above
        optweights <- optim(par = as.vector(startingweights), fn = sharpe, 
                            expectedreturn = expectedreturn, covmatrix = covmatrix , 
                            control=list(fnscale=-1),lower = 0, method = "L-BFGS-B")
        optweights <- as.matrix(optweights$par)
        optweights <- rbind(optweights, 1-sum(optweights))
        
        #Indexed Portfolio
        indexportfolio <- data.frame()
        for(c in 1:ncol(data)){
            indexportfolio[1,c] <- 100
        }
        for(c in 1:ncol(data)){
            for(r in 1:nrow(returns)){
                indexportfolio[r+1,c] <- indexportfolio[r,c]*(1+returns[r,c])
            }
        }
        
        colnames(indexportfolio) <- NULL
        rownames(indexportfolio) <- NULL
        
        #Form Portfolio with the sharperatio optimal weights
        portfolio <- data.frame()
        for(r in 1:nrow(indexportfolio)){
            portfolio[r,1] <- as.matrix(indexportfolio[r,]) %*% optweights
        }
        
        
        #Returns a Portfolio Indexed to 100
        return(portfolio)
    }
    
    
    ### Equity + Dept Portfolio
    
   
     equityanddeptpf <- function(equity, dept, equityaspercent){
      
      
      bondindex <- data.frame(matrix(NA,ncol = 2,nrow = nrow(equity)))
      bondindex[,1] <- dept
      bondindex[,2] <- dept
      dept <- optimpf(bondindex)
      portfolio <- data.frame()
      
      for(r in 1:nrow(equity)){
        
        portfolio[r,1] <- equity[r,1]*equityaspercent + dept[r,1]*(1-equityaspercent)
        
      }
      
      
      
      return(portfolio)
      
      
      
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

     
     
    ### Risk Parity Portfolio
     
    riskparity <- function(covmatrix, par){
      
      par <- as.matrix(par)
      par <- rbind(par, 1-sum(par[,1]))
      startingweights <- par
      
      marginalrisk <- covmatrix%*%startingweights
      contribution <- as.matrix(as.numeric((startingweights*marginalrisk)))/sqrt(as.numeric((t(startingweights)%*%covmatrix%*%startingweights)))
      portfoliorisk <- sqrt(as.numeric((t(startingweights)%*%covmatrix%*%startingweights)))
      
      
      contributionpercent <- matrix(ncol = 1, nrow = 3)
      for(r in 1:nrow(contribution)){
        contributionpercent[r,1] <- contribution[r,1]/sum(contribution)
      }
      
      
      
      if(par[1,1]+par[2,1]>1){
        portfoliorisk <- 3
      }

        
        portfoliorisk <- abs((contributionpercent[1,1]-contributionpercent[2,1]))+
                         abs((contributionpercent[1,1]-contributionpercent[3,1]))
      
      
      return(portfoliorisk)
      
    }
    
    riskparitypf <- function(equity, dept, commodity){
      
      data <- as.matrix(cbind(equity,dept,commodity))
      
      startingweights <- as.matrix(rep(1/ncol(data),length.out=(ncol(data)-1)))
      
      returns <- data.frame()
      for(c in 1:ncol(data)){
        for(r in 1:nrow(data)-1){
          returns[r,c] <- (data[r+1,c]-data[r,c])/data[r,c]
        }
      }
      
      returnmatrix <- as.matrix(returns[,])
      covmatrix <- cov(returnmatrix)
      
      
      
      optweights <- optim(par = as.vector(startingweights), fn = riskparity, 
                          covmatrix = covmatrix,lower = 0, method = "L-BFGS-B")
      
      optweights <- as.matrix(optweights$par)
      optweights <- rbind(optweights,1-sum(optweights))
      
      portfolio <- data.frame(100)
      
      for(r in 1:nrow(returns)){
        portfolio[(1+r),1] <- portfolio[r,1]*(1+(returns[r,1]*optweights[1,1]+
                                              returns[r,2]*optweights[2,1]+
                                              returns[r,3]*optweights[3,1]))
        }
    
      return(portfolio)

    }
    
  
    ### MinVar Portfolio
    minvar <- function(covmatrix,par){
      
      
      par <- as.matrix(par)
      par <- rbind(par, 1-sum(par[,1]))
      
      std <- t(par)%*%covmatrix%*%par
      
      if(par[1,1]+par[2,1]>1){
        std <- 3
      }
      
      return(std)
    }
    
    
    
    
    minvarpf <- function(data){
      
      returns <- data.frame()
      for(c in 1:ncol(data)){
        for(r in 1:nrow(data)-1){
          returns[r,c] <- (data[r+1,c]-data[r,c])/data[r,c]
        }
      }
      
      returnmatrix <- as.matrix(returns[,])
      covmatrix <- as.matrix(cov(returnmatrix))
      
      startingweights <- as.matrix(rep(1/ncol(data),length.out=(ncol(data)-1)))
      
      optweights <- optim(par = as.vector(startingweights), fn = minvar, 
                          covmatrix = covmatrix,lower = 0, method = "L-BFGS-B")
      
      optweights <- as.matrix(optweights$par)
      optweights <- rbind(optweights,1-sum(optweights))
      
      
      portfolio <- data.frame(100)
      for(r in 1:nrow(returns)){
        portfolio[(1+r),1] <- portfolio[r,1]*(1+(as.matrix(returns[r,])%*%optweights))
      }
      
      
      return(portfolio)
      
    }



    # PDF Download Handler
    output$downloadReport <- downloadHandler(
      filename = function() {
        paste('my-report', sep = '.', switch(
          input$format, PDF = 'pdf', HTML = 'html', Word = 'docx'
        ))
      },
      
      content = function(file) {
        src <- normalizePath('report.Rmd')
        
        # temporarily switch to the temp dir, in case you do not have write
        # permission to the current working directory
        owd <- setwd(tempdir())
        on.exit(setwd(owd))
        file.copy(src, 'report.Rmd', overwrite = TRUE)
        
        library(rmarkdown)
        out <- render('report.Rmd', switch(
          input$format,
          PDF = pdf_document(), HTML = html_document(), Word = word_document()
        ))
        file.rename(out, file)
      }
    )

    

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
        if (sum(sim$data) == 0) {
            valueBox(
                paste0(NA, "$"), "Average Value", icon = icon("hand-holding-usd"),
                color = "blue"
            )
        } else {
            valueBox(
                paste0(round(mean(sim$data)), "$"), "Average Value", icon = icon("hand-holding-usd"),
                color = "blue"
            )
        }
    })

    output$uplimBox <- renderValueBox({
        if (sum(sim$data) == 0) {
            valueBox(
                paste0(NA, "$"), "90% Limit Profit", icon = icon("greater-than"),
                color = "green"
            )
        } else {
            valueBox(
                paste0(round(sim$data[order(sim$data)[length(sim$data)*0.9]] - input$initial_wealth), "$"), "90% Limit Profit", icon = icon("greater-than"),
                color = "green"
            )
        }
    })

    output$lowlimBox <- renderValueBox({
        if (sum(sim$data) == 0) {
            valueBox(
                paste0(NA, "$"), "10% Limit Loss", icon = icon("less-than"),
                color = "red"
            )
        } else {
            valueBox(
                paste0(round(sim$data[order(sim$data)[length(sim$data)*0.1]] - input$initial_wealth), "$"), "10% Limit Loss", icon = icon("less-than"),
                color = "red"
            )
        }
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
            paste0(round(mean(sim$terminal_wealth)), "$"), "Average Value", icon = icon("hand-holding-usd"),
            color = "blue"
        )
    })

    output$uplimBox1 <- renderValueBox({
        valueBox(
            paste0(round(sim$terminal_wealth[order(sim$terminal_wealth)[draws*0.9]] - input$initial_wealth), "$"), "90% Limit Profit", icon = icon("greater-than"),
            color = "green"
        )
    })

    output$lowlimBox1 <- renderValueBox({
        valueBox(
            paste0(round(sim$terminal_wealth[order(sim$terminal_wealth)[draws*0.1]] - input$initial_wealth), "$"), "10% Limit Loss", icon = icon("less-than"),
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


