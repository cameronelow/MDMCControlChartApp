# MDMCControlChartApp
For my senior thesis project, I coded a web application in R that would display a control chart analyzing equipment data for the Marine Depot Maintenance Command.

library(qcc)
library(shiny)
library(DT)

mydata <-read.csv("dummydata.csv")
variables <- 1:ncol(mydata)
names(variables) <- names(mydata)
equipment <- mydata[1:nrow(mydata), c("CON")]

ui <- fluidPage(
  titlePanel("Control Charts"),
  sidebarLayout(
    sidebarPanel(
      fileInput("file1", "Choose a file in CSV format", accept = c("text/csv", "text/comma-separated-values,text/plain",
                                                                   ".csv")
      ), #fileInput
      
      hr(),
      
      selectInput(inputId = "equip", label = "Contract Order Number:", choices = equipment), #selectInput
      
      
      selectInput(inputId = "var", label = "Choose which variable you want to graph", choices = variables), #selectInput
      
      
      
      ############################
      
      #Added by A. Coleman 20180225
      
      br(),
      
      actionButton("goButton", "Go!"),
      
      p("Update your variables"),
      
      ############################
      
      
      hr(),
      
      helpText("Created by Cameron Lowry, Senior Thesis, 2018")
      
    ), #sidebar panel
    
    
    
    mainPanel(
      
      plotOutput(outputId = "controlchart"),
      dataTableOutput(outputId = "datachart")
      
    ) #main panel
  ) #sidebar layout
) #user interface




server <- function(input, output, session) {
  
  
  #Load the file
  loadedFile <- reactive({
    file <- input$file1
    file
  }) #loadedFile()
  
  #Update the variables from the file input
  observe({
    
    f <- loadedFile()
    if(is.null(f))
      return(NULL)
    
    temp <- read.csv(f$datapath)
    v <- 1:ncol(temp)
    names(v) <- names(temp)
    if ("CON" %in% colnames(temp))
      e <- temp[1:nrow(temp), c("CON")]
    
    if("MWSLIN" %in% colnames(temp))
      e <-temp[1:nrow(temp), c("MWSLIN")]
    
    updateSelectInput(session, 'equip', choices = e)
    updateSelectInput(session, 'var', choices = v)
    
  }) #observe UpdateSelectInput
  
  #Update the graph when the action button is pressed
  graph <- eventReactive(input$goButton, {
    
    file <-loadedFile()
    
    if(is.null(file)) #Use the default data set
    {
      col <- as.integer(input$var)
      
      d <- mydata[mydata$CON %in% input$equip, col ]
      
      
    }
    else
    {
      d <- read.csv(file$datapath, header = TRUE)
      col <- as.integer(input$var)
      
      if ("CON" %in% colnames(d))
        d <- d[d$CON == input$equip, col]
      if ("MWSLIN" %in% colnames(d))
        d <- d[d$MWSLIN == input$equip, col]
      
    }
    
    #Now, calculate the average moving range for the control limits
    mRange <- c()
    difference <- 0
    mRange[1] <- 0
    for (num in c(2 : length(d)))
    {
      difference <- d[num] - d[num - 1]
      difference = abs(difference)
      mRange[num] <- difference
    }
    
    mRangeSum <- sum(mRange, na.rm = TRUE)
    mRangeLength <- length(mRange)
    
    AverageMovingRange = (mRangeSum) / (mRangeLength)
    mean = mean(d)
    
    UCL <-  mean + 2.66*AverageMovingRange
    LCL <- mean - 2.66*AverageMovingRange
    
    #Set the limits for the y-axis
    maxYValue <- max(d, na.rm = TRUE) + 100
    
    if (UCL > maxYValue)
      maxYValue <- UCL + 100
    
    #Manually set the control limits to be based on the average moving range
    controlLimits <- c(LCL, UCL)
    
    if (LCL < 0)
      controlLimits <- c(0, UCL)
    
    #create the control chart
    spcChart <- qcc(d, type = "xbar.one", limits = controlLimits, ylim = c(0, maxYValue), plot = TRUE ) #qcc
    
    (warn.limits <- limits.xbar(spcChart$center, spcChart$std.dev, spcChart$sizes, 2))
    plot(spcChart, add.stats = TRUE, chart.all = TRUE, label.limits = c("LCL", "UCL"), title = "XmR Chart",xlab = "Increments", ylab = "Values",ylim = c(0, maxYValue), restore.par =  FALSE)
    
    
  })#eventReactive
  
  #Graph the control chart
  output$controlchart <- renderPlot({
    
    graph()
    
  }) #render plot
  
  
  
  #Output the data table
  output$datachart <- DT::renderDataTable({
    
    file <-loadedFile()
    
    if(is.null(file))
    {
      df <-mydata
    }
    else
    {
      df <-read.csv(file$datapath)
    }
    df
  }) #render data table
  
  
} #server function

shinyApp(ui = ui, server = server)
