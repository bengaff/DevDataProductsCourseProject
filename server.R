library(shiny)
library(ggplot2)
library('scales')

exampleData <- readRDS("data/exampleData.rds")      # Load example data to use if no data uploaded yet

shinyServer(function(input, output) {

# Set file upload data parameters based on current inputs
    rawData <- reactive({                       # Save raw data either from example or file to upload
                inFile <- input$file1           # Create object to store details of file to upload
                if (is.null(inFile)) exampleData else
                    read.csv(inFile$datapath, header=TRUE, sep=input$sep, quote=input$quote)})

# Upload Tab 1 - Create objects to display rawData to be used for analysis & show whether response is % or volume
    output$uploadData <- renderTable({data.frame(rawData())})
    output$controlResponseType <- renderText({
        rawDataFrame <- data.frame(rawData())
        controlRespType <- ifelse(max(rawDataFrame$Control.Response)<1,"Percentage","Volume")
        paste0("Control Response = ",controlRespType)})
    output$testResponseType <- renderText({
        rawDataFrame <- data.frame(rawData())
        testRespType <- ifelse(max(rawDataFrame$Test.Response)<1,"Percentage","Volume")
        paste0("Test Response = ",testRespType)})

# Upload Tab 2 - Create table to summarise results
    output$responseSummary <- renderTable({
        rawDataFrame <- data.frame(rawData())
        controlpHat <- if (max(rawDataFrame$Control.Response)<1) rawDataFrame$Control.Response else 
                            rawDataFrame$Control.Response/rawDataFrame$Control.Volume
        testpHat <- if (max(rawDataFrame$Test.Response)<1) rawDataFrame$Test.Response else
                            rawDataFrame$Test.Response/rawDataFrame$Test.Volume
        controlVar <- (controlpHat*(1-controlpHat))/rawDataFrame$Control.Volume
        testVar <- (testpHat*(1-testpHat))/rawDataFrame$Test.Volume
        differenceSE <- sqrt(controlVar+testVar)
# Assume normality for creation of 95% confidence interval of difference between test & control reponse
        confidenceIntervalMin <- controlpHat - testpHat - 1.96*differenceSE
        confidenceIntervalMax <- controlpHat - testpHat + 1.96*differenceSE
        testConclusion <- ifelse(confidenceIntervalMin>0,"Control = Best", 
                                ifelse(confidenceIntervalMax<0,"Test = Best","No Significant Difference"))
        responseSummary <- cbind(rawDataFrame[,c(1,2)],percent(controlpHat),rawDataFrame[,4],percent(testpHat),
                         testConclusion)
        colnames(responseSummary) <- c("Grouping","Control Volume","Control Response","Test Volume",
                                       "Test Response","95% Confidence Conclusion")
        responseSummary})

# Upload Tab 3 - Create charts to visualise estimated population distributions and confidence intervals
    output$distributionPlots <- renderPlot({
        rawDataFrame <- data.frame(rawData())
        controlpHat <- if (max(rawDataFrame$Control.Response)<1) rawDataFrame$Control.Response else 
            rawDataFrame$Control.Response/rawDataFrame$Control.Volume
        testpHat <- if (max(rawDataFrame$Test.Response)<1) rawDataFrame$Test.Response else
            rawDataFrame$Test.Response/rawDataFrame$Test.Volume
        controlVar <- (controlpHat*(1-controlpHat))/rawDataFrame$Control.Volume
        testVar <- (testpHat*(1-testpHat))/rawDataFrame$Test.Volume
        # Assume normality for estimation of underlying response population distributions
        rangeMin <- min(controlpHat,testpHat) - (2*max(sqrt(controlVar),sqrt(testVar)))
        rangeMax <- max(controlpHat,testpHat) + (2*max(sqrt(controlVar),sqrt(testVar)))
        plotData <- data.frame("Grouping"=factor(),"x"=numeric(),"hx"=numeric(),"ControlTest"=numeric())
        for(i in 1:nrow(rawDataFrame)) {
            plotData <- rbind(plotData,
                              data.frame("Grouping"=rep(rawDataFrame[i,1],100),
                                         "x"=seq(rangeMin,rangeMax,length=100),
                                         "hx"=dnorm(seq(rangeMin,rangeMax,length=100),controlpHat[i],sqrt(controlVar[i])),
                                         "ControlTest"=rep("Control",100)))
            plotData <- rbind(plotData,
                              data.frame("Grouping"=rep(rawDataFrame[i,1],100),
                                         "x"=seq(rangeMin,rangeMax,length=100),
                                         "hx"=dnorm(seq(rangeMin,rangeMax,length=100),testpHat[i],sqrt(testVar[i])),
                                         "ControlTest"=rep("Test",100)))}
        levels(plotData$Grouping) <- as.character(rawDataFrame[,1])
        g <- ggplot(plotData,aes(x,hx))
        g + geom_polygon(aes(fill=ControlTest),alpha=0.4) + facet_grid(Grouping~.) + 
            labs(x="% Response",y="") + theme(axis.text.y = element_blank(),axis.line.y=element_blank())})

# Scenario Tab 1 - Create table to summarise results from different scenarios
output$scenarioSummary <- renderTable({
    scenarios <- c("Min Control - Min Test","Min Control - Med Test","Min Control - Max Test",
                   "Med Control - Min Test","Med Control - Med Test","Med Control - Max Test",
                   "Max Control - Min Test","Max Control - Med Test","Max Control - Max Test")    
    controlVolume <- rep(input$controlVolume,9)
    testVolume <- rep(input$testVolume,9)
    controlpHat <- c(rep(input$controlResponse[1],3),rep(mean(input$controlResponse),3),rep(input$controlResponse[2],3))
    testpHat <- rep(c(input$testResponse[1],mean(input$testResponse),input$testResponse[2]),3)    
    controlVar <- (controlpHat*(1-controlpHat))/controlVolume
    testVar <- (testpHat*(1-testpHat))/testVolume
    differenceSE <- sqrt(controlVar+testVar)
    # Assume normality for creation of 95% confidence interval of difference between test & control reponse
    confidenceIntervalMin <- controlpHat - testpHat - 1.96*differenceSE
    confidenceIntervalMax <- controlpHat - testpHat + 1.96*differenceSE
    testConclusion <- ifelse(confidenceIntervalMin>0,"Control = Best", 
                             ifelse(confidenceIntervalMax<0,"Test = Best","No Significant Difference"))
    responseSummary <- cbind(scenarios,controlVolume,percent(controlpHat),
                             testVolume,percent(testpHat),testConclusion)
    colnames(responseSummary) <- c("Scenario","Control Volume","Control Response","Test Volume",
                                   "Test Response","95% Confidence Conclusion")
    responseSummary})

# Scenario Tab 2 - Create charts to visualise estimated population distributions for different scenarios
output$scenarioPlots <- renderPlot({
    scenarios <- c("Min Ctrl-Min Test","Min Ctrl-Med Test","Min Ctrl-Max Test","Med Ctrl-Min Test","Med Ctrl-Med Test",
                   "Med Ctrl-Max Test","Max Ctrl-Min Test","Max Ctrl-Med Test","Max Ctrl-Max Test")    
    controlVolume <- rep(input$controlVolume,9)
    testVolume <- rep(input$testVolume,9)
    controlpHat <- c(rep(input$controlResponse[1],3),rep(mean(input$controlResponse),3),rep(input$controlResponse[2],3))
    testpHat <- rep(c(input$testResponse[1],mean(input$testResponse),input$testResponse[2]),3)    
    controlVar <- (controlpHat*(1-controlpHat))/controlVolume
    testVar <- (testpHat*(1-testpHat))/testVolume
    # Assume normality for estimation of underlying response population distributions
    rangeMin <- min(controlpHat,testpHat) - (2*max(sqrt(controlVar),sqrt(testVar)))
    rangeMax <- max(controlpHat,testpHat) + (2*max(sqrt(controlVar),sqrt(testVar)))
    plotData <- data.frame("Scenario"=factor(),"x"=numeric(),"hx"=numeric(),"ControlTest"=numeric())
    
    for(i in 1:9) {
        plotData <- rbind(plotData,
                          data.frame("Scenario"=rep(scenarios[i],100),
                                     "x"=seq(rangeMin,rangeMax,length=100),
                                     "hx"=dnorm(seq(rangeMin,rangeMax,length=100),controlpHat[i],sqrt(controlVar[i])),
                                     "ControlTest"=rep("Control",100)))
        plotData <- rbind(plotData,
                          data.frame("Scenario"=rep(scenarios[i],100),
                                     "x"=seq(rangeMin,rangeMax,length=100),
                                     "hx"=dnorm(seq(rangeMin,rangeMax,length=100),testpHat[i],sqrt(testVar[i])),
                                     "ControlTest"=rep("Test",100)))}
    levels(plotData$Scenario) <- as.character(scenarios)
    g <- ggplot(plotData,aes(x,hx))
    g + geom_polygon(aes(fill=ControlTest),alpha=0.4) + facet_grid(Scenario~.) + 
        labs(x="% Response",y="") + theme(axis.text.y = element_blank(),axis.line.y=element_blank())})

})