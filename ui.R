library(shiny)
library(ggplot2)
library('scales')
require(markdown)

shinyUI(navbarPage("Campaign Significance!",
    theme = "bootstrap.css",        # Set theme sourced from https://bootswatch.com/sandstone/
    
# Tab1 - Introduction/Instructions
    tabPanel("Instructions",includeMarkdown("instructions.md")),

# Tab2 - Analysis
    navbarMenu("Analysis",
# Tab2 - Sub1=Using File Upload
        tabPanel("File Upload",
            sidebarLayout(
        # Add a sidebar for user to upload data for analysis 
                sidebarPanel(
                    includeText("introupload.txt"),         # Include explanatory text
                    tags$hr(),                              # Include line break
                    radioButtons('sep', 'Separator',        # Allow user to set separator option for csv upload
                                 c(Comma=',',Semicolon=';',Tab='\t'),','),
                    radioButtons('quote', 'Quote',          # Allow user to set quote option for csv upload
                                 c(None='','Double Quote'='"','Single Quote'="'"),'"'),
                    tags$hr(),                              # Include line break
                    fileInput('file1', 'Choose CSV File',   # Allow user to select csv file to upload
                              accept=c('text/csv', 'text/comma-separated-values,text/plain', '.csv')),
                    width=3),
        
                mainPanel(
        # Add main panel with 3 tabs that show input data, summarise results & plot significance tests 
                    tabsetPanel(
                        tabPanel("Input Data",                                # Summarise data currently used for analysis
                                 br(),tags$label("Input Data Currently Being Used for Analysis"),tableOutput('uploadData'),
                                 br(),tags$label("Classification of Response Data for Analysis"),
                                 textOutput('controlResponseType'),textOutput('testResponseType')),
                        tabPanel("Results Summary",
                                 br(),tags$label("Summary Analysis of Test vs. Control Performance"),tableOutput('responseSummary')),
                        tabPanel("Significance Plots",
                                 br(),tags$label("Estimated Population Response Probability Density Plots"),
                                 plotOutput('distributionPlots',height="600px"))),
                    width=9),
                position="left")),
# Tab2 - Sub2=Using Manual Input
        tabPanel("Scenario Test",
             sidebarLayout(
                 # Add a sidebar for user to specify different volume & response options 
                 sidebarPanel(
                     includeText("intromanual.txt"),               # Include explanatory text
                     br(),
                     numericInput('controlVolume', 'Select Control Volume',  # Allow user to set volume of contacts in control
                                  value=1000,min=100,step=1000),  # Set minimum volume to 100 to ensure normal assumption okay
                     sliderInput('controlResponse', 'Expected Control Response',   # Allow user to set expected control response range
                                  min=0.001,max=0.1,value=c(0.01,0.015)),   # Default set to range of 1% to 1.5%
                     numericInput('testVolume', 'Select Test Volume',  # Allow user to set volume of contacts in test
                                  value=500,min=100,step=1000),  # Set minimum volume to 100 to ensure normal assumption okay
                     sliderInput('testResponse', 'Expected Test Response',   # Allow user to set expected test response range
                                 min=0.001,max=0.1,value=c(0.015,0.02)),   # Default set to range of 1.5% to 2%
                     width=3),
                 
                 mainPanel(
                     # Add main panel with 2 tabs that summarise results & plot significance tests from different scenarios
                     tabsetPanel(
                         tabPanel("Response Scenarios Summary",
                                  br(),tags$label("Summary of Test vs. Control Performance Scenarios"),
                                  tableOutput('scenarioSummary')),
                         tabPanel("Significance Plots",
                                  tags$label("Estimated Population Response Probability Density Plots"),
                                  plotOutput('scenarioPlots',height="700px"))),
                     width=9),
                 position="left")))))