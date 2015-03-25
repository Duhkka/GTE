library(shiny)
library(markdown)
library(shinythemes)

shinyUI(fluidPage(
  list(tags$head(HTML('<link rel="icon", href="images.jpeg", type="image/jpeg" />'))),
  div(header="",align="left",img(src="genia.png"),titlePanel(title="", windowTitle="Genia Tag Experiments")),  
  navbarPage(title ="TAG Experiments",collapsible = TRUE,inverse = FALSE,id = "NavTag",
             header = "", footer = "",theme =  shinytheme("cerulean"),
              #themes: cerulean,cosmo,flatly,journal,readable,spacelab,united  
        tabPanel("Overview",     
        tabsetPanel(tabPanel("Recent Runs", plotOutput("ovslrunPlot")),
               tabPanel("Active Cells", plotOutput("ovslcellPlot")),
               tabPanel("Summary", verbatimTextOutput("ovslSummary")))),
        
        navbarMenu("AC",
                   tabPanel("Single Tag", 
                            tabsetPanel(type = "tabs",
                                        tabPanel("Single Level", dataTableOutput("acltable")))),
                   tabPanel("Background",
                            tabsetPanel(type = "tabs",
                                        tabPanel("Background", dataTableOutput("acbacktable"))))),
        navbarMenu("DC",
                   tabPanel("Single Tag",
                            tabsetPanel(type = "tabs",
                                        tabPanel("Single Level", dataTableOutput("dcsltable")),
                                        tabPanel("Summary", verbatimTextOutput("dcslsummary")))),
                   tabPanel("Background",
                            tabsetPanel(type = "tabs",
                                        tabPanel("Background", dataTableOutput("dcbacktable")))),
                   tabPanel("Cy Study",
                            tabsetPanel(type = "tabs",
                                        tabPanel("Cy Study", dataTableOutput("dccytable")))),
                   tabPanel("Competition",
                            tabsetPanel(type = "tabs",
                                        tabPanel("Competition", dataTableOutput("dccomptable"))))),
        tabPanel("Plots",
                 navlistPanel(
                   "Header",
                   tabPanel("First",
                            h3("This is the first panel")
                   ),
                   tabPanel("Second",
                            h3("This is the second panel")
                   ),
                   "-----",
                   tabPanel("Third",
                            h3("This is the third panel")
                   )
                 ))
        )))
        