library(shiny)
library(markdown)
library(shinythemes)

shinyUI(fluidPage(
  list(tags$head(HTML('<link rel="icon", href="images.jpeg", type="image/jpeg" />'))),
  div(header="",align="left",img(src="genia.png"),titlePanel(title="", windowTitle="Genia Tag Experiments")),  
  navbarPage(title ="TAG Experiments",collapsible = TRUE,inverse = FALSE,id = "NavTag",
             header = "", footer = "",theme =  shinytheme("cerulean"),
              #themes: cerulean,cosmo,flatly,journal,readable,spacelab,united  
        tabPanel("OVERVIEW",     
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
        navbarMenu("EXPLORE",
                   tabPanel("By Tag (DC)",
                   sidebarLayout(position="right",
                     sidebarPanel(width=3,
                       selectInput("dctagname", "Select Tag:",choices=dc_single_tags)
                     ),
                        mainPanel(plotOutput("exTagDC")))),
                   tabPanel("By Tag (AC)",
                   sidebarLayout(position="right",
                                 sidebarPanel(width=3,
                                   selectInput("actagname", "Select Tag:",choices=ac_single_tags)
                                 ),
                                 mainPanel(plotOutput("exTagAC"))))
                   
        )
        )))
        