library(shiny)
library(markdown)
library(shinythemes)
library(shinyBS)

shinyUI(fluidPage(
#   bsButton("moTrig", "Open Modal", style = "primary"),
#   bsModal("moMod", "Example Modal", trigger="moTrig",
#           tags$p(HTML(noteText)),
#           tags$div(class = "row-fluid",
#           tags$div(class = "span4 well control-panel",
#                   sliderInput("moobs", "Number of observations:", min = 1, max = 1000, value = 500),
#                   selectInput("modist", "Distribution", choices = c("Normal", "Lognormal", "Uniform", "Exponential")),
#                   textInput("motitle", "Plot Title", "A Modal Plot")
#           )
#           )
#   ),
  list(tags$head(HTML('<link rel="icon", href="images.jpeg", type="image/jpeg" />'))),
  div(header="",align="left",img(src="genia.png"),img(src="RocheSeq.png"),titlePanel(title="", windowTitle="Genia Tag Experiments")),  
  navbarPage(title ="TAG Experiments",collapsible = TRUE,inverse = FALSE,id = "NavTag",
             header = "", footer = "",theme =  shinytheme("cerulean"),
              #themes: cerulean,cosmo,flatly,journal,readable,spacelab,united  
        tabPanel("OVERVIEW",     
        tabsetPanel(tabPanel(width = "100%",height="100%","AC Runs",                      
                             verticalLayout(
                              wellPanel(width = "60%",
                                        fluidRow( 
                                          column(4,dateRangeInput('ac_dateRange',label = 'Select range of dates:',start = ac_earliest, end = ac_latest))
                                        )
                              )
                            ), plotOutput("ovacrunPlot")),
                    tabPanel("AC Summary", dataTableOutput("ovacSummary")),
                    tabPanel("DC Runs",     
                      verticalLayout(
                        wellPanel(width = "60%",
                                  fluidRow( 
                                    column(4,dateRangeInput('dc_dateRange',label = 'Select range of dates:',start = dc_earliest, end = dc_latest))
                                  )
                        )
                      ),plotOutput("ovdcPlot")),
                    tabPanel("DC Summary", dataTableOutput("ovdcSummary"))
               )),
        
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
                    mainPanel(height="100%",plotOutput("exTagDC")))),
                   tabPanel("By Tag (AC)",
                   sidebarLayout(position="right",
                                 sidebarPanel(width=3,
                                   selectInput("actagname", "Select Tag:",choices=ac_single_tags)
                                 ),
                                 mainPanel(height="100%",plotOutput("exTagAC"))))
                   
        ),
        tags$head(tags$script("var f_fnRowCallback = function( nRow, qData, iDisplayIndex,     iDisplayIndexFull ){
                          $('td', nRow).click( function(){Shiny.onInputChange('f_fnRowCallback',     [qData])} );
                              }                                        
                              
                              Shiny.addCustomMessageHandler('fnRowCallback', function(x) { 
                              
                              uiOutput(x)
                              })"))
        )))
        