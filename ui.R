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
               tabPanel("Summary", verbatimTextOutput("ovslSummary")))
        
        ))))
# )
# ),
#     navbarMenu("AC",
#            tabPanel("Single Tag",
#                     tabsetPanel(type = "tabs", 
#                                 tabPanel("Runs", plotOutput("runPlot")),
#                                 tabPanel("Mean OC", plotOutput("mocPlot")),
#                                 tabPanel("Summary", verbatimTextOutput("summary")), 
#                                 tabPanel("Single Level ", dataTableOutput("sltable"))
#                                 
#                     )
#            ),
#            tabPanel("Cy3 Effect Study",
#                     tabsetPanel(type = "tabs",
#                                 tabPanel("Summary", verbatimTextOutput("summary")),
#                                 tabPanel("Dye", dataTableOutput("dtable"))),
#                     tabPanel("Plots")
#            ),
#            tabPanel("Competition",
#                     tabsetPanel(type = "tabs",
#                                 tabPanel("Summary", verbatimTextOutput("summary")),
#                                 tabPanel("Competition", dataTableOutput("ctable"))),
#                     tabPanel("Plots")
#            ),
#            tabPanel("Background",
#                     tabsetPanel(type = "tabs",
#                                 tabPanel("Summary", verbatimTextOutput("summary")),
#                                 tabPanel("Background", dataTableOutput("btable"))),
#                     tabPanel("Plots")
#            ),
#            tabPanel("Aggregate Plots", 
#                     dataTableOutput("table"))
#            
# ),
#      navbarMenu("DC",
#                 tabPanel("Single Tag",
#                          tabsetPanel(type = "tabs", 
#                                      tabPanel("Quality Runs", plotOutput("gtagPlot")),
#                                      tabPanel("Mean OC", plotOutput("mocPlot")),
#                                      tabPanel("Summary", verbatimTextOutput("summary")), 
#                                      tabPanel("Single Level ", dataTableOutput("sltable"))
#                                      
#                          )
#                 ),
#                 tabPanel("Cy3 Effect Study",
#                          tabsetPanel(type = "tabs",
#                                      tabPanel("Summary", verbatimTextOutput("summary")),
#                                      tabPanel("Dye", dataTableOutput("dtable"))),
#                                      tabPanel("Plots")
#                 ),
#                 tabPanel("Competition",
#                          tabsetPanel(type = "tabs",
#                                      tabPanel("Summary", verbatimTextOutput("summary")),
#                                      tabPanel("Competition", dataTableOutput("ctable"))),
#                                      tabPanel("Plots")
#                 ),
#                 tabPanel("Background",
#                           tabsetPanel(type = "tabs",
#                                       tabPanel("Summary", verbatimTextOutput("summary")),
#                                       tabPanel("Background", dataTableOutput("btable"))),
#                                       tabPanel("Plots")
#                 ),
#                 tabPanel("Aggregate Plots", 
#                          dataTableOutput("table"))
#                 
#      ),
#      
#      navbarMenu("Explore",
#                 tabPanel("Compare Tag",
#                          tabsetPanel(type = "tabs", 
#                                      tabPanel("AC vs DC", plotOutput("stacdcPlot")),
#                                      tabPanel("Another Tag(s)", plotOutput("mocPlot")),
#                                      tabPanel("Summary", verbatimTextOutput("summary")), 
#                                      tabPanel("Single Level ", dataTableOutput("sltable"))
#                                      
#                          )
#                 ),
#                 tabPanel("Different Tags",
#                          tabsetPanel(type = "tabs",
#                                      tabPanel("Summary", verbatimTextOutput("summary")),
#                                      tabPanel("Dye", dataTableOutput("dtable"))),
#                          tabPanel("Plots")
#                 ),
#                 tabPanel("Competition",
#                          tabsetPanel(type = "tabs",
#                                      tabPanel("Summary", verbatimTextOutput("summary")),
#                                      tabPanel("Competition", dataTableOutput("ctable"))),
#                          tabPanel("Plots")
#                 ),
#                 tabPanel("Background",
#                          tabsetPanel(type = "tabs",
#                                      tabPanel("Summary", verbatimTextOutput("summary")),
#                                      tabPanel("Background", dataTableOutput("btable"))),
#                          tabPanel("Plots")
#                 ),
#                 tabPanel("Aggregate Plots", 
#                          dataTableOutput("table"))
#                 
#      
#       )))
# )