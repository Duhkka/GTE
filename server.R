#library(RMySQL)
library(shiny)

shinyServer(function(input, output,session) {
  
  output$ovslrunPlot <- renderPlot({ 
    ACTags=reactive({
#       start_date = as.numeric(input$dateRange[1])
#       end_date = as.numeric(input$dateRange[2])
#       ids = sDates>=start_date & sDates <=end_date
      tw = summary(as.factor(ac_single_data$tags))
      tw[order(-tw),drop=TRUE]
    })
    barplot(t(as.matrix(ACTags())),las=2)  })

# Generate a summary of the data
output$ovslSummary <- renderPrint({
  ACTags=reactive({
    #       start_date = as.numeric(input$dateRange[1])
    #       end_date = as.numeric(input$dateRange[2])
    #       ids = sDates>=start_date & sDates <=end_date
    tw = summary(as.factor(ac_single_data$tags))
    tw[order(-tw),drop=TRUE]
  })
  data.frame(ACTags())
})

})  
