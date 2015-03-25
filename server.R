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

  output$acbacktable <- renderDataTable({
    oxdata=reactive({
      #      start_date = as.numeric(input$dateRange[1])
      #      end_date = as.numeric(input$dateRange[2])
      #      ids = ac_single_Dates>=start_date & ac_single_Dates <=end_date
      #      subset(ac_single_data, ids)
      ac_background_data
    })
    data.frame(oxdata())  
  })

  output$acltable <- renderDataTable({
    osdata=reactive({
#      start_date = as.numeric(input$dateRange[1])
#      end_date = as.numeric(input$dateRange[2])
#      ids = ac_single_Dates>=start_date & ac_single_Dates <=end_date
#      subset(ac_single_data, ids)
       ac_single_data
    })
    data.frame(osdata())  
  })

  output$dcsltable <- renderDataTable({
    osdata=reactive({
#       start_date = as.numeric(input$dateRange[1])
#       end_date = as.numeric(input$dateRange[2])
#       ids = sDates>=start_date & sDates <=end_date
#       subset(sdata, ids)
        dc_single_data
    })
    data.frame(osdata())  
  },
    options = list(lengthMenu = c(10, 20, 50), pageLength = 10,autoWidth = FALSE)
  )

  output$dcbacktable <- renderDataTable({
    osdata=reactive({
      #       start_date = as.numeric(input$dateRange[1])
      #       end_date = as.numeric(input$dateRange[2])
      #       ids = sDates>=start_date & sDates <=end_date
      #       subset(sdata, ids)
      dc_background_data
    })
    data.frame(osdata())  
  },
  options = list(lengthMenu = c(10, 20, 50), pageLength = 10,autoWidth = FALSE)
  )

  output$dccytable <- renderDataTable({
    osdata=reactive({
      #       start_date = as.numeric(input$dateRange[1])
      #       end_date = as.numeric(input$dateRange[2])
      #       ids = sDates>=start_date & sDates <=end_date
      #       subset(sdata, ids)
      dc_cy_data
    })
    data.frame(osdata())  
  },
  options = list(lengthMenu = c(10, 20, 50), pageLength = 10,autoWidth = FALSE)
  )

  output$dccomptable <- renderDataTable({
    osdata=reactive({
      #       start_date = as.numeric(input$dateRange[1])
      #       end_date = as.numeric(input$dateRange[2])
      #       ids = sDates>=start_date & sDates <=end_date
      #       subset(sdata, ids)
      dc_competition_data
    })
    data.frame(osdata())  
  },
  options = list(lengthMenu = c(10, 20, 50), pageLength = 10,autoWidth = FALSE)
  )

  output$dcslsummary <- renderPrint({
    GTags=reactive({
      input$gtag
#       start_date = as.numeric(input$dateRange[1])
#       end_date = as.numeric(input$dateRange[2])
#       ids = dc_single_Dates>=start_date & dc_single_Dates<=end_date
      qids = dc_single$Quality.Run == "Yes"
      qr = table(dc_single_data[,c("TagName")][qids])#[ids])
      qr[order(-qr),drop=TRUE]
      
    })
    data.frame(GTags())
  })

})  
