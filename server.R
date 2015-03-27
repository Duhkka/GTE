library(shiny)
library(ggplot2)

shinyServer(function(input, output,session) {
  output$ovdcPlot <- renderPlot({
    DCTags=reactive({
      input$dtag
      start_date = as.numeric(input$dc_dateRange[1])
      end_date = as.numeric(input$dc_dateRange[2])
      ids = dc_single_Dates>=start_date & dc_single_Dates <=end_date
      qids = dc_single$Quality.Run == "Yes"
      qr = table(dc_single_data[,c("Tag")][ids][qids])
      #    dr = table(sdata[,c("Tag")][ids])
      #    qrd = qr[ids]
      qr[order(-qr),drop=TRUE]
    })
    if (sum(DCTags()) > 0)
    {
      
      barplot(t(
        as.matrix(DCTags())), 
        beside=TRUE, 
        main=paste("Genia Tags in Quality DC Runs: ", sum(DCTags()) , "(from ",input$dc_dateRange[1] ," to ", input$dc_dateRange[2], ")"),
        ylab="Num Quality Runs",xlab="",las=2)
      
    }
  })
  
  output$ovacrunPlot <- renderPlot({ 
    ACTags=reactive({
      start_date = as.numeric(input$ac_dateRange[1])
      end_date = as.numeric(input$ac_dateRange[2])
      ids = ac_single_Dates>=start_date & ac_single_Dates <=end_date
      #tw = summary(as.factor(ac_single_data$tags))
      #tw[order(-tw),drop=TRUE]
      qr = table(ac_single_data[,c("tags")][ids])
      #    dr = table(sdata[,c("Tag")][ids])
      #    qrd = qr[ids]
      qr[order(-qr),drop=TRUE]
      
    })
    if (sum(ACTags()) > 0)
    {
    barplot(t(
      as.matrix(ACTags())), 
      beside=TRUE, 
      main=paste("Genia Tags AC Runs: ", sum(ACTags())," ( from ",input$ac_dateRange[1] ," to ", input$ac_dateRange[2], " )"),
      ylab="Num of Runs",xlab="",las=2)
    }
  })

  # Generate a summary of ac data
  output$ovacSummary <- renderPrint({
#     ACTags=reactive({
#       #       start_date = as.numeric(input$dateRange[1])
#       #       end_date = as.numeric(input$dateRange[2])
#       #       ids = sDates>=start_date & sDates <=end_date
#       tw = summary(as.factor(ac_single_data$tags))
#       tw[order(-tw),drop=TRUE]
#     })
#     if (sum(ACTags()) > 0)
#     {
#     data.frame(ACTags())
#     }
    tags_data_frame
  })

# Generate a summary of dc data
output$ovdcSummary <- renderPrint({
  DCTags=reactive({
  input$dtag
  start_date = as.numeric(input$dc_dateRange[1])
  end_date = as.numeric(input$dc_dateRange[2])
  ids = dc_single_Dates>=start_date & dc_single_Dates <=end_date
  qids = dc_single$Quality.Run == "Yes"
  qr = table(dc_single_data[,c("Tag")][ids][qids])
  qr[order(-qr),drop=TRUE]
    
  })
  if (sum(DCTags()) > 0)
  {
  data.frame(DCTags())
  }
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

  output$exTagDC <- renderPlot({ 
    TagDC=reactive({
    t2ids = dc_single_data$Tag==input$dctagname
    
    qids = dc_single$Quality.Run == "Yes"
    
    qt2dt  = dc_single_data[,c("Mean.DT")][t2ids][qids]
    DT = data.frame(na.omit(qt2dt[qt2dt>0]))
    names(DT)=c("DT")
    
    qt2oc  = dc_single_data[,c("Mean.OC")][t2ids][qids]   
    OC = data.frame(na.omit(qt2oc[qt2oc>0 ]))
    names(OC)=c("OC")
    
    qt2dtsig  = dc_single_data[,c("sigma.DT")][t2ids][qids]
    DTsig = data.frame(na.omit(qt2dtsig[qt2dtsig>0]))
    names(DTsig)=c("sigma_DT")
    
    qt2ocsig  = dc_single_data[,c("sigma.OC")][t2ids][qids]   
    OCsig = data.frame(na.omit(qt2ocsig[qt2ocsig>0 ]))
    names(OCsig)=c("sigma_OC")
    
    p1 = ggplot(OC, aes(x='',y=OC)) + geom_boxplot() + ggtitle("OC for quality runs") #+ ylim(0.1, .3)   
    p2 = ggplot(DT, aes(x='',y=DT)) + geom_boxplot() + ggtitle("DT for quality runs") #+ ylim(0.3, 1.0) 
    p3 = ggplot(OCsig, aes(x='',y=sigma_OC)) + geom_boxplot() + ggtitle("Sigma OC for quality runs")
    p4 = ggplot(DTsig, aes(x='',y=sigma_DT)) + geom_boxplot() + ggtitle("Sigma DT for quality runs")
    multiplot(p1,p2,p3,p4,  cols=2) 

    })
     TagDC() })

output$exTagAC <- renderPlot({ 
  TagAC=reactive({
    t2ids = ac_single_data$tag==input$actagname

    qt2dt  = ac_single_data[,c("Inactive.Cells")][t2ids]
    IC = data.frame(na.omit(qt2dt[qt2dt>0]))
    names(IC)=c("IC")
    
    qt2oc  = ac_single_data[,c("Single.Pore.Cells")][t2ids]  
    SPC = data.frame(na.omit(qt2oc[qt2oc>0 ]))
    names(SPC)=c("SPC")
    
    qt2dtsig  = ac_single_data[,c("Inactive.Cell.Reps")][t2ids]
    ICR = data.frame(na.omit(qt2dtsig[qt2dtsig>0]))
    names(ICR)=c("ICR")
    
    qt2ocsig  = ac_single_data[,c("Active.Reps")][t2ids]
    AR = data.frame(na.omit(qt2ocsig[qt2ocsig>0 ]))
    names(AR)=c("AR")
    
    qt2ocsig  = ac_single_data[,c("Single.Pore.Reps")][t2ids]
    SPR = data.frame(na.omit(qt2ocsig[qt2ocsig>0 ]))
    names(SPR)=c("SPR")
    
    p1 = ggplot(IC, aes(x='',y=IC)) + geom_boxplot() + ggtitle("# Inactive cells") #+ ylim(0.1, .3)   
    p2 = ggplot(SPC, aes(x='',y=SPC)) + geom_boxplot() + ggtitle("# Single Pore Cells") #+ ylim(0.3, 1.0) 
    p3 = ggplot(ICR, aes(x='',y=ICR)) + geom_boxplot() + ggtitle("Inactive Cell Reps")
    p4 = ggplot(AR, aes(x='',y=AR)) + geom_boxplot() + ggtitle("Active Reps")
    p5 = ggplot(SPR, aes(x='',y=SPR)) + geom_boxplot() + ggtitle("Single Pore Reps")
    multiplot(p1,p2,p3,p4,  cols=2) 
    
  })
  TagAC() })

})  
