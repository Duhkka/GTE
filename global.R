setwd("~/Projects/R/GTE")
options(stringsAsFactors=F)

dc_single               = read.csv("SingleLevel.csv",as.is=T)
dc_background           = read.csv("BACK CSV.csv", as.is=T)
dc_cy                   = read.csv("CY CSV.csv", as.is=T)
dc_competition          = read.csv("COMP CSV.csv", as.is=T)

dc_single_colnames      = names(dc_single)
dc_background_colnames  = names(dc_background)
dc_cy_colnames          = names(dc_cy)
dc_competition_colnames = names(dc_competition)

dc_single_Dates         = as.Date(as.character(dc_single$Date), "%y%m%d")
dc_single_dc_date_list     = structure(dc_single_Dates, Class="Date")
dc_single_earliest      = min(dc_single_dc_date_list, na.rm=TRUE)
dc_single_latest        = max(dc_single_dc_date_list, na.rm=TRUE)
dc_single_tags          = dc_single[!duplicated(dc_single$Tag.Name),c("Tag.Name")]
dc_single_data = data.frame(format(dc_single_Dates),dc_single[c("Tag.Name","Full.Name","Station","ChipNum","Status","Num.cells.clean","Num.cells.quality.sequencing","Mean.capturing.level.OC","Capturing.Level.Standard.Deviation","Mean.Dwell.time","Quality.Run","Dwell.time.std")])
names(dc_single_data) = c("Date","Tag","TagName","Station","Chip","Status","Clean","Seq","Mean.OC","sigma.OC","Mean.DT","Quality.Run","sigma.DT")
dc_date_list = structure(dc_single_Dates,Class="Date")
dc_earliest = min(dc_date_list,na.rm=TRUE)
dc_latest   = max(dc_date_list,na.rm=TRUE)
dc_runs=dim(dc_single_data )
dc_total_runs=dc_runs[1]

dc_single_quality_runs = dc_single[dc_single$Quality.Run == "Yes", c("Tag.Name","Full.Name","Num.cells.capturing","Num.cells.quality.sequencing","Mean.capturing.level.OC","Capturing.Level.Standard.Deviation")]

tag_name <- list()
full_tag_name <- list()
mean_num_cell_cap <- list()
mean_num_cell_qual_seq <- list()
mean_capturing_oc <- list()
mean_capturing_sd <- list()

for(name in unique(dc_single_quality_runs$Tag.Name))
{
  tag_name <- c(tag_name, name)
  full_tag_name <- c(full_tag_name, dc_single_quality_runs[dc_single_quality_runs$Tag.Name == name, c("Full.Name")][1])
  
  num_cell_cap <- dc_single_quality_runs[dc_single_quality_runs$Tag.Name == name, c("Num.cells.capturing")]
  cap_count = sum(num_cell_cap)/length(num_cell_cap)
  mean_num_cell_cap <- c(mean_num_cell_cap, cap_count)
  
  num_cell_qual_seq <- dc_single_quality_runs[dc_single_quality_runs$Tag.Name == name, c("Num.cells.quality.sequencing")]
  qual_seq_count = sum(num_cell_qual_seq)/length(num_cell_qual_seq)
  mean_num_cell_qual_seq <- c(mean_num_cell_qual_seq, qual_seq_count)
  
  mean_cap_oc <- dc_single_quality_runs[dc_single_quality_runs$Tag.Name == name, c("Mean.capturing.level.OC")]
  mean_cap_oc_count = sum(mean_cap_oc)/length(mean_cap_oc)
  mean_capturing_oc <- c(mean_capturing_oc, mean_cap_oc_count)
  
  cap_lev_sd <- dc_single_quality_runs[dc_single_quality_runs$Tag.Name == name, c("Capturing.Level.Standard.Deviation")]
  cap_lev_sd_count = sum(as.numeric(cap_lev_sd))/length(cap_lev_sd)
  mean_capturing_sd <- c(mean_capturing_sd, cap_lev_sd_count)  
}
dc_tags_data = cbind( tag_name, full_tag_name, round(as.numeric(mean_num_cell_cap,0)), round(as.numeric(mean_num_cell_qual_seq,0)), round(as.numeric(mean_capturing_oc,2)), round(as.numeric(mean_capturing_sd),3))
dc_tags_data_frame <- data.frame(dc_tags_data)
names(dc_tags_data_frame) = c("Tag","Full Name","Mean Cells Capturing","Mean Quality Sequencing Cells","Mean Capturing OC","Mean OC StDev")

dc_tags_data_frame = as.data.frame(lapply(dc_tags_data_frame,unlist))

dc_background_Dates = as.Date(as.character(dc_background$Date), "%y%m%d")
dc_background_data = data.frame(format(dc_background_Dates),dc_background[c("Tag.Name","Station","ChipNum","Status","Num.cells.clean","Num.cells.quality.sequencing","Mean.capturing.level.Percent.OC.","Capturing.Level.Standard.Deviation","Mean.Dwell.time","Dwell.time.std")])
names(dc_background_data) = c("Date","Tag","Station","ChipNum","Status", "Clean Cells", "Quality Cells", "Mean Cap Percent OC", "Capturing StdDev", "Mean Dwell Time", "Dwell Time StdDev")
dc_cy_Dates = as.Date(as.character(dc_cy$Date),"%y%m%d")
dc_cy_data = data.frame(format(dc_cy_Dates),dc_cy[c("Tag.Name","Station","ChipNum","Status")])

dc_competition_Dates = as.Date(as.character(dc_competition$Date),"%y%m%d")
dc_competition_data = data.frame(format(dc_competition_Dates), dc_competition[c("Tag.Name","Station","ChipNum","Status")])

ac_single               = read.csv("single_tag_ac.csv",as.is=T)
ac_background           = read.csv("ac_background.csv", as.is=T)

ac_single_colnames      = names(ac_single)
ac_background_colnames  = names(ac_background)


ac_single_Dates         = as.Date(as.character(ac_single$expDate),"%y%m%d")
ac_single_dc_date_list     = structure(ac_single_Dates,Class="Date")
ac_single_earliest      = min(ac_single_dc_date_list,na.rm=TRUE)
ac_single_latest        = max(ac_single_dc_date_list,na.rm=TRUE)
ac_single_tags          = ac_single[!duplicated(ac_single$tags),c("tags")]

ac_single_data          = data.frame(format(ac_single_Dates),ac_single[c("tags","stationID","chipNum","Single.Pore.Cells","Single.Pore.Reps","Active.Cells","Active.Reps","Inactive.Cells","Inactive.Cell.Reps")])
cap_pore_list           = list()
cap_pore_list[["Percent.Active.Pores"]] = ac_single$Active.Cells / ac_single$Single.Pore.Cells


rapply( cap_pore_list, f=function(x) ifelse(is.nan(x),0,x), how="replace" )

ac_single_data          = cbind(ac_single_data, cap_pore_list)
names(ac_single_data)   = c("Date","Tag","Station","chipNum","Single Pores","Single Reps","Active Pores","Active Reps","Inactive Cells","Inactive Reps", "Percent Active Pores")


ac_dc_date_list = structure(ac_single_Dates,Class="Date")
ac_earliest = min(ac_dc_date_list,na.rm=TRUE)
ac_latest   = max(ac_dc_date_list,na.rm=TRUE)
ac_runs=dim(ac_single_data )
ac_total_runs=ac_runs[1]

TAG <- list()
SPC <- list()
SPR <- list()
IC  <- list()
ICR <- list()
AC  <- list()
ACR <- list()
for(name in unique(ac_single$tags))
{
  TAG <- c(TAG, name)
  print(name)
  SPC <-c(SPC, sum(ac_single[ac_single$tags == name, c("Single.Pore.Cells")]))
  print(SPC)
  SPR <- c(SPR, sum(ac_single[ac_single$tags == name, c("Single.Pore.Reps")]))
  IC <- c(IC, sum(ac_single[ac_single$tags == name, c("Inactive.Cells")]))
  ICR <- c(ICR, sum(ac_single[ac_single$tags == name, c("Inactive.Cell.Reps")]))
  AC <- c(AC, sum(ac_single[ac_single$tags == name, c("Active.Cells")]))
  ACR <- c(ACR, sum(ac_single[ac_single$tags == name, c("Active.Reps")]))
}
tags_data = cbind( TAG, SPC, SPR, IC, ICR, AC, ACR)
ac_tags_data_frame <- data.frame(tags_data)

names(ac_tags_data_frame) = c("Tag","Single Pore Cells","Single Pore Reps","Inactive Cells","Inactive Cell Reps","Active Cells","Active Reps")
#tags_data_frame[,-1] <-round(tags_data_frame[,-1],0)
ac_tags_data_frame = as.data.frame(lapply(ac_tags_data_frame,unlist))


ac_background_Dates     = as.Date(as.character(ac_background$expDate), "%y%m%d")
ac_background_dc_date_list = structure(ac_background_Dates, Class="Date")
ac_background_earliest  = min(ac_background_dc_date_list, na.rm=TRUE)
ac_background_latest    = max(ac_background_dc_date_list, na.rm=TRUE)
ac_background_tags      = ac_background[!duplicated(ac_background$tags),c("tags")]
ac_background_data      = data.frame(format(ac_background_Dates),ac_background[c("tags","stationID","chipNum","Single.Pore.Cells","Single.Pore.Reps","Active.Cells","Active.Reps","Inactive.Cells","Inactive.Cell.Reps")])
#names(ac_background_data)   = c("Date","Tag","Station","chipNum","Single Pores","Single Reps","Active Pores","Active Reps","Inactive Cells","Inactive Reps", "Percent Active Pores")
multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
# HemoDates = as.Date(as.character(ac_single$Hemo.Complex.Prep),"%y%m%d")
# dc_date_list = structure(sDates,Class="Date")
# earliest = min(dc_date_list,na.rm=TRUE)
# latest   = max(dc_date_list,na.rm=TRUE)
# sdata = data.frame(format(sDates),ac_single[c("Tag.Name","Full.Name","Station","ChipNum","Status","Num.cells.clean","Num.cells.quality.sequencing","Mean.capturing.level.OC","Capturing.Level.Standard.Deviation","Mean.Dwell.time","Quality.Run")])
# names(sdata) = c("Date","Tag","TagName","Station","Chip","Status","Clean","Seq","Mean.OC","sigma.OC","Mean.DT","Quality.Run")
# 
# #Aggregation for each Tag
# aggtags_mean = aggregate(sdata[c("Mean.OC")],list(Tag=sdata$Tag,TagName=sdata$TagName),mean,na.rm=T)
# aggtags_median = aggregate(sdata[c("Mean.OC")],list(Tag=sdata$Tag,TagName=sdata$TagName),median,na.rm=T)
# # another way
# t2ids = sdata$Tag=="T-00002"
# sdata$Tag[t2ids] # has many zeros (not quality runs)
# mean(sdata$Mean.OC[t2ids]) # =0.1215
# median(sdata$Mean.OC[t2ids]) # is more like 0.194
# summary(sdata$Mean.OC[t2ids]) #    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.::0.0000  0.0000  0.1940  0.1215  0.2060  0.4580 
# boxplot(sdata[,c("Mean.OC")][t2ids])
# qids = single$Quality.Run == "Yes"
# boxplot(sdata[,c("Mean.OC")][t2ids][qids]) # incorrect because has zeros and NAs - same as qt2 below
# qt2oc  = sdata[,c("Mean.OC")][t2ids][qids]
# median(na.omit(qt2oc[qt2oc>0])) # 0.206
# mean(na.omit(qt2oc[qt2oc>0])) # 0.227
# boxplot(na.omit(qt2oc[qt2oc>0])) # this looks correct
# qt2dt  = sdata[,c("Mean.DT")][t2ids][qids]
# median(na.omit(qt2dt[qt2dt>0])) # 0.206
# mean(na.omit(qt2dt[qt2dt>0])) # 0.227
# boxplot(na.omit(qt2dt[qt2dt>0])) # this looks correct
# 
# moc = aggtags_mean$Mean.OC
# omoc = moc[order(-moc),drop=TRUE]
# #barplot(t(as.matrix(omoc)))
# # summaries by tag
# tagsumm = by(sdata,sdata["Tag"],summary)
# #tagsumm[gtags[1]] # produces summary but 
# 
# 
# 
# 
# gtags = single[!duplicated(single$Tag.Name),c("Tag.Name")]
# machines = sort(single[!duplicated(toupper(single$Station)),c("Station")])
# status = single[!duplicated(single$Status),c("Status")]
# lohi = strsplit(single$CellsActive,"-")
# lo=NULL
# hi=NULL
# for (i in 1:length(lohi)) {
#   lo[i] = as.numeric(lohi[[i]][1])
#   hi[i] = as.numeric(lohi[[i]][2])
# }
# numActive = hi-lo
# 
# cy = read.csv("Cy Experiments.csv",as.is=T)
# cDates = as.Date(as.character(cy$Date),"%y%m%d")
# cdata = data.frame(format(cDates),cy[c("Tag.Name","Station","ChipNum","Status")])
# 
# dc = read.csv("DC Competition Experiments.csv", as.is=T)
# dDates = as.Date(as.character(dc$Date),"%y%m%d")
# ddata = data.frame(format(dDates),dc[c("Tag.Names","Station","ChipNum","Status")])
# 
# tdf=data.frame(single$Tag.Name,single$Mean.capturing.level.OC)