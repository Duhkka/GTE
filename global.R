setwd("~/Projects/R/GTE")
#library(RMySQL)
#library(DBI)
options(stringsAsFactors=F)

dc_single               = read.csv("SingleLevel.csv",as.is=T)
dc_background           = read.csv("BACK CSV.csv", as.is=T)
dc_cy                   = read.csv("CY CSV.csv", as.is=T)
dc_competition          = read.csv("COMP CSV.csv", as.is=T)

dc_single_colnames      = names(dc_single)
dc_background_colnames  = names(dc_background)
dc_cy_colnames          = names(dc_cy)
dc_competition          = names(dc_competition)

dc_single_Dates         = as.Date(as.character(dc_single$Date), "%y%m%d")
dc_single_date_list     = structure(dc_single_Dates, Class="Date")
dc_single_earliest      = min(dc_single_date_list, na.rm=TRUE)
dc_single_latest        = max(dc_single_date_list, na.rm=TRUE)
dc_single_tags          = dc_single[!duplicated(dc_single$Tag.Name),c("Tag.Name")]
dc_single_data = data.frame(format(dc_single_Dates),single[c("Tag.Name","Full.Name","Station","ChipNum","Status","Num.cells.clean","Num.cells.quality.sequencing","Mean.capturing.level.OC","Capturing.Level.Standard.Deviation","Mean.Dwell.time","Quality.Run")])
names(dc_single_data) = c("Date","Tag","TagName","Station","Chip","Status","Clean","Seq","Mean.OC","sigma.OC","Mean.DT","Quality.Run")

ac_single               = read.csv("single_tag_ac.csv",as.is=T)
ac_background           = read.csv("ac_background.csv", as.is=T)

ac_single_colnames      = names(ac_single)
ac_background_colnames  = names(ac_background)


ac_single_Dates         = as.Date(as.character(ac_single$expDate),"%y%m%d")
ac_single_date_list     = structure(ac_single_Dates,Class="Date")
ac_single_earliest      = min(ac_single_date_list,na.rm=TRUE)
ac_single_latest        = max(ac_single_date_list,na.rm=TRUE)
ac_single_tags          = ac_single[!duplicated(ac_single$tags),c("tags")]
ac_single_data          = data.frame(format(ac_single_Dates),ac_single[c("tags","stationID","chipNum","Inactive.Cells","Active.Reps","Single.Pore.Reps","Inactive.Cell.Reps","Single.Pore.Cells")])

ac_background_Dates     = as.Date(as.character(ac_background$expDate), "%y%m%d")
ac_background_date_list = structure(ac_background_Dates, Class="Date")
ac_background_earliest  = min(ac_background_date_list, na.rm=TRUE)
ac_background_latest    = max(ac_background_date_list, na.rm=TRUE)
ac_background_tags      = ac_background[!duplicated(ac_background$tags),c("tags")]
ac_background_data      = data.frame(format(ac_background_Dates),ac_background[c("tags","stationID","chipNum","Inactive.Cells","Active.Reps","Single.Pore.Reps","Inactive.Cell.Reps","Single.Pore.Cells")])

# HemoDates = as.Date(as.character(ac_single$Hemo.Complex.Prep),"%y%m%d")
# date_list = structure(sDates,Class="Date")
# earliest = min(date_list,na.rm=TRUE)
# latest   = max(date_list,na.rm=TRUE)
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