#Merge NCEAS data

##import individual datafiles
AND<-read.csv("C://Users//Ashr//Dropbox//UNH LTER Mini Workshop//Data Sets//MergeData//Andrews.csv", header=TRUE)
BAL1<-read.csv("C://Users//Ashr//Dropbox//UNH LTER Mini Workshop//Data Sets//MergeData//BaltimoreLTER.csv", header=TRUE)
BAL2<-read.csv("C://Users//Ashr//Dropbox//UNH LTER Mini Workshop//Data Sets//MergeData//BaltimoreOther.csv", header=TRUE)
BON<-read.csv("C://Users//Ashr//Dropbox//UNH LTER Mini Workshop//Data Sets//MergeData//Bonanza.csv", header=TRUE)
FIN<-read.csv("C://Users//Ashr//Dropbox//UNH LTER Mini Workshop//Data Sets//MergeData//Finland.csv", header=TRUE)
HUB<-read.csv("C://Users//Ashr//Dropbox//UNH LTER Mini Workshop//Data Sets//MergeData//HubbardBrook.csv", header=TRUE)##updated 5/9/17
KON<-read.csv("C://Users//Ashr//Dropbox//UNH LTER Mini Workshop//Data Sets//MergeData//KONZA.csv", header=TRUE)
LUQ<-read.csv("C://Users//Ashr//Dropbox//UNH LTER Mini Workshop//Data Sets//MergeData//Luquillo.csv", header=TRUE)
NIW<-read.csv("C://Users//Ashr//Dropbox//UNH LTER Mini Workshop//Data Sets//MergeData//NIWOT.csv", header=TRUE)
Timb<-read.csv("C://Users//Ashr//Dropbox//UNH LTER Mini Workshop//Data Sets//MergeData//TimberlakeWetlands.csv", header=TRUE)
Trop<-read.csv("C://Users//Ashr//Dropbox//UNH LTER Mini Workshop//Data Sets//MergeData//TropicalBrookshire.csv", header=TRUE)
LIN<-read.csv("C://Users//Ashr//Dropbox//UNH LTER Mini Workshop//Data Sets//MergeData//LINX2.csv", header=TRUE)
UK<-read.csv("C://Users//Ashr//Dropbox//UNH LTER Mini Workshop//Data Sets//MergeData//UK.csv", header=TRUE)
WB<-read.csv("C://Users//Ashr//Dropbox//UNH LTER Mini Workshop//Data Sets//MergeData//Walker_Branch.csv", header=TRUE)##updated 5/9/17
NC<-read.csv("C://Users//Ashr//Dropbox//UNH LTER Mini Workshop//Data Sets//MergeData//NCurban.csv", header=TRUE)
RP<-read.csv("C://Users//Ashr//Dropbox//UNH LTER Mini Workshop//Data Sets//MergeData//RioPiedras.csv", header=TRUE)
LMP<-read.csv("C://Users//Ashr//Dropbox//UNH LTER Mini Workshop//Data Sets//MergeData//Lamprey.csv", header=TRUE)
COL<-read.csv("C://Users//Ashr//Dropbox//UNH LTER Mini Workshop//Data Sets//MergeData//Colorado.csv", header=TRUE)
Sp_Arc<-read.csv("C://Users//Ashr//Dropbox//UNH LTER Mini Workshop//Data Sets//MergeData//Spencer_Arctic.csv", header=TRUE)
Sp_Cong<-read.csv("C://Users//Ashr//Dropbox//UNH LTER Mini Workshop//Data Sets//MergeData//Congo.csv", header=TRUE)
COL_Br<-read.csv("C://Users//Ashr//Dropbox//UNH LTER Mini Workshop//Data Sets//MergeData//College_Brook.csv", header=TRUE)

NIW2<-subset(NIW, Type=="stream")#only include streams for NIW


##Script to Merge all data
dflist = list(AND,BAL1,BAL2,BON,FIN,HUB,KON,LUQ,NIW2,Timb,Trop,LIN,UK,WB,NC,RP, LMP, COL, Sp_Arc, Sp_Cong, COL_Br)
NCEAS_MERGE = Reduce(function(...) merge(..., all=T), dflist)

#export Merged file
workdir1<-"C://Users//Ashr//Dropbox//UNH LTER Mini Workshop//Data Sets//MergeData"
setwd(workdir1)
write.table(NCEAS_MERGE, "NCEAS_merge_10MAY17.csv",sep=",",col.names = NA)

library("data.table")
NCEAS_MERGE[NCEAS_MERGE==0]<-NA# replaces all values of 0 with NA


NCEAS_MERGE2<-read.csv("C://Users//Ashr//Dropbox//UNH LTER Mini Workshop//Data Sets//MergeData//NCEAS_merge_10MAY17.csv", header=TRUE,  na.strings="NA", stringsAsFactors = FALSE)
NCEAS_MERGE$Date2<-as.POSIXct(NCEAS_MERGE$Date, "%Y-%m-%d")
unique(NCEAS_MERGE$Site)

library(dplyr)
length(which(NCEAS_MERGE$DOC > 0 & NCEAS_MERGE$NO3N > 0 &NCEAS_MERGE$DON > 0 & NCEAS_MERGE$NH4N > 0   ))
length(which(NCEAS_MERGE$DOP>0))

freq1<-read.csv("C://Users//Ashr//Dropbox//UNH LTER Mini Workshop//Data Sets//MergeData//freq.csv", header=TRUE)
ggplot(data=freq1, aes(freq1$Freq)) + geom_histogram()

################################################################
################################################################
###Calculate means by column
library( taRifx )
NCEAS_MERGE <- japply( NCEAS_MERGE, which(sapply(NCEAS_MERGE, class)=="character"), as.numeric )#this converts from character to numeric so we can calculate means

library(plyr)
NCEAS_MERGE_mean<-ddply(NCEAS_MERGE,.(Site, LTER),colwise(mean,is.numeric, na.rm=TRUE)) ## are some columns stored as character? 
NCEAS_MERGE_mean3<-ddply(NCEAS_MERGE,.(Site,LTER),colwise(mean))

write.table(NCEAS_MERGE_mean, "NCEAS_mean_17MAY17.csv",sep=",",col.names = NA)
