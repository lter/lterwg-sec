# r NCEAS Files to Examine Data 
# 10/26/17
# ---------------------------------------------------------------------------
#
library(dplyr)
#
# ---------------------------------------------------------------------------
# Read in the data
# ---------------------------------------------------------------------------
# Change the working directory
wname <- getwd()
dname <- paste(wname,"Data",sep="/")
setwd(dname)
NCEAS <- read.csv("NCEAS_CLEANED_25OCT17_RAW.csv")
setwd(wname)

# Formating date 
NCEAS$Date <- as.Date(NCEAS$Date,format = "%m/%d/%Y")

# Creating a data frame to look at date stucture of the site 
s<-unique(NCEAS$Site)
Time_S<-data.frame(matrix(data=NA, nrow = 796, ncol = 25))
Time_S[,1]<-s
colnames(Time_S)<-c("Site","DOCs","DOCe","DOCcnt","DOCfrq","DONs","DONe","DONcnt","DONfrq",
                    "DOPs","DOPe","DOPcnt","DOPfrq","SRPs","SRPe","SRPcnt","SRPfrq",
                    "NO3Ns","NO3Ne","NO3Ncnt","NO3Nfrq","NH4Ns","NH4Ne","NH4Ncnt","NH4Nfrq")

con<-c("DOC","DON","DOP", "SRP", "NO3N","NH4N")

for (l in 1:length(s)) {
    a<-filter(NCEAS,Site==s[l])
      for (h in 1:length(con)) {
        st<-paste0(con[h],"s")
        ed<-paste0(con[h],"e")
        cnt<-paste0(con[h],"cnt")
        frq<-paste0(con[h],"frq")
        p<-a$Date[!is.na(con[h])]
          if (sum(is.na(a[,con[h]])==nrow(a))) {
            Time_S[l,st]<-NA
            Time_S[l,ed]<-NA
            Time_S[l,cnt]<-NA
            Time_S[l,frq]<-NA
          } else{
            Time_S[l,st]<-as.character(p[1])
            Time_S[l,ed]<-as.character(p[length(p)])
            Time_S[l,cnt]<-length(p)
            Time_S[l,frq]<-difftime(p[length(p)],p[1],units="days")/length(p)
          }
      }
  }
write.csv(Time_S,"Time_Series_Duration_Data.csv")

