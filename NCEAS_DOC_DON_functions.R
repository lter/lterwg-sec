rm(list = ls())

library(ggplot2)
library(plyr)
library(dplyr)
library(tidyr)
library(plotrix)#calculate standard error with dplyr and tidyr
library(cowplot)
library(scales)
library(lme4)
library(corrplot) #Making correlation coeff scater plot matroces
library(devtools) #updates to corrplot
library(trend) #Non-Parametric Trend Tests and Change-Point Detection
library(Kendall)
library(mice)
library(data.table)
library(gridExtra)
library(gtable)
library(grid)
library(reshape)
library(reshape2)
library(RColorBrewer)
library(purrr)

options(stringsAsFactors = FALSE)

# setwd("~/Documents/PhD Dissertation/NCEAS Synthesis/NCEAS Data 2019")


###### CONSTANTS ########
data_path <- "Data"


###### FUNCTIONS ########

#### Subsetting stream and dates -----

data_subset <- function(site, stream, st_year){
  # Filter site
  site_DOC <- C_N %>% filter(LTER==site) %>% 
    select(Site_Stream_Name,Year,Month,Day,MonthNum,DOC) #%>% 
    # group_by(Site_Stream_Name)
  # Check stream names
  # unique(LUQ_DOC$Site_Stream_Name)
  
  #a check to see how many months/entries per year
  stream_in_site <- site_DOC %>% 
    filter(Year >= st_year) %>% 
    filter(Site_Stream_Name==stream)
}

# Main function computing the necessary stats 
main <- function(lter_site, stream_name, start_year, start_month, end_year, end_month, frequ){
  
  #printing the site in processing
  cat(sprintf("Processing site %s \n", stream_name))
  
  # Subset the data according to site and dates
  stream_subset <- data_subset(lter_site, stream_name, start_year)
  
  #Making 5 different data sets(seeds) with 50 iterations to fill in missing DOC or DOC data
  stream_DOC <- mice(data=stream_subset,m=5,method="pmm",maxit = 50,seed = 500, print=FALSE)
  
  #Tydr has complete so need to specify complete from mice hence the ::
  #Completes the missing DOC data from iteration 1 from above but can choose and from 1-5
  stream_comp <- mice::complete(stream_DOC,1)
  
  #Creating monthly mean DOC values to base time series
  stream_month <- stream_comp %>% 
    group_by(Year, MonthNum) %>% 
    summarise(DOC=mean(DOC, na.rm=T)) %>% 
    arrange(Year, MonthNum)
  
  test_month <- stream_month %>% group_by(Year) %>% count(Year)
  
  #### Test for missing month
  if((min(test_month$n) < 12) && (lter_site!="BNZ")){
    #Making the month column numeric
    stream_month$MonthNum <- as.numeric(as.character(stream_month$MonthNum))
    
    #Truncating data to months with 8-12 months
    stream_Month_Trunc<-stream_month %>% filter(Year>=start_year & Year<=end_year) %>% arrange(Year,MonthNum)
    
    #Creating rows for missing months
    stream_Month_Fill <- setDT(stream_Month_Trunc)[, .SD[match(1:12, MonthNum)], by = Year]
    
    #Giving new row a month number
    stream_Month_Fill$MonthNum <- rep(1:12,nrow(stream_Month_Fill))
    
    #Re-doing iterations for newly made empty rows
    stream_DOC_Fill <- mice(data=stream_Month_Fill,m=5,method="pmm",maxit = 50,seed = 500)
    
    #Filling in the newly made empty rows
    Comp_stream_Fill<- mice::complete(stream_DOC_Fill,1)
    Comp_stream_Fill %>% group_by(Year) %>% count(Year)
  }
  
  #Creating a time series object
  #Start at year 2000 on month 1 since the frequency is monthly
  #### CHANGE START YEAR
  stream_ts = ts(stream_month$DOC,frequency=frequ,
                 start=c(as.numeric(start_year),1), end=c(as.numeric(end_year),end_month))
  
  #Plotting data as a time series (would need to change y-axis label)
  plot.ts(stream_ts)
  
  #Seasonal Mann Kendall test
  seasonal_Kendall <- SeasonalMannKendall(stream_ts)
  # # Tau
  # seasonal_Kendall$tau[1]
  # # P-value
  # seasonal_Kendall$sl[1]
  #The purpose of this test is to test for monotonic trend
  
  #Sen slope
  sen_slope <- sens.slope(stream_ts)
  # # p-value
  # sen_slope$p.value
  # #slope
  # sen_slope$estimates
  data.frame(LTER=lter_site, 
            stream_name=stream_name,
            ken_tau=seasonal_Kendall$tau[1], 
            ken_p.value=seasonal_Kendall$sl[1], 
            sen_slope=sen_slope$estimates, 
            sen_p.values=sen_slope$p.value)
}


################################################################################

###################
#### MAIN code ####
###################


######### DOC Time Series ##########

# Read the LUT
lut <- read.csv(file.path(data_path,"LookUpTable.csv"), header=T) %>% 
  select(-2)

#Entire global data
C_N <- read.csv(file.path(data_path,"NCEAS_190211.csv"),header=T)

# split dates into month year
######## Editing master file ####

#Change formating of date column to R friendly
C_N$Sampling_Date<-as.Date(C_N$Sampling_Date, format="%m/%d/%Y")

#Extracting month from dates
C_N$Month <- format(as.Date(C_N$Sampling_Date), "%B") #use lowercase m for month number

#Extracting years from dates
C_N$Year <- format(as.Date(C_N$Sampling_Date), "%Y") #lowercase y for 99' instead 1999

#Extracting days from dates
C_N$Day <- format(as.Date(C_N$Sampling_Date), "%d") #day number

C_N$MonthNum <- format(as.Date(C_N$Sampling_Date), "%m") #Month number so it's easier to sort

### TESTTING Area
# hardcoded paramaeters for testing
# 
# lter_site <- "LUQ"
# stream_name <- "RI"
# start_year <- 2000
# start_month <- 1
# end_year <- 2015
# end_month <- 12
# frequ <- 12

# #Testing main function on online
# lut1 <- lut[9,]
# tt <- main(lut1$LTER, lut1$Abrv, lut1$StartYear, lut1$start.month, lut1$EndYear, lut1$End.Month, lut1$Frequency)
# # function(lter_site, stream_name, start_year, start_month, end_year, end_month, frequ)
# # LTER Abrv StartYear start.month EndYear End.Month Frequency
# # LUQ   RI      2000           1    2015        12        12

# Run for ALL
all_sites_table <- pmap_dfr(lut, ~ main(lter_site=..1, stream_name=..2, start_year=..3, 
                       start_month=..4, end_year=..5, end_month=..6, frequ=..7))

# Write the file
write_csv(all_sites_table, "all_sites_KenSen.csv")
