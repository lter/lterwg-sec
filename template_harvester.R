library(dplyr)
library(readxl)
library(tidyverse)
library(stringr)
library(lubridate)

# ------ General Comments ------ #
#TIMB works
#Problem with Fin
setwd("/Users/celine/Desktop/Templates_updated_26OCT2017")
xls_file <- list.files("/Users/celine/Desktop/Templates_updated_26OCT2017/Site_Data_Template")[10]
xls_file
LUT_file <- "convert/Conversions_Celine11-8-17.xlsx"

# ---------- Step 1. READ THE DATA ---------- #

# Read the unit conversion LUT

LUT <- read_excel(LUT_file)

# Read the raw units
setwd("/Users/celine/Desktop/Templates_updated_26OCT2017/Site_Data_Template")

units <- read_excel(xls_file, sheet = "Solute Units") %>%
  select(1:2)

# Read the data in & check values
data <- read_excel(xls_file, sheet = "Raw Data", na = "NA")

names <- names(data)
str(data)

# ---------- Step 2. CLEAN THE DATA ---------- #

# Check if column is numeric, and if not, need to remove #VALUE! values to NA and set as numeric
#sec_df <- as.data.frame(lapply(sec_df, function(x) gsub("#VALUE!", NA, x)), stringsAsFactors = FALSE)

# Set all -9999 values to NA

data <- as.data.frame(lapply(data, function(x){replace(x, x==-9999, NA)}))
names(data) <- names # Sets same names for conversion

# Check if Sampling Date, Time are in standard format

#### For V4_Fin
if (xls_file == "Site_Data_Template_V4_Fin.xlsx") { #class(data$`Sampling Date`)[1] != "POSIXct"
  data$`Sampling Date` <- gsub("[.]","-", data$`Sampling Date`)
  data$`Sampling Date` <- dmy(data$`Sampling Date`)
}

### Specific to V4_AND, ASSUMING cm == cms
if (xls_file == "Site_Data_Template_V4_AND.xlsx") {
  units[[1,2]] <- "cms"
  data$Time <- strftime(data$Time, format = "%H:%M:%S", tz = "GMT") 
}
### Specific to ARC_GRO, ASSUMING Alkalinity mg/L == mg HCO3/L
if (xls_file == "Site_Data_Template_V4_ARC_GRO.xlsx") {
  units[[2,2]] <- "mg HCO3/L"
}

### Specific to ARC_PAR, ASSUMING Alkalinity mg/L == mg HCO3/L
if (xls_file == "Site_Data_Template_V4_ARC_PAR.xlsx") {
  data$`Site/Stream Name` <- str_split(data$`Site/Stream Name`, " ", simplify = TRUE)[,1]
  units[[2,2]] <- "mg HCO3/L"
}

### Specific to HBF
if (xls_file == "Site_Data_Template_V4_HBF.xlsx") {
  data$Time <- strftime(data$Time, format = "%H:%M:%S", tz = "GMT") 
}

# ---------- Step 3. JOIN THE DATA ---------- #

# Set Measurement in units with same chemical name as LUT

LUT2 <- mutate(LUT, Measurement=str_split(Required_Form,"-", simplify = TRUE)[,1])

# Join two tables together to get conversions value

convert <- left_join(units, LUT2, by = c("Measurement" = "Measurement", "Unit" = "Options"))

# Make sure variables are the same in 'Measurements' column of convert table with 'Variable' names in data

convert$Measurement[grep("Specific Conductance", convert$Measurement)] <- "Spec Cond"
convert$Measurement[grep("Q", convert$Measurement)] <- "Q (Discharge)"
convert$Measurement[grep("Alkalinity", convert$Measurement)] <- "alkalinity"

# ---------- Step 4. CONVERT THE DATA ---------- #

# Convert values in 'data' table by 'convert' conversions table


# STEPS:
# For all variables in data (length(data))
# For all the observations in nrow(convert)
# If variable == convert$Measurement[i]
# Multiply column by convert$Conversion[i]

for (i in 1:length(data)) {
  for (j in 1:nrow(convert)) {
    if (names(data)[i] == convert$Measurement[j]) {
      if(!is.na(convert$Conversion[j])) {
        data[,i] = data[,i] * convert$Conversion[j]
      }
    }
  }
}



# ---------- Step 5. Export as .csv file ------- #
setwd("/Users/celine/Desktop/Templates_updated_26OCT2017/csv_conversions")
str_sub(xls_file, -5, str_length(xls_file)) <- "_converted.csv"
output_file <- xls_file
write.csv(data, output_file, row.names = FALSE, fileEncoding = "UTF-8", quote = TRUE)

