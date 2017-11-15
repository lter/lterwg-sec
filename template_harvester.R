#################################################################################################
### This script harvests the templates created for the different LTER and international sites ###
### that were selected by the LTER working group Stream Elementary Stream elemental cycling   ###
#################################################################################################

### Authors: Celine Mol and Julien Brun, NCEAS, UCSB
### Email: SciComp@nceas.ucsb.edu



# ------ General Comments ------ #
#TIMB works
#Problem with Fin


### LIBRARIES ----

library(dplyr)
library(readxl)
library(tidyverse)
library(stringr)
library(lubridate)


### CONSTANT ----

# setwd("/Users/celine/Desktop/Templates_updated_26OCT2017")
# Set the path to directory containing the templates and LUT
data_path <- "Templates_updated_26OCT2017"
output_path <- file.path(data_path, "csv_conversions")
# test if the directory exists
dir.create(output_path, showWarnings = FALSE)
# Filename to LUT
# LUT_file <- file.path(data_path, "Conversions_Celine11-8-17.xlsx") ## This file is not the the GDrive
LUT_file <- file.path(data_path, "Conversions_Celine11-3-17.xlsx") ## This file is not the the GDrive

# List all the templates
xls_templates <- list.files(path = data_path, pattern = "Site_Data_Template", full.names = TRUE)
xls_templates


### MAIN ----

# Need to add the loop througg the files

xls_file <- xls_templates[1]
xls_file

# ---------- Step 1. READ THE DATA ---------- #

# Read the unit conversion LUT

LUT <- read_excel(LUT_file)

# Read the raw units
units <- read_excel(xls_file, sheet = "Solute Units") %>%
  select(1:2)

# Read the data in & check values
data <- read_excel(xls_file, sheet = "Raw Data", na = "NA")

if (xls_file == "Site_Data_Template_V4_NIW.xlsx") {
  data <- read_excel(xls_file, sheet = "Raw Data", 
                     col_types = c("text", "text", "text", "text", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric", 
                                   "numeric", "blank"))
  
}
if (xls_file == "Site_Data_Template_V4_UK.xlsx") {
  data <- read_excel(xls_file, sheet = "Raw Data", 
                     col_types = c("text", "text", "date", "text", "text", 
                                   "text", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric", 
                                   "numeric", "numeric", "numeric", "numeric", 
                                   "numeric"))
  
}

names <- names(data)
str(data)


# ---------- Step 2. CLEAN THE DATA ---------- #

# Set all -9999 values to NA

data <- as.data.frame(lapply(data, function(x){replace(x, x==-9999, NA)}))
names(data) <- names # Resets names for conversion

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

### Specific to LMP
if (xls_file == "Site_Data_Template_V4_LMP.xlsx") {
  names(data)[13] <- "DO mg/L"
}

## Specific to LUQ
if(xls_file == "Site_Data_Template_V4_LUQ.xlsx") {
  lengths <- str_length(data$Time)
  values <- grep(3, lengths)
  str_sub(data$Time[values], 0, 0) <- "0"
  str_sub(data$Time, -2, 1) <- ":"
  str_sub(data$Time, 6, 6) <- ":00"
  data$Time <- as.POSIXct(data$Time, format = "%H:%M:%S") # Puts it in "2017-11-09 09:33:00 PST"
  data$Time <- strftime(data$Time, format = "%H:%M:%S") # Puts it back in "09:33:00"
  data$`Sampling Date` <- as.Date(data$`Sampling Date`, format = "%m/%d/%Y")
  names(data)[13] <- "DO mg/L"
  names(data)[11] <- "Temp C"
}

## Specific to NIW
if(xls_file == "Site_Data_Template_V4_NIW.xlsx") {
  data$`Sampling Date` <- ymd(data$`Sampling Date`)
  data$Time <- as.character(data$Time)
  
}

## Specific to UK
if(xls_file == "Site_Data_Template_V4_UK.xlsx") {
  data$`Sampling Date` <- as.Date(data$`Sampling Date`, format = "%m/%d/%Y")
  names(data)[13] <- "DO mg/L"
}

data$LTER <- as.character(data$LTER)
data$`Site/Stream Name` <- as.character(data$`Site/Stream Name`)

# ---------- Step 3. JOIN THE DATA ---------- #

# Set Measurement in units with same chemical name as LUT

LUT2 <- mutate(LUT, Measurement=str_split(Required_Form,"-", simplify = TRUE)[,1])

# Join two tables together to get conversions value

convert <- left_join(units, LUT2, by = c("Measurement" = "Measurement", "Unit" = "Options"))

# Make sure variables are the same in 'Measurements' column of convert table with 'Variable' names in data

convert$Measurement[grep("Specific Conductance", convert$Measurement)] <- "Spec Cond"
convert$Measurement[grep("Q", convert$Measurement)] <- "Q (Discharge)"
convert$Measurement[grep("Alkalinity", convert$Measurement)] <- "alkalinity"
## DO % and Temp C not same, but these don't seem to have conversions so not important

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

# setwd("/Users/celine/Desktop/Templates_updated_26OCT2017/csv_conversions")
outname <- paste0(tools::file_path_sans_ext(basename(xls_file)), "_converted.csv")
output_file <- file.path(output_path, outname)
write.csv(data, output_file, row.names = FALSE, fileEncoding = "UTF-8", quote = TRUE)
