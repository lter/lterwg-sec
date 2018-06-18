#################################################################################################
### This script harvests the templates created for the different LTER and international sites ###
###           that were selected by the LTER working group Stream Elemental Cycling           ###
#################################################################################################

### Authors: Celine Mol, Margaux Sleckman and Julien Brun, NCEAS, UCSB
### Email: SciComp@nceas.ucsb.edu


#### LIBRARIES ####
library(googledrive)
library(purrr)
library(dplyr)
library(readxl)
library(tidyverse)
library(stringr)
library(lubridate)

#### CONSTANTS ####

# define path if not using RStudio project relative to the repository
# setwd("/Users/celine/Desktop") 

# Set the relative path to directory containing the templates and LUT
# Assuming the top folder for the data is named "Templates_201802", 
# similar as on the working group Goolgle Drive

template_folder <- "Templates_201802" 
# test if the directory exists
dir.create(template_folder, showWarnings = FALSE)

# List the templates from the google Drive
drive_folder <- "1HgU9ynNdUGD-YoTbk4hoK8KTV-uChoB8"
templates_on_drive <- drive_ls(as_id(drive_folder), pattern = "xlsx")

output_path <- file.path(template_folder, "csv_conversions")
# Set the relative path to all units file
units_path <- file.path(template_folder, "LTER_units.csv")
# test if the directory exists
dir.create(output_path, showWarnings = FALSE)

# Filename to LUT
LUT_file <- file.path(template_folder, "Conversions.xlsx")

#### FUNCTIONS ####

#' Download the templates from Google Drive
#'
#' @param templates_dribble a dribble
#' @param local_folder a character
#'
#' @export
#'
#' @examples template_downloader(templates_drive, "Templates_201802")
template_downloader <- function(templates_dribble, local_folder){
  # Download the templates
  for (i in 1:nrow(templates_dribble)){
    drive_download(as_id(templates_dribble$id[[i]]), 
                   file.path(local_folder, templates_dribble$name[[i]]),
                   overwrite = TRUE)
  }
}

#' Read Excel template of LTER and other specific sites to extract data 
#'
#' @param xls_file A character. Filename.
#'
#' @return A data frame containing the data.
#' @export
#'
#' @examples
#' read_the_data("Templates_updated_26OCT2017/Site_Data_Template_V4_ARC_GRO.xlsx")
#'
 
read_the_data <- function(xls_file) {
  # Read the data in & check values
  read_data <- read_excel(xls_file, sheet = "Raw Data", na = "NA")
  
  # Some templates are slightly different
  
  if (str_detect(xls_file, "NIW")) {
    read_data <- read_excel(xls_file, sheet = "Raw Data", 
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
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "blank"))
    
    
  }
  if (str_detect(xls_file, "UK")) {
    read_data <- read_excel(xls_file, sheet = "Raw Data", 
                            col_types = c("text", "text", "date", "date", "text", 
                                          "text", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", "numeric", 
                                          "numeric", "numeric", "numeric", "numeric", "numeric"))
  
    # date in this template is set to date. 
    # set time to date as well to avoid conversion to time. 
  }
  
  if (str_detect(xls_file, "WBR")) {
    read_data <- read_excel(xls_file, sheet = "Raw Data", 
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
                                          "numeric", "numeric","numeric", "numeric", "numeric"))
 # date in this template is set to Text. It did not render the conversion when sample time column was of type: date. 
    
  }
  
  if (str_detect(xls_file, "LIN")) {
    read_data <- read_excel(xls_file, sheet = "Raw Data", 
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
                                          "numeric", "numeric","numeric", "numeric", "numeric"))
    
# date in this template is set to Text. Otherwise, conversion turns dates into numerals. 
    
  }
  
   if (str_detect(xls_file, "Fin")) {
    read_data <- read_excel(xls_file, sheet = "Raw Data",
                         col_types = c("text", "text", "text", "text", "text",
                                          "text", "numeric", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric", "numeric",
                                          "numeric", "numeric","numeric", "numeric","numeric"))
    
    #Fin dates should be kept as text! Otherwise, conversion turns dates into numerals.
    
      }

  if (str_detect(xls_file, "HBF")) {
    read_data <- read_excel(xls_file, sheet = "Raw Data",
                            col_types = c("text", "text", "date", "date", "text",
                                          "text", "numeric", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric", "numeric",
                                          "numeric", "numeric","numeric", "numeric","numeric"))
    
    # half of the Sampling Date column defined differently from other half. We read the original dataset twice to extract both vectors that have different type. 
    
    # 1. Date values read when whole column is defined as type "date" 
    date.format1 <- read_data$`Sampling Date`[0:2889]
    date.format1 <- ymd(date.format1)
    date.format1 <- strftime(date.format1, format = "%m/%d/%Y")
  
    read_data <- read_excel(xls_file, sheet = "Raw Data",
                            col_types = c("text", "text", "text", "date", "text",
                                          "text", "numeric", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric", "numeric",
                                          "numeric", "numeric", "numeric", "numeric",
                                          "numeric", "numeric","numeric", "numeric","numeric"))
    # 2. Date values read when whole column is defined as type "date" 
    date.format2 <- read_data$`Sampling Date`[2890:7423]
    date.format2 <- ydm(date.format2)
    date.format2 <- strftime(date.format2, format = "%m/%d/%Y")
    
      # Check how we can avoid explicitly stating the column range so that this part can be adaptable. 
    
    # combined columns
    date.format <-c(date.format1, date.format2)
    read_data$`Sampling Date` <- date.format
   
  }
 
# #merges tabs for UK dataset (wait on this. Dataset requires some cleaning before getting into a template).
#   if (str_detect(xls_file, "UK")) {
#   read_data <- function(xls_file){
#     sheets<-readxl::excel_sheets(xls_file)
#     x<-lapply(sheets, function(X) readxl::read_excel(xls_file) )
#     lapply(sheets, function(X) readxl::read_excel(xls_file, sheet=X))
#     names(x)<-sheets
#     x
#   }
#   }  # date in this template is set to Text. It did not render the conversion when sample time column was of type: date. 
  
 
  return(read_data)
}

#' Clean the data of various ways of storing NA as well as normalizing units
#'
#' @param data A data frame. Data set to be cleaned.
#' @param file A character. Filename. Used to detect special cases.
#'
#' @return A data frame.
#' @export
#'
#' @examples
#' clean_the_data(site_data, "Templates_updated_26OCT2017/Site_Data_Template_V4_ARC_GRO.xlsx")
clean_the_data <- function(data, file) {
  # Set all -9999 values to NA
  names <- names(data)
  data <- as.data.frame(lapply(data, function(x){replace(x, x==-9999, NA)}))
  names(data) <- names # Resets names for conversion
  
  
# Check if Sampling Date, Time are in standard format and other changes

#### Specific to V4_Fin
  if (str_detect(file, "Fin")){ # class(data$`Sampling Date`)[1] != "POSIXct"
    data$`Sampling Date` <- gsub("[.]","-", data$`Sampling Date`)
    data$`Sampling Date` <- dmy(data$`Sampling Date`)
  }
 
   if (str_detect(file, "Fin")){
    data$DOC<-data$TOC*0.95
    data$TDN<-data$TN*0.95
    data$DON<-(data$TDN-(data$NH4+data$NO3))
  }

### For V4_WBR
  
### Specific to V4_AND, ALSO ASSUMING cm == cms
  if (str_detect(file, "AND")) {
    #units[[1,2]] <- "cms"
    data$Time <- strftime(data$Time, format = "%H:%M:%S", tz = "GMT")
    data$`Sampling Date` <- dmy(data$`Sampling Date`)
  }
  
### Specific to ARC_GRO, ASSUMING Alkalinity mg/L == mg HCO3/L
  
### Specific to ARC_PAR, ASSUMING Alkalinity mg/L == mg HCO3/L
  if (str_detect(file, "ARC_PAR")) {
    data$`Site/Stream Name` <- str_split(data$`Site/Stream Name`, " ", simplify = TRUE)[,1]
    #units[[2,2]] <- "mg HCO3/L"
  }
  
  ### Specific to HBF
  if (str_detect(file, "HBF")) {
    data <- unique(data)     # to remove duplicates rows 
    data$Time <- strftime(data$Time, format = "%H:%M:%S", tz = "GMT")
    data$`Sampling Date` <- as.Date(data$`Sampling Date`, format = "%m/%d/%Y")
    
    # data$`Sampling Date` <- as.character(data$`Sampling Date`[0:2889])
    # data$`Sampling Date` <- as.Date(data$`Sampling Date`[2889:.])
    # data$`Sampling Date` <- parse_date_time(x=`Sampling Date`,
    #                                          orders = c("m/d/y","y-m-d"),
    #                                          tz="GMT", locale = Sys.getlocale("LC_TIME"))  # put in GMT format
    # data$`Sampling Date` <- strftime(data$`Sampling Date`, format = "%m/%d/%Y") #put into m/d/y format
 
    
    
    #data$`Sampling Date` <- parse_date_time(x = data$`Sampling Date`, orders = c("m/d/y","y-m-d"),
    #                tz="UTC", locale = Sys.getlocale("LC_TIME"))
    
    #data$`Sampling Date` <- as.Date(data$`Sampling Date`, format = "%m/%d/%Y")
    #data$`Sampling Date` <- parse_date_time(data$`Sampling Date`,orders = c("m/d/y", "y-d-m"))  
    #strftime(data$`Sampling Date`, format = c("m/d/y", "y-d-m"))
    #data$`Sampling Date` <- dmy(data$`Sampling Date`)
  }

### Specific to LMP
  if (str_detect(file, "LMP")) {
    names(data)[13] <- "DO mg/L"
  }
  
### Specific to LUQ
  if(str_detect(file, "LUQ")) {
    lengths <- str_length(data$Time)
    values <- grep(3, lengths)
    data$Time <- as.character(data$Time)
    str_sub(data$Time[values], 0, 0) <- "0"
    str_sub(data$Time, -2, 1) <- ":"
    str_sub(data$Time, 6, 6) <- ":00"
    data$Time <- as.POSIXct(data$Time, format = "%H:%M:%S") # Puts it in "2017-11-09 09:33:00 PST"
    data$Time <- strftime(data$Time, format = "%H:%M:%S") # Puts it back in "09:33:00"
    data$`Sampling Date` <- as.Date(data$`Sampling Date`, format = "%m/%d/%Y")
    names(data)[13] <- "DO mg/L"
    names(data)[11] <- "Temp C"
  }

### Specific to LIN
  if (str_detect(file, "LIN")){
    data$DON<-(data$TDN-(data$NH4+data$NO3))
  }

### Specific to TIM
  if (str_detect(file, "TIM")){
    data$DON<-(data$TDN-(data$NH4+data$NO3))
  }
    
### Specific to NIW
  if(str_detect(file, "NIW")) {
    data$`Sampling Date` <- ymd(data$`Sampling Date`)
    data$Time <- as.character(data$Time)
  }
  
  ### Specific to UK
  if(str_detect(file, "UK")) {
    data$`Sampling Date` <- as.Date(data$`Sampling Date`, format = "%m/%d/%Y")
    # data$Time <- as.POSIXct(data$Time, format = "%H:%M:%S")  #this was tested but is not necessary
    data$Time <- strftime(data$Time, format = "%H:%M:%S", tz="GMT") #changes the GMT format, removes AM/PM
    names(data)[13] <- "DO mg/L"
  }

  data$LTER <- as.character(data$LTER)
  data$`Site/Stream Name` <- as.character(data$`Site/Stream Name`)
  return(data)
}

#' Homogenization of the measurement units across the different sites
#'
#' @param conversions_file A character. A filename to your conversions file.
#' @param file A character. Filename. Used to import "Solute Units" sheet.
#'
#' @return A data frame. Your prepared conversions data frame.
#' @export
#'
#' @examples
#' join_the_data(LUT_file, "Templates_updated_26OCT2017/Site_Data_Template_V4_ARC_GRO.xlsx")
join_the_data <- function(conversions_file, file) {
  # Read the unit conversion LUT
  LUT <- read_excel(conversions_file)
  
  # Read the raw units
  units <- read_excel(file, sheet = "Solute Units") %>%
    select(1:2)
  
  # Set Measurement in units with same chemical name as LUT
  
  LUT2 <- mutate(LUT, Measurement=str_split(Required_Form,"-", simplify = TRUE)[,1])
  
  # Join two tables together to get conversions value
  
  convert <- left_join(units, LUT2, by = c("Measurement" = "Measurement", "Unit" = "Options"))
  
  # Make sure variables are the same in 'Measurements' column of convert table with 'Variable' names in data
  convert$Measurement[grep("Specific Conductance", convert$Measurement)] <- "Spec Cond"
  convert$Measurement[grep("Q", convert$Measurement)] <- "Q (Discharge)"
  convert$Measurement[grep("Alkalinity", convert$Measurement)] <- "alkalinity"
  convert$Measurement[convert$Measurement == "DO"] <- "DO mg/L"
  ## DO % and Temp C not same, but these don't seem to have conversions so not important
  
  return(convert)
}


#' Make unit conversions on your cleaned data file
#'
#' @param convert A data frame. Your prepared conversions data frame.
#' @param data A data frame. Your cleaned data.
#'
#' @return
#' @export
#'
#' @examples
#' convert_the_data(conversion_file, clean_data)
convert_the_data <- function(convert, data) {
  # Convert values in 'data' table by 'convert' conversions table
  # STEPS:
  # For all variables in data (length(data))
  # For all the observations in nrow(convert)
  # If variable == convert$Measurement[i]
  # Multiply column by convert$Conversion[i]
  
  # Remove NAs from the conversion (to save time in the loop)
  converter <- na.omit(convert)
  
  for (i in 1:length(data)) {
    for (j in 1:nrow(converter)) {
      if (names(data)[i] == converter$Measurement[j]) {
        data[,i] = data[,i] * converter$Conversion[j]
      }
    }
  }

    
  return(data)
}


#' Export your cleaned and converted data into csv
#'
#' @param data A data frame. Your prepared converted data frame.
#' @param file A character. File name. The name of the xls template you're working with.
#' @param outpath A character. File path. The name of the path where you want your csv converted file to go.
#'
#' @return
#' @export 
#'
#' @examples
#' make_csv(converted, site_template, output_path)
make_csv <- function(data, file, outpath) {
  # setwd("/Users/celine/Desktop/Templates_updated_26OCT2017/csv_conversions")
  outname <- paste0(tools::file_path_sans_ext(basename(file)), "_converted.csv")
  output_file <- file.path(outpath, outname)
  write.csv(data, output_file, row.names = FALSE, fileEncoding = "UTF-8", quote = TRUE)
}

#' Create a data frame for the units data
#'
#' @param conversion A data frame. Your prepared conversions data frame.
#'
#' @return A data frame. Your empty units data frame.
#' @export
#'
#' @examples
#' create_units_data(conversion_file)
create_units_data <- function(conversion){
  units_data <- as.data.frame(t(conversion$Measurement))
  units_data <- cbind("LTER", units_data)
  colnames(units_data) <- as.character(unlist(units_data[1,]))
  units_data <- units_data[-1,]
  
  return(units_data)
}

#' Fill the units in units data frame
#'
#' @param site_template A character. File name. The name of the xls template you're working with.
#' @param conversion A data frame. Your prepared conversions data frame.
#' @param units_data A data frame. Your empty units data frame.
#'
#' @return A data frame. Your filled units data frame. 
#' @export
#'
#' @examples
#' fill_units_data(site_template, conversion_file, units_data_frame)
fill_units_data <- function(site_template, conversion, units_data) {
  filename <- tools::file_path_sans_ext(basename(site_template))
  units_row <- cbind(filename, t(conversion$Unit))
  colnames(units_row) <- colnames(units_data)
  units_data <- rbind(units_data, units_row)
  return(units_data)
}


#### MAIN ####
# ---------- Step 0. DOWNLOAD THE TEMPLATES ---------- #

#template_downloader(templates_on_drive, template_folder)

# List all the templates
xls_templates <- list.files(path = template_folder, pattern = "^[A-Z]*Site*", full.names = TRUE)
#xls_templates <- list.files(path = template_folder, pattern = "Site_Data_Template_V4_TIMB", full.names = TRUE)
xls_templates

for (i in 1:length(xls_templates)) {
  site_template <- xls_templates[i]
  cat(sprintf("Processing template %s", basename(site_template)), "\n")
  
  # ---------- Step 1. READ THE DATA ---------- #
 
  #site_template<-file.path(template_folder, "Site_Data_Template_V4_HBF")
  site_data <- read_the_data(site_template)
  
  # ---------- Step 2. CLEAN THE DATA ---------- #
  clean_data <- clean_the_data(site_data, site_template)
    
  # ---------- Step 3. BUILD THE UNIT CONVERSION TABLE ---------- #
  conversion_file <- join_the_data(LUT_file, site_template)
    
  # ---------- Step 4. CONVERT THE DATA ---------- #
  converted <- convert_the_data(conversion_file, clean_data)
    
  # ---------- Step 5. Export as .csv file ------- #
  make_csv(converted, site_template, output_path)
  
}

#   ---------- Step 6. CREATE UNITS DATA FRAME -------- #
  if (i == 1){
    units_data_frame <- create_units_data(conversion_file) # DO THIS JUST ONCE, NOT EVERY TIME
    full_units_data <- fill_units_data(site_template, conversion_file, units_data_frame)
  }

   # ---------- Step 7. FILL UNITS DATA FRAME ------- #
   full_units_data <- fill_units_data(site_template, conversion_file, full_units_data)
# }

## Write csv for all units dataframe outside of loop
write.csv(full_units_data, units_path, row.names = FALSE, fileEncoding = "Latin1", quote = TRUE)


###test code for step 6. Removed full_units_data from function.
# ## Only needed when building the units summary
#   #---------- Step 6. CREATE UNITS DATA FRAME -------- #
#   if (i == 1){
#     units_data_frame <- create_units_data(conversion_file) # DO THIS JUST ONCE, NOT EVERY TIME
#   }
#   # ---------- Step 7. FILL UNITS DATA FRAME ------- #
#   full_units_data <- fill_units_data(site_template, conversion_file, full_units_data)



#### TESTER #####
# vector1 <- c("05/05/18",
#             "06/23/18","05-06-2018",
#             "08-25-2018")
# vector1
# 
# vector1 <- parse_date_time(x = vector1,
#                 orders = c("m/d/y","y-m-d"),
#                 tz="GMT", locale = Sys.getlocale("LC_TIME")
#                 )
# vector1 <- strftime(vector1, format = "%m/%d/%Y")
# # 
# # ###
# # data$`Sampling Date` <- parse_date_time(x=`Sampling Date`,
# #                                         orders = c("m/d/y","y-m-d"),
# #                                         tz="GMT", locale = Sys.getlocale("LC_TIME"))
# # data$`Sampling Date` <- strftime(data$`Sampling Date`, format = "%m/%d/%Y")
# # 
# # ###
# # vector1
# # 
# # vector1 <- as.Date(vector1, format = "%m/%d/%Y")
# # vector1 <- dmy(vector1)
# # vector1 
# 
# site_data1<-sample_n(site_data, 10)
# site_data1
