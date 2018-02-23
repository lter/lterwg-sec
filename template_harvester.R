#################################################################################################
### This script harvests the templates created for the different LTER and international sites ###
###           that were selected by the LTER working group Stream Elemental Cycling           ###
#################################################################################################

### Authors: Celine Mol, Margaux Sleckman and Julien Brun, NCEAS, UCSB
### Email: SciComp@nceas.ucsb.edu


#### LIBRARIES ####

library(dplyr)
library(readxl)
library(tidyverse)
library(stringr)
library(lubridate)

#### CONSTANTS ####

# define path if not using RStudio project relative to the repository
# setwd("/Users/celine/Desktop") 

# Set the relative path to directory containing the templates and LUT
# Assuming the top folder for the data is named "Templates_updated_26OCT2017", 
# similar as on the working group Goolgle Drive

template_folder <- "Templates_201802" 
output_path <- file.path(template_folder, "csv_conversions")
# Set the relative path to all units file
units_path <- file.path(template_folder, "LTER_units.csv")
# test if the directory exists
dir.create(output_path, showWarnings = FALSE)

# Filename to LUT
LUT_file <- file.path(template_folder, "Conversions.xlsx")


#### FUNCTIONS ####

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
  
  # Three templates are slightly different
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
                                          "numeric", "numeric", "numeric", "blank"))
    
  }
  if (str_detect(xls_file, "UK")) {
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
                                          "numeric", "numeric", "numeric"))
  # date in this template is set to date. 
    
    
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
                                          "numeric", "numeric","numeric"))
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
                                          "numeric", "numeric","numeric"))
    
    # date in this template is set to Text. Otherwise, conversion turns dates into numerals. 
    
  }
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
  
  # Check if Sampling Date, Time are in standard format
  
  #### For V4_Fin
  if (str_detect(file, "Fin")) { # class(data$`Sampling Date`)[1] != "POSIXct"
    data$`Sampling Date` <- gsub("[.]","-", data$`Sampling Date`)
    data$`Sampling Date` <- dmy(data$`Sampling Date`)
  }
  ### For V4_WBR
  
  #if (str_detect(file, "WBR")){
  #  data$`Sampling Date`<-mdy(data$`Sampling Date`)
  #}
  
  ### Specific to V4_AND, ALSO ASSUMING cm == cms
  if (str_detect(file, "AND")) {
    #units[[1,2]] <- "cms"
    data$Time <- strftime(data$Time, format = "%H:%M:%S", tz = "GMT") 
  }
  
  ### Specific to ARC_GRO, ASSUMING Alkalinity mg/L == mg HCO3/L
  
  ### Specific to ARC_PAR, ASSUMING Alkalinity mg/L == mg HCO3/L
  if (str_detect(file, "ARC_PAR")) {
    data$`Site/Stream Name` <- str_split(data$`Site/Stream Name`, " ", simplify = TRUE)[,1]
    #units[[2,2]] <- "mg HCO3/L"
  }
  
  ### Specific to HBF
  if (str_detect(file, "HBF")) {
    data$Time <- strftime(data$Time, format = "%H:%M:%S", tz = "GMT") 
  }
  
  ### Specific to LMP
  if (str_detect(file, "LMP")) {
    names(data)[13] <- "DO mg/L"
  }
  
  ## Specific to LUQ
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
  
  ## Specific to NIW
  if(str_detect(file, "NIW")) {
    data$`Sampling Date` <- ymd(data$`Sampling Date`)
    data$Time <- as.character(data$Time)
  }
  
  ## Specific to UK
  if(str_detect(file, "UK")) {
    data$`Sampling Date` <- as.Date(data$`Sampling Date`, format = "%m/%d/%Y")
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

# List all the templates
xls_templates <- list.files(path = template_folder, pattern = "Site_Data", full.names = TRUE)
#xls_templates <- list.files(path = template_folder, pattern = "Site_Data_Template_V4_WBR", full.names = TRUE)
xls_templates

for (i in 1:length(xls_templates)) {
  site_template <- xls_templates[i]
  cat(sprintf("Processing template %s", basename(site_template)), "\n")
  
  # ---------- Step 1. READ THE DATA ---------- #
 
  #site_template<-file.path(template_folder, "Site_Data_Template_V4_KBS2")
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

<<<<<<< HEAD
#   ---------- Step 6. CREATE UNITS DATA FRAME -------- #
  if (i == 1){
    units_data_frame <- create_units_data(conversion_file) # DO THIS JUST ONCE, NOT EVERY TIME
     full_units_data <- fill_units_data(site_template, conversion_file, units_data_frame)
   }
   # ---------- Step 7. FILL UNITS DATA FRAME ------- #
   full_units_data <- fill_units_data(site_template, conversion_file, full_units_data)


## Write csv for all units dataframe outside of loop
 write.csv(full_units_data, units_path, row.names = FALSE, fileEncoding = "Latin1", quote = TRUE)
=======
## Only needed when building the units summary
#   #---------- Step 6. CREATE UNITS DATA FRAME -------- #
#   if (i == 1){
#     units_data_frame <- create_units_data(conversion_file) # DO THIS JUST ONCE, NOT EVERY TIME
#   }
#   # ---------- Step 7. FILL UNITS DATA FRAME ------- #
#   full_units_data <- fill_units_data(site_template, conversion_file, full_units_data)
# 
# }
# 
# ## Write csv for all units dataframe outside of loop
# write.csv(full_units_data, units_path, row.names = FALSE, fileEncoding = "Latin1", quote = TRUE)
>>>>>>> e1da4144e31b60d89689acb7bb025dfe8af66db4



