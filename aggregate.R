
# ----- LIBRARIES ----- #
library(readr)
library(tidyverse)

setwd("/Users/celine/Desktop")

# ----- CONSTANTS ----- #
data_path <- "Templates_updated_26OCT2017/csv_conversions"
output_file <- "AGGREGATE_Templates_11-15-17.csv"
csv_files <- list.files(path = data_path, pattern = "Site_Data_Template", full.names = TRUE)
csv_files

# ----- CONVERT ----- #

first_file <- read_csv(csv_files[1], 
                         col_types = cols(LTER = col_character(),
                                          `Site/Stream Name` = col_character(),
                                          `Sampling Date` = col_character(), ##TEST
                                          Time = col_character(), ##TEST
                                          `Land Use` = col_character(),
                                          Treatment = col_character(),
                                          `Q (Discharge)` = col_double(),
                                          alkalinity = col_double(),
                                          ANC = col_double(),
                                          pH = col_double(),
                                          `Temp C` = col_double(),
                                          DIC = col_double(),
                                          `DO mg/L` = col_double(),
                                          `DO %` = col_double(),
                                          Conductivity = col_double(),
                                          `Spec Cond` = col_double(),
                                          DOC = col_double(),
                                          TOC = col_double(),
                                          TDN = col_double(),
                                          TN = col_double(),
                                          DON = col_double(),
                                          NO3 = col_double(),
                                          NH4 = col_double(),
                                          TKN = col_double(),
                                          PO4 = col_double(),
                                          SRP = col_double(),
                                          Si = col_double(),
                                          TDP = col_double(),
                                          Na = col_double(),
                                          K = col_double(),
                                          Ca = col_double(),
                                          Mg = col_double(),
                                          SO4 = col_double(),
                                          Cl = col_double(),
                                          DOP = col_double()))

for (i in 2:length(csv_files)) {
  current_file <- read_csv(csv_files[i], 
                         col_types = cols(LTER = col_character(),
                                          `Site/Stream Name` = col_character(),
                                          `Sampling Date` = col_character(), ##TEST
                                          Time = col_character(), ##TEST
                                          `Land Use` = col_character(),
                                          Treatment = col_character(),
                                          `Q (Discharge)` = col_double(),
                                          alkalinity = col_double(),
                                          ANC = col_double(),
                                          pH = col_double(),
                                          `Temp C` = col_double(),
                                          DIC = col_double(),
                                          `DO mg/L` = col_double(),
                                          `DO %` = col_double(),
                                          Conductivity = col_double(),
                                          `Spec Cond` = col_double(),
                                          DOC = col_double(),
                                          TOC = col_double(),
                                          TDN = col_double(),
                                          TN = col_double(),
                                          DON = col_double(),
                                          NO3 = col_double(),
                                          NH4 = col_double(),
                                          TKN = col_double(),
                                          PO4 = col_double(),
                                          SRP = col_double(),
                                          Si = col_double(),
                                          TDP = col_double(),
                                          Na = col_double(),
                                          K = col_double(),
                                          Ca = col_double(),
                                          Mg = col_double(),
                                          SO4 = col_double(),
                                          Cl = col_double(),
                                          DOP = col_double()))
  first_file <- rbind(first_file, current_file)
}

colnames(first_file) <- c("LTER", "Site_Stream_Name", "Sampling_Date", "Time", 
                          "Land_Use", "Treatment", "Q_Discharge", "alkalinity", 
                          "ANC", "pH", "Temp_C", "DIC", "DO_mg_L", "DO_percent", "Conductivity",
                          "Spec_Cond", "DOC", "TOC", "TDN", "TN", "DON", "NO3", "NH4", 
                          "TKN", "PO4", "SRP", "Si", "TDP", "Na", "K", "Ca", "Mg", "SO4", "Cl", "DOP")

# ----- OUTPUT ----- #

write.csv(first_file, output_file, row.names = FALSE, fileEncoding = "UTF-8", quote = TRUE)

#-------------------------------------------------------------------------------------------------------------------------

# ----- CHECK IF CORRECTLY CONVERTED ----- #

dataset <- read.csv("AGGREGATE_Templates_11-15-17.csv")
str(dataset)

# ----- MEAN ----- #

means <- data.frame(levels(dataset$LTER))
colnames(means)[1] <- "LTER"

for (i in 7:length(names(dataset))) {
  value <- aggregate(dataset[,i] ~ LTER, data = dataset, FUN=mean)
  value[[2]] <- round(value[[2]], digits = 2)
  colnames(value)[2] <- colnames(dataset[i])
  means <- left_join(means, value)
}

# ----- MIN ----- #

mins <- data.frame(levels(dataset$LTER))
colnames(mins)[1] <- "LTER"

for (i in 7:length(names(dataset))) {
  value <- aggregate(dataset[,i] ~ LTER, data = dataset, FUN=min)
  value[[2]] <- round(value[[2]], digits = 2)
  colnames(value)[2] <- colnames(dataset[i])
  mins <- left_join(mins, value)
}

# ----- MAX ----- #

maxs <- data.frame(levels(dataset$LTER))
colnames(maxs)[1] <- "LTER"

for (i in 7:length(names(dataset))) {
  value <- aggregate(dataset[,i] ~ LTER, data = dataset, FUN=max)
  value[[2]] <- round(value[[2]], digits = 2)
  colnames(value)[2] <- colnames(dataset[i])
  maxs <- left_join(maxs, value)
}

## OUTPUT SUMMARY STATISTICS


write.csv(means, "MEAN_Summary_Statistics.csv", row.names = FALSE, fileEncoding = "UTF-8", quote = TRUE)
write.csv(mins, "MIN_Summary_Statistics.csv", row.names = FALSE, fileEncoding = "UTF-8", quote = TRUE)
write.csv(maxs, "MAX_Summary_Statistics.csv", row.names = FALSE, fileEncoding = "UTF-8", quote = TRUE)
