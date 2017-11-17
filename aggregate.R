
# ----- LIBRARIES ----- #
library(readr)
library(tidyverse)

setwd("/Users/celine/Desktop")

# ----- CONSTANTS ----- #
data_path <- "Templates_updated_26OCT2017/csv_conversions"
output_file <- "AGGREGATE_Templates_11-16-17.csv"
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

dataset <- read.csv(bounded_file) #output_file
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

write.csv(means, "INBOUNDS_MEAN_Summary_Statistics.csv", row.names = FALSE, fileEncoding = "UTF-8", quote = TRUE)
write.csv(mins, "MIN_Summary_Statistics.csv", row.names = FALSE, fileEncoding = "UTF-8", quote = TRUE)
write.csv(maxs, "MAX_Summary_Statistics.csv", row.names = FALSE, fileEncoding = "UTF-8", quote = TRUE)

#-------------------------------------------------------------------------------------------------------------------------

# ----- BOUND DATA IN A PLAUSIBLE RANGE ----- #

bounded_file <- "INBOUNDS_AGGREGATE_Templates_11-16-17.csv"

# fix PH range
dataset$pH <- ifelse(dataset$pH >= 4 & dataset$pH <= 9,  dataset$pH, NA)

# fix DO range
dataset$DO_mg_L <- ifelse(dataset$DO_mg_L >=13,dataset$DO_mg_L, NA)

# fix Conductivity range
dataset$Conductivity <- ifelse(dataset$Conductivity >= 10, dataset$Conductivity, NA)

# fix specific conductance range
dataset$Spec_Cond <- ifelse(dataset$Spec_Cond >= 10, dataset$Spec_Cond, NA)

# fix specific DOC range
dataset$DOC <- ifelse(dataset$DOC >= 0.1 & dataset$DOC <= 150, dataset$DOC, NA)

# fix specific TDN range
dataset$TDN <- ifelse(dataset$TDN > 0.05 & dataset$TDN < 25, dataset$TDN, NA)

# fix specific DON range
dataset$DON <- ifelse(dataset$DON >= 0, dataset$DON, NA)

# fix specific NO3 range
dataset$NO3 <- ifelse(dataset$NO3 >= 0.01 & dataset$NO3 <= 17, dataset$NO3, NA)

# fix specific NH4 range
dataset$NH4 <- ifelse(dataset$NH4 >= 0.05, dataset$NH4, NA)

# fix specific PO4P range
dataset$PO4 <- ifelse(dataset$PO4 > 0.02 & dataset$PO4 < 2, dataset$PO4, NA)

# fix specific SRP range
dataset$SRP <- ifelse(dataset$SRP > 0.02 & dataset$SRP < 2, dataset$SRP, NA)

# fix specific TDP range
dataset$TDP <- ifelse(dataset$TDP > 0.02 & dataset$TDP < 10, dataset$TDP, NA)

# fix specific DOP range
dataset$DOP <- ifelse(dataset$DOP > 0.02 & dataset$DOP < 10, dataset$DOP, NA)

# fix specific TPO4 range
#dataset$TPO4 <- ifelse(dataset$TPO4 > 0.02 | dataset$TPO4 < 10, dataset$TPO4, NA)

# fix specific POC range
#dataset$POC <- ifelse(dataset$POC > 0.02 | dataset$POC < 10, dataset$POC, NA)

# fix specific range
for (c in c("Si", "Na", "Ca", "Mg", "SO4", "Cl")){ # "Fe", "Sr", "Al" 
  dataset[, c] <- ifelse(dataset[, c] > 0.1, dataset[, c], NA)
}

# fix specific K range
dataset$K <- ifelse(dataset$K > 0.1 & dataset$K < 30, dataset$K, NA)


write.csv(dataset, bounded_file, row.names = FALSE, fileEncoding = "UTF-8", quote = TRUE)









