library(readr)
library(tidyverse)

setwd("/Users/celine/Desktop/Templates_updated_26OCT2017/csv_conversions")
files <- list.files("/Users/celine/Desktop/Templates_updated_26OCT2017/csv_conversions")

# ----- CONVERT ----- #

first_file <- read_csv(files[1], 
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

for (i in 2:length(files)) {
  current_file <- read_csv(files[i], 
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

# ----- CHECK IF CORRECTLY CONVERTED ----- #

dataset <- first_file


#for (i in 7:length(names(dataset))) {
#  aggregate(dataset[,i] ~ LTER, data = first_file, FUN=mean)
#}
aggregate(NO3 ~ LTER, data = dataset, FUN=mean)
## ISSUES WITH FIN, LMP, TROP - check conversions














