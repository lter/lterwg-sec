##########################################################################################
### This script aggregated the harmonized templates for the different study sites that ###
### were selected by the LTER working group Stream Elementary Stream elemental cycling ###
##########################################################################################

### Authors: Celine Mol and Julien Brun, NCEAS, UCSB
### Email: SciComp@nceas.ucsb.edu


#### LIBRARIES ####

library(tidyverse)


#### CONSTANTS ####

# define path if not using RStudio project relative to the repository
# setwd("/Users/celine/Desktop") 
today <- Sys.Date()
data_path <- "Templates_updated_26OCT2017/csv_conversions"
aggregated_file <- paste0("AGGREGATE_Templates_", today, ".csv")
bounded_file <- paste0("INBOUNDS_AGGREGATE_Templates_",today,".csv")


#### FUNCTIONS ####

aggregator <- function(csv_list) {
  for (i in seq_along(csv_list)){
    # read the converted site csv
    current_file <- read_csv(csv_list[i], 
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
    # Handle the first file case
    if (i > 1) {
      if (ncol(current_file) <= 35) { # remove extra columns like location
        all_csvs <-  rbind(all_csvs, current_file)
        } else {
        cat(sprintf("this template %s has more than the expected 35 columns", basename(csv_list[i])),"\n")
        all_csvs <-  rbind(all_csvs, current_file[,1:35]) 
        }
      } else {
      all_csvs <-  current_file
    }
  }
  # Set fields names
  colnames(all_csvs) <- c("LTER", "Site_Stream_Name", "Sampling_Date", "Time",
                            "Land_Use", "Treatment", "Q_Discharge", "alkalinity",
                            "ANC", "pH", "Temp_C", "DIC", "DO_mg_L", "DO_percent", "Conductivity",
                            "Spec_Cond", "DOC", "TOC", "TDN", "TN", "DON", "NO3", "NH4",
                            "TKN", "PO4", "SRP", "Si", "TDP", "Na", "K", "Ca", "Mg", "SO4", "Cl", "DOP")

  return(all_csvs)
}


#### MAIN ####

# ---------- Step 1. AGGREGATE THE DATA ---------- #

# List the csv files to aggregate
csv_files <- list.files(path = data_path, pattern = "Site_Data_Template", full.names = TRUE)
csv_files

# Aggregate the converted files
all_sites <- aggregator(csv_files)

# Write the consolidated data set
write.csv(all_sites, aggregated_file, row.names = FALSE, fileEncoding = "UTF-8", quote = TRUE)


# ---------- Step 2. COMPUTE SUMMARY STATS ---------- #

# MEAN 
site_means <- all_sites %>% 
  group_by(LTER) %>% 
  summarise_if(is.numeric, mean, na.rm = TRUE)

# MIN 
site_mins <- all_sites %>% 
  group_by(LTER) %>% 
  summarise_if(is.numeric, min, na.rm = TRUE)
# Remove the infinity values created fo only NAs
is.na(site_mins) <- sapply(site_mins,is.infinite)
  
# MAX 
site_maxs <- all_sites %>% 
  group_by(LTER) %>% 
  summarise_if(is.numeric, max, na.rm = TRUE)
# Remove the infinity values created fo only NAs
is.na(site_maxs) <- sapply(site_maxs,is.infinite)

## OUTPUT SUMMARY STATISTICS

write.csv(site_means, "MEAN_Summary_Statistics.csv", row.names = FALSE, fileEncoding = "UTF-8", quote = TRUE)
write.csv(site_mins, "MIN_Summary_Statistics.csv", row.names = FALSE, fileEncoding = "UTF-8", quote = TRUE)
write.csv(site_maxs, "MAX_Summary_Statistics.csv", row.names = FALSE, fileEncoding = "UTF-8", quote = TRUE)


# ---------- Step 3. BOUND DATA IN A PLAUSIBLE RANGE ---------- #

# fix PH range
all_sites$pH <- ifelse(all_sites$pH >= 4 & all_sites$pH <= 9,  all_sites$pH, NA)

# fix DO range
all_sites$DO_mg_L <- ifelse(all_sites$DO_mg_L >=13,all_sites$DO_mg_L, NA)

# fix Conductivity range
all_sites$Conductivity <- ifelse(all_sites$Conductivity >= 10, all_sites$Conductivity, NA)

# fix specific conductance range
all_sites$Spec_Cond <- ifelse(all_sites$Spec_Cond >= 10, all_sites$Spec_Cond, NA)

# fix specific DOC range
all_sites$DOC <- ifelse(all_sites$DOC >= 0.1 & all_sites$DOC <= 150, all_sites$DOC, NA)

# fix specific TDN range
all_sites$TDN <- ifelse(all_sites$TDN >= 0.05 & all_sites$TDN <= 25, all_sites$TDN, NA)

# fix specific DON range
all_sites$DON <- ifelse(all_sites$DON >= 0, all_sites$DON, NA)

# fix specific NO3 range
all_sites$NO3 <- ifelse(all_sites$NO3 >= 0.01 & all_sites$NO3 <= 17, all_sites$NO3, NA)

# fix specific NH4 range
all_sites$NH4 <- ifelse(all_sites$NH4 >= 0.05, all_sites$NH4, NA)

# fix specific PO4P range
all_sites$PO4 <- ifelse(all_sites$PO4 >= 0.02 & all_sites$PO4 <= 2, all_sites$PO4, NA)

# fix specific SRP range
all_sites$SRP <- ifelse(all_sites$SRP >= 0.02 & all_sites$SRP <= 2, all_sites$SRP, NA)

# fix specific TDP range
all_sites$TDP <- ifelse(all_sites$TDP >= 0.02 & all_sites$TDP <= 10, all_sites$TDP, NA)

# fix specific DOP range
all_sites$DOP <- ifelse(all_sites$DOP >= 0.02 & all_sites$DOP <= 10, all_sites$DOP, NA)

# fix specific TPO4 range
#all_sites$TPO4 <- ifelse(all_sites$TPO4 > 0.02 | all_sites$TPO4 < 10, all_sites$TPO4, NA)

# fix specific POC range
#all_sites$POC <- ifelse(all_sites$POC > 0.02 | all_sites$POC < 10, all_sites$POC, NA)

# fix specific range
for (measurement in c("Si", "Na", "Ca", "Mg", "SO4", "Cl")){ # "Fe", "Sr", "Al" 
  all_sites[, measurement] <- ifelse(all_sites[, measurement][[1]] > 0.1, all_sites[, measurement][[1]], NA)
}

# fix specific K range
all_sites$K <- ifelse(all_sites$K > 0.1 & all_sites$K < 30, all_sites$K, NA)


write.csv(all_sites, bounded_file, row.names = FALSE, fileEncoding = "UTF-8", quote = TRUE)









