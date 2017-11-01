## Code to implement QA/QC checks and fixes on the main data set of the LTER SEC working group

library(Hmisc)
library(tidyr)
library(lubridate)
library(googledrive)


### CONSTANT ----
# Filename
input_file <- "NCEAS_merge_10MAY17_Raw.csv"
fixed_file <- "NCEAS_typefixed_10MAY17_Raw.csv"
cleaned_file <- "NCEAS_CLEANED_25OCT17_Raw.csv"

### MAIN ----

# read file in 
sec_df <- read.csv(input_file, stringsAsFactors = FALSE, fileEncoding="latin1") # csv seems to have encoding issues with the default locale

# Summary of content
describe(sec_df)

## Fixing lat-lon parsing ----

# Needed for NCEAS_merge.csv
## Copying PON data to the right column
# sec_df$PON <- sec_df$Longitude
# 
# # Removing FALSE longitude column
# sec_df$Longitude <- NULL
# 
# # split Latitude column into Latitude and Longitude
# sec_df <- separate(sec_df, Latitude, c("Latitude", "Longitude"), sep = ",") 

# remove junk columns from parsing error
# sec_df <- select(sec_df, -c(68:74))

## Fixing "lost" strings----

sec_df$Site <- iconv(sec_dfc$Site, to='ASCII//TRANSLIT')

# Remove #VALUE! ----
sec_df <- as.data.frame(lapply(sec_df, function(x) gsub("#VALUE!", NA, x)), stringsAsFactors = FALSE)

# Remove #DIV/0! ----
sec_df <- as.data.frame(lapply(sec_df, function(x) gsub("#DIV/0!", NA, x)), stringsAsFactors = FALSE)

# Remove POC from the POC column
sec_df$POC <- gsub("POC", NA, sec_df$POC)

# Remove >50 from the SO4S column
sec_df$SO4S <- gsub(">50", NA, sec_df$SO4S)

# Clean pH column
sec_df$pH <- gsub("QNS", NA, sec_df$pH)

# Clean DOC column
sec_df$DOC <- gsub("", NA, sec_df$SO4S)

# Clean NO3N column
# find all the values that are not numeric
grep("[a-z]", sec_df$NO3N, value=TRUE)

# since the list is long, we use another (bulk) approach to fix this
sec_df$NO3N <- as.numeric(as.character(sec_df$NO3N))

# remove row that seems to be a repetition of the header
sec_df <- sec_df[-c(87612), ] # to be improved as row position dependant

# Remove ratios that were computed in Excel to recompute them in R
sec_df <- dplyr::select(sec_df, -c(DOC_NO3, DOC_DON, DOC_PO4, DIN_PO4))

# write temp file and read it back in to fix the numerical fields
write.csv(sec_df, fixed_file, row.names = FALSE, fileEncoding = "UTF-8", quote = TRUE)

## Read in the cleaned dataset ----
# Read the new file in
sec_dfc <- read.csv(fixed_file, stringsAsFactors = FALSE)
rm(sec_df)

# Check column types
str(sec_dfc)

## Bound the values in plausible range ----
# Adding a new column with the date into date format
sec_dfc$DateType <- dmy(sec_df$Date)

# Remove -9999 and set it to NA
sec_dfc <- as.data.frame(lapply(sec_dfc, function(x) gsub(-9999, NA, x)), stringsAsFactors = FALSE)

# fix PH range
sec_dfc$pH <- ifelse(sec_dfc$pH > 4 | sec_dfc$pH < 9,  sec_dfc$pH, NA)

# fix DO range
sec_dfc$DO_mgL <- ifelse(sec_dfc$DO_mgL >=13,sec_dfc$DO_mgL, NA)

# fix Conductivity range
sec_dfc$Conductivity <- ifelse(sec_dfc$Conductivity >= 10, sec_dfc$Conductivity, NA)

# fix specific conductance range
sec_dfc$SpCond <- ifelse(sec_dfc$SpCond >= 10, sec_dfc$SpCond, NA)

# fix specific DOC range
sec_dfc$DOC <- ifelse(sec_dfc$DOC > 0.1 | sec_dfc$DOC < 150, sec_dfc$DOC, NA)

# fix specific TDN range
sec_dfc$TDN <- ifelse(sec_dfc$TDN > 0.05 | sec_dfc$TDN < 25, sec_dfc$TDN, NA)

# fix specific DON range
sec_dfc$DON <- ifelse(sec_dfc$DON >= 0, sec_dfc$DON, NA)

# fix specific NO3N range
sec_dfc$NO3N <- ifelse(sec_dfc$NO3N > 0.01 | sec_dfc$NO3N < 17, sec_dfc$NO3N, NA)

# fix specific NH4 range
sec_dfc$NH4N <- ifelse(sec_dfc$NH4N >= 0.05, sec_dfc$NH4N, NA)

# fix specific PO4P range
sec_dfc$PO4P <- ifelse(sec_dfc$PO4P > 0.02 | sec_dfc$PO4P < 2, sec_dfc$PO4P, NA)

# fix specific SRP range
sec_dfc$SRP <- ifelse(sec_dfc$SRP > 0.02 | sec_dfc$SRP < 2, sec_dfc$SRP, NA)

# fix specific TDP range
sec_dfc$TDP <- ifelse(sec_dfc$TDP > 0.02 | sec_dfc$TDP < 10, sec_dfc$TDP, NA)

# fix specific DOP range
sec_dfc$DOP <- ifelse(sec_dfc$DOP > 0.02 | sec_dfc$DOP < 10, sec_dfc$DOP, NA)

# fix specific TPO4 range
sec_dfc$TPO4 <- ifelse(sec_dfc$TPO4 > 0.02 | sec_dfc$TPO4 < 10, sec_dfc$TPO4, NA)

# fix specific POC range
sec_dfc$POC <- ifelse(sec_dfc$POC > 0.02 | sec_dfc$POC < 10, sec_dfc$POC, NA)

# fix specific range
for (c in c("Si", "Na", "Ca", "Mg", "SO4S", "Cl", "Fe", "Sr", "Al")){
  sec_dfc[, c] <- ifelse(sec_dfc[, c] > 0.1, sec_dfc[, c], NA)
}

# fix specific K range
sec_dfc$K <- ifelse(sec_dfc$K > 0.1 | sec_dfc$K < 30, sec_dfc$K, NA)

# # Thanks! https://stackoverflow.com/questions/20495598/replace-accented-characters-in-r-with-non-accented-counterpart-utf-8-encoding
# unwanted_array = list(    'Š'='S', 'š'='s', 'Ž'='Z', 'ž'='z', 'À'='A', 'Á'='A', 'Â'='A', 'Ã'='A', 'Ä'='A', 'Å'='A', 'Æ'='A', 'Ç'='C', 'È'='E', 'É'='E',
#                           'Ê'='E', 'Ë'='E', 'Ì'='I', 'Í'='I', 'Î'='I', 'Ï'='I', 'Ñ'='N', 'Ò'='O', 'Ó'='O', 'Ô'='O', 'Õ'='O', 'Ö'='O', 'Ø'='O', 'Ù'='U',
#                           'Ú'='U', 'Û'='U', 'Ü'='U', 'Ý'='Y', 'Þ'='B', 'ß'='Ss', 'à'='a', 'á'='a', 'â'='a', 'ã'='a', 'ä'='a', 'å'='a', 'æ'='a', 'ç'='c',
#                           'è'='e', 'é'='e', 'ê'='e', 'ë'='e', 'ì'='i', 'í'='i', 'î'='i', 'ï'='i', 'ð'='o', 'ñ'='n', 'ò'='o', 'ó'='o', 'ô'='o', 'õ'='o',
#                           'ö'='o', 'ø'='o', 'ù'='u', 'ú'='u', 'û'='u', 'ý'='y', 'ý'='y', 'þ'='b', 'ÿ'='y')

# write final file and read it back in to fix the numerical fields
write.csv(sec_dfc, cleaned_file, row.names = FALSE, fileEncoding = "UTF-8", quote = TRUE)
