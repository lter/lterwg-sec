library(Hmisc)
library(tidyr)
library(lubridate)


### CONSTANT ----
# Filename
input_file <- "NCEAS_merge_10MAY17_Raw.csv"
fixed_file <- "NCEAS_typefixed_10MAY17_Raw.csv"

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
sec_df <- select(sec_df, -c(DOC_NO3, DOC_DON, DOC_PO4, DIN_PO4))

# write temp file and read it back in to fix the numerical fields
write.csv(sec_df, fixed_file, row.names = TRUE, fileEncoding = "UTF-8", quote = TRUE)

# Read the new file in
sec_dfc <- read.csv(fixed_file, stringsAsFactors = FALSE)

# Check column types
str(sec_dfc)

# Adding a new column with the date into date format
sec_dfc$DateType <- dmy(sec_df$Date)


