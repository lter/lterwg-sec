library(Hmisc)
library(tidyr)


# Filename
data_file <- "NCEAS_merge_10MAY17_Raw.csv"
# read file in 
sec_df <- read.csv(data_file, stringsAsFactors = FALSE, fileEncoding="latin1") # csv seems to have encoding issues with the default locale


# Summary
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

# Remove ratios
sec_df <- select(sec_df, -c(DOC_NO3, DOC_DON, DOC_PO4, DIN_PO4))


# write temp file and read it back in to fix the numerical fields
write.csv(sec_df,"test.csv", row.names = TRUE, fileEncoding = "UTF-8", quote = TRUE)
sec_dfc <- read.csv("test.csv", stringsAsFactors = FALSE)


