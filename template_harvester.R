library(dplyr)
library(readxl)

xls_file <- "Site_Data_Template_V4_LUQ.xlsx"
LUT_file <- "Conversions.xlsx"


# Read the unit conversion LUT

LUT <- read_excel(LUT_file)

# Read the raw units
units <- read_excel(xls_file, sheet = "Solute Units") %>%
  select(1:2)
# names(units)[3] <- "Comment"

# Read the data in
data <- read_excel(xls_file, sheet = "Raw Data")


# LOOOOOOP

measurement <- units$Measurement[6]
raw_units <- units$Unit[6]




