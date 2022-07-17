library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(reshape2)


data<-read_csv(
  "https://data.lacity.org/resource/2nrs-mtv8.csv?$limit=523509&$offset=100")

#Check table structure
str(data)

#___________________________ DATA CLEAN
# combine date and time occurred as a new column
data <- data %>%
  unite("datetime_occ",
        sep = " ",
        date_occ:time_occ,na.rm = TRUE,
        remove = FALSE)

#format it to POSIXct
data$datetime_occ <- ymd_hm(data$datetime_occ)



# Rank Crime In LA (by year)

# Comparison of crime from 2020-to present

# Crime Female vs Male

# Age group most affected by crime 

# Months affected by crime most

# what time does crime happen most.

# What area has the most crime

# 