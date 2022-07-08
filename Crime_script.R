library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)

# Get data
data <- read.csv("Crime_Data_from_2020_to_Present.csv")

#Check table structure
str(data)

#___________________________ DATA CLEAN

# change all column names to lowercase.
names(data) <- tolower(names(data))

# rename columns
data <- rename(data, date_reported = date.rptd) # Stop
data <- rename(data, date_occured = date.occ)
data <- rename(data, time_occured = time.occ)
data <- rename(data, area_name = area.name)
data <- rename(data, crime_cd = crm.cd)
data <- rename(data, crime_desc = crm.cd.desc)
data <- rename(data, vict_age = vict.age)
data <- rename(data, vict_sex = vict.sex)
data <- rename(data, vict_descent = vict.descent)
data <- rename(data, premis_cd = premis.cd)
data <- rename(data, premis_desc = premis.desc)
data <- rename(data, weapon_used_cd = weapon.used.cd)
data <- rename(data, weapon_desc = weapon.desc)
data <- rename(data, cross_street = cross.street)
str(data)

# Check how many missing values are there
sum(is.na(data)) #310,620 null values


# Date_reported and Date_occured are all reported  
# at midnight seems to report at the exact date. so it is safe to remove.

# remove 12:00:00 AM from both columns
data$date_reported <- gsub("0:00","",as.character(data$date_reported))
data$date_occured <- gsub("0:00","",as.character(data$date_occured))

# trim white space
data$date_reported <- trimws(data$date_reported)
data$date_occured <- trimws(data$date_occured)

# parse columns from char to Date datatype
data$date_reported <-mdy(data$date_reported)
data$date_occured <- mdy(data$date_occured)

# USED Excel to clean Time. from military to standard time.

# char-time Parsed to time 
data$time_occured <- hms::as_hms(data$time_occured)

# I want to change victim descent from abbreviation to the whole word.
# Could also use dplyr::case_when
data$vict_descent[data$vict_descent == "A"] <- "Asian"
data$vict_descent[data$vict_descent == "B"] <- "Black"
data$vict_descent[data$vict_descent == "C"] <- "Chineese"
data$vict_descent[data$vict_descent == "D"] <- "Cambodian"
data$vict_descent[data$vict_descent == "F"] <- "Filipino"
data$vict_descent[data$vict_descent == "G"] <- "Guamaniam"
data$vict_descent[data$vict_descent == "H"] <- "Hispanic/Latin/Mexican"
data$vict_descent[data$vict_descent == "I"] <- "American Indian"
data$vict_descent[data$vict_descent == "J"] <- "Japanese"
data$vict_descent[data$vict_descent == "K"] <- "Korean"
data$vict_descent[data$vict_descent == "L"] <- "Laotian"
data$vict_descent[data$vict_descent == "O"] <- "Other"
data$vict_descent[data$vict_descent == "P"] <- "Pacific Islander"
data$vict_descent[data$vict_descent == "S"] <- "Samoan"
data$vict_descent[data$vict_descent == "U"] <- "Hawaiian"
data$vict_descent[data$vict_descent == "V"] <- "Vietnamese"
data$vict_descent[data$vict_descent == "W"] <- "White"
data$vict_descent[data$vict_descent == "X"] <- "Unkown"
data$vict_descent[data$vict_descent == "Z"] <- "Asian Indian"


min(data$vict_age) # -1 ??? what does that mean
max(data$vict_age) # 120 years old.

# Check if ages -1:1 years are victim ages or unknowns
data %>% 
  filter(between(vict_age,-1,1)) %>% 
  arrange(desc(vict_age))
# There is no 1 year old victims, 0 years old are not victims -1 are unknowns

#Create an age group and a development stage. helps with the analysis.
data <- data %>%
  mutate(
    dev_stage = dplyr::case_when(
      vict_age <= 1 ~ "Unkown",
      vict_age > 1 & vict_age <=4 ~ "Toddler",
      vict_age > 4 & vict_age <=12 ~ "Child",
      vict_age > 12 & vict_age <=19 ~ "Teen",
      vict_age > 19 & vict_age <=39 ~ "Adult",
      vict_age > 39 & vict_age <=59 ~ "Middle Age",
      vict_age > 59  ~ "Senior"
    )
  )

data <- data %>%
  mutate(
    age_group = dplyr::case_when(
      vict_age <= 1 ~ "Unkown",
      vict_age > 1 & vict_age <=4 ~ "2-4",
      vict_age > 4 & vict_age <=12 ~ "5-12",
      vict_age > 12 & vict_age <=19 ~ "13-19",
      vict_age > 19 & vict_age <=39 ~ "20-39",
      vict_age > 39 & vict_age <=59 ~ "40-59",
      vict_age > 59  ~ "60+"
    )
  )

#________________________
#__________________ EDA ____________________________________________
 # Looking through crime data from Jan 01-2020 to Dec-31-2021

#Crime most committed in LA between 2020-2022
data %>% 
  select(crime_desc, date_occured) %>% 
  filter(between(date_occured,'2020-01-01','2021-12-31')) %>% 
  group_by(crime_desc) %>% 
  count(crime_desc) %>% 
  arrange(desc(n))

# Top 10 Most common crimes in LA
# 1 VEHICLE - STOLEN                                        - 44366
# 2 BATTERY - SIMPLE ASSAULT                                - 32477
# 3 VANDALISM - FELONY ($400 & OVER, ALL CHURCH VANDALISMS) - 26564
# 4 BURGLARY FROM VEHICLE                                   - 26158
# 5 BURGLARY                                                - 24999
# 6 ASSAULT WITH DEADLY WEAPON, AGGRAVATED ASSAULT          - 24245
# 7 THEFT PLAIN - PETTY ($950 & UNDER)                      - 21958
# 8 INTIMATE PARTNER - SIMPLE ASSAULT                       - 21566
# 9 THEFT OF IDENTITY                                       - 18653
# 10 THEFT FROM MOTOR VEHICLE - PETTY ($950 & UNDER)        - 17951
#______________

# So i can save time I will make a new data frame 
# with the years between 2020-2021
eda_data <- data %>% 
  filter(between(date_occured,'2020-01-01','2021-12-31'))


 # Stolen Vehicle 
                    # what we need to find out
# How has vehicle theft changed through the years. -
# What months does it happen the most -
# What days of the week does this happen. -
# What time in the day does this happen.
# What areas are affected the most (area name)
# Where does this happen (premis_desc)
# "maybe" what streets are affected the most.
# How can we fix this.


# vehicle stolen by date Through out the years.
eda_data %>% 
  select(crime_desc,date_occured) %>% 
  filter(crime_desc == "VEHICLE - STOLEN") %>% 
  group_by(date_occured,year=factor(year(date_occured))) %>% 
  count(crime_desc) %>% 
  ggplot(aes(x=date_occured,y=n)) +
  geom_point(aes(color=year),size=3) + geom_smooth() +
  scale_color_manual(values = c("#0F4C5C","#FB8B24")) +
  scale_x_date(date_breaks = "1 month", date_labels = "%B") +
  labs(title = "Vehicle Theft Through The Years",
       subtitle="Vehicle Theft Case Count Through The Years",
       x="Months", y="Vehicle Theft Cases",
       color="Year",
       caption="Data Provided by Los Angeles Police Department") +
  theme_minimal()
#_________________

# What months do vehicles get stolen the most.
eda_data %>% 
  select(crime_desc,date_occured) %>% 
  filter(crime_desc == "VEHICLE - STOLEN") %>% 
  group_by(month=factor(month(date_occured,label=TRUE,abbr=TRUE)),
           year=factor(year(date_occured))) %>% 
  count(crime_desc) %>% 
  ggplot(aes(x=month,
             y=n,
             fill=year)) +
  geom_bar(stat="identity",position = "dodge") +
  geom_text(aes(label=n),position=position_dodge(1),vjust=-0.5) +
  scale_fill_manual(values = c("#0F4C5C","#FB8B24")) +
  labs(title = "Vehicle Theft By Months (2020-2021)",
       subtitle="Vehicle Theft Cases Count By Month",
       x="Months", y="Vehicle Theft Cases",
       fill="Year",
       caption="Data Provided by Los Angeles Police Department")
#___________________

# What days of the week does this happen. #dow-(dayOfWeek)
eda_data %>% 
  select(crime_desc,date_occured) %>% 
  filter(crime_desc == "VEHICLE - STOLEN") %>% 
  group_by(dow=factor(wday(date_occured,label=TRUE,abbr=TRUE)),
                      year=factor(year(date_occured))) %>%
  count(crime_desc) %>% 
  ggplot(aes(x=dow,
             y=n,
             fill=year)) +
  geom_bar(stat="identity",position = "dodge") +
  geom_text(aes(label=n),position=position_dodge(1),vjust=-0.5) +
  scale_fill_manual(values = c("#0F4C5C","#FB8B24")) +
  labs(title = "Vehicle Theft By Day Of The Week (2020-2021)",
       subtitle="Vehicle Theft Cases Count By Day Of The Week",
       x="Months", y="Vehicle Theft Cases",
       fill="Year",
       caption="Data Provided by Los Angeles Police Department")
#________________________

# What time of day does vehicle theft happen most.
eda_data %>% 
  select(crime_desc,time_occured) %>% 
  filter(crime_desc == "VEHICLE - STOLEN") %>% 
  group_by(time_occured) %>% 
  count(crime_desc) %>% 
  ggplot(aes(x=time_occured,y=n)) + geom_point() +
  scale_x_datetime(breaks = scales::date_breaks("30 mins"), date_labels = "%H:%M")


# Add a new column displaying time period of the day AM or PM 
eda_data <- eda_data %>% 
  mutate(
    time_period = dplyr::case_when(
      am(time_occured) == TRUE ~ "AM",
      pm(time_occured) == TRUE ~ "PM"
    )
  )













