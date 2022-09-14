library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(reshape2)
library(data.table)

################################## Original data ################################
#if you want to see original data table run this
original_data<-read_csv(
  "https://data.lacity.org/resource/2nrs-mtv8.csv?$limit=527897&$offset=100")

sql_data <- original_data %>% 
  filter(year(date_occ)<2022, vict_age > 1)

write.csv(sql_data, file = "LA_CrimeData.csv")
################################### End of OD ##################################


################################# Grab Data ##################################

# Grab Data
data<-read_csv(
  "https://data.lacity.org/resource/2nrs-mtv8.csv?$limit=527897&$offset=100")

#Check table structure
str(data)
############################## End of Grabbing Data ############################


################################# DATA CLEAN ###################################

# combine date and time occurred as a new column
data <- data %>%
  unite("datetime_occ",
        sep = " ",
        date_occ:time_occ,na.rm = TRUE,
        remove = FALSE)

#format it to POSIXct
data$datetime_occ <- ymd_hm(data$datetime_occ)


# I want to change victim descent from abbreviation to the whole word.
# Could also use dplyr::case_when
data$vict_descent[data$vict_descent == "A"] <- "Asian"
data$vict_descent[data$vict_descent == "B"] <- "Black"
data$vict_descent[data$vict_descent == "C"] <- "Chineese"
data$vict_descent[data$vict_descent == "D"] <- "Cambodian"
data$vict_descent[data$vict_descent == "F"] <- "Filipino"
data$vict_descent[data$vict_descent == "G"] <- "Guamaniam"
data$vict_descent[data$vict_descent == "H"] <- "Hispanic/Latin/Mexican"
# Removing mexican since they are Hispanic or Latin 
data$vict_descent[data$vict_descent == "Hispanic/Latin/Mexican"] <- "Hispanic/Latin"
#
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



########## ClEAN AGE ###############

min(data$vict_age) # -1 ??? what does that mean
max(data$vict_age) # 120 years old.

# Check if ages -1:1 years are victim ages or unknowns

#count how many cases with the victim age less than 1
data %>% 
  filter(vict_age < 1) %>% 
  group_by(vict_age) %>% 
  count(crm_cd_desc) %>% 
  summarise(total=sum(n))

# what crimes contain victim ages less than one
print(data %>% 
  filter(vict_age < 1) %>% 
  group_by(crm_cd_desc) %>% 
  count(vict_age) %>% 
    arrange(desc(vict_age)),n=138)
# Most of the crimes committed on victims less than 1 year of age are not possible
# Unless its a crime against a child violent or sexual.
# most crimes against victims younger than 1 are either an error but more possibly 
# a mark for Unknown.

# what crimes contain victim ages less than one, and are Male or female and 
# does not have an NA value in Vict_descent column
print(data %>% 
        filter(vict_age < 1,vict_sex %in% c("F","M"),  !is.na(vict_descent)) %>% 
        group_by(crm_cd_desc) %>% 
        count(vict_age) %>% 
        arrange(desc(vict_age)) %>% 
        summarise(total=sum(n)),n=138)
# Count observation that contain, Female or Male value and victim descent is not NA
data %>% 
  filter(vict_age < 1,vict_sex %in% c("F","M"), !is.na(vict_descent)) %>% 
  summarise(sum(n()))

## This analysis leads me to believe that any crime with the age of 0 is an error
# in the data entry process or an unknown.
# I will remove the zero age from the data that does not contain sex, or descent
# 
################################ AGE CLEANING END #############################


# dr_no is a case number should be a string not numerical
data$dr_no <- as.character(data$dr_no)

# crime description didnt have spaces it was affecting text wrap in plot
data$crm_cd_desc[data$crm_cd_desc == 
                   "THEFT-GRAND ($950.01 & OVER)EXCPT,GUNS,FOWL,LIVESTK,PROD"] <-
  "THEFT-GRAND ($950.01 & OVER) EXCPT, GUNS, FOWL, LIVESTK, PROD"

################################ End OF Data Clean #############################


############################### Processing Data ################################

#Create an age group, development stage,season,and time period of the day
#helps with the analysis.
# Development stage.
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
# Age Group
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

# season
data <- data %>% 
  mutate(
    season = dplyr::case_when(
      month(date_occ) >=1 & month(date_occ) <= 2 ~ "Winter",
      month(date_occ) > 2 & month(date_occ) <= 5 ~ "Spring",
      month(date_occ) > 5 & month(date_occ) <= 8 ~ "Summer",
      month(date_occ) > 8 & month(date_occ) <= 11 ~ "Autumn",
      month(date_occ) > 11 ~ "Winter"
    )
  )

# Check if script above worked.
data %>% 
  group_by(month(date_occ),season) %>% 
  summarise() # it worked.



# Period of the day.
data <- data %>% 
  mutate(
    time_period = dplyr::case_when(
      as.numeric(time_occ) > 0000 & as.numeric(time_occ) <= 0459 ~ "Night",
      as.numeric(time_occ) > 0459 & as.numeric(time_occ) <= 0959 ~ "Early Morning",
      as.numeric(time_occ) > 0959 & as.numeric(time_occ) <= 1159 ~ "Late Morning",
      as.numeric(time_occ) > 1159 & as.numeric(time_occ) <= 1659 ~ "Early Noon",
      as.numeric(time_occ) > 1659 & as.numeric(time_occ) <= 1759 ~ "Late Noon",
      as.numeric(time_occ) > 1759 & as.numeric(time_occ) <= 1859 ~ "Evening",
      as.numeric(time_occ) > 1859 & as.numeric(time_occ) <= 2359 ~ "Night"
    )
  )
# Check if script above worked.

# female and male as numerical form 0-female 1-male
data <- data %>% 
  mutate(
    num_vict_sex = dplyr::case_when(
      vict_sex == "F" ~ 0,
      vict_sex == "M" ~ 1,
      vict_sex == "H" ~ 2,
      vict_sex == "X" ~ 2,
      vict_sex == NA ~ 3,
    )
  )

print(data %>% 
  group_by(hour(datetime_occ),time_period) %>% 
  summarise(),n=29)
################################# End Of Amending  #############################
data %>% 
  group_by(vict_sex) %>% 
  summarise()


##################### Exploratory Data Analysis ################################
# _________EDA Table
# EDA data table 2020-2021
eda_Data <- data %>% 
  filter(year(date_occ)<2022, vict_age > 1)
str(eda_Data)
# ___________________________________________


# Comparison in total crimes from 2020 to 2021 
eda_Data %>% 
  group_by(year = factor(year(date_occ))) %>% 
  summarise(cases = n()) %>% 
  ggplot(aes(year, y = cases, fill = year)) + 
  geom_bar(stat = "identity", width = .5) + 
  scale_fill_manual(values = c("#219EBC","#FB8500")) +
  geom_text(aes(label=cases), vjust= -0.2, size= 4, fontface = "bold") +
  labs(title = "Total Cases of Crime", subtitle = "(2020-2021)",
       x= "YEAR",y="CASES", 
       fill="YEAR",
       caption="Data Provided By Los Angeles Police Department")

# total crime by victim sex
eda_Data %>% 
  group_by(year = factor(year(datetime_occ)), vict_sex) %>% 
  filter(vict_sex == 'F'| vict_sex == 'M' ) %>% 
  summarise(cases = n()) %>% 
  ggplot(aes(vict_sex, y = cases, fill = year)) + 
  geom_bar(stat = "identity", width = .5)  + 
  scale_fill_manual(values = c("#219EBC","#FB8500")) +
  geom_text(aes(label=cases), position = position_stack(vjust=0.5), 
            size= 4, fontface = "bold") +
  labs(title = "Total Cases Of Crime By Victim Sex", subtitle = "(2020-2021)",
       x= "SEX",y="CASES", 
       fill="YEAR",
       caption="Data Provided By Los Angeles Police Department")

# total crime by age group
eda_Data %>% 
  group_by(year = factor(year(datetime_occ)), factor(age_group)) %>% 
  summarise(cases = n())



# total crime by victim descent
eda_Data %>%  
  group_by(year = factor(year(datetime_occ)), vict_descent) %>% 
  summarise(cases = n())






















