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
#count how many ages between -1 and 1
data %>%
  select(vict_age,vict_sex) %>% 
  filter(between(vict_age,-1,1), vict_sex != "X") %>% 
  group_by(vict_sex,vict_age) %>% 
  count(vict_sex)

# I strongly believe Age zero stands for someone calling 911 
# their age was not given or recorded. 
print(data %>% 
        select(vict_age,vict_sex,crm_cd_desc) %>% 
        filter(vict_age == 0,vict_sex != "X") %>% 
        tail(),n=100)

# checking what age -1 represents
print(data %>% 
        select(vict_age,vict_sex,crm_cd_desc) %>% 
        filter(vict_age == -1,vict_sex != "X") %>% 
        tail(),n=100)
# out of over 500,000 observations it seems that -1 is a mistake

#Create an age group and a development stage. helps with the analysis.
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

#dr_no is a case number should be a string not numerical
data$dr_no <- as.character(data$dr_no)

# Rank Crime In LA (by year)
data %>% 
  select(crm_cd_desc,date_occ) %>% 
  group_by(crm_cd_desc,Year=factor(year(date_occ))) %>% 
  count(crm_cd_desc) %>% 
  ungroup() %>% 
  top_n(10) %>% 
  ggplot(aes(x=crm_cd_desc,y=n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Year) +
  labs(title = "Top 10 Crimes In LA (2020-Present)",
       subtitle="Top ten most committed crimes in Los Angeles",
       x="CRIME", y="Cases",
       fill="Year",
       caption="Data Provided By Los Angeles Police Department") +
  theme(axis.text.x = element_text(angle = 0,vjust = .5,size=8)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
  scale_fill_manual(values = c("#219EBC","#023047","#FB8500"))
  
  

# Comparison of crime from 2020-to present
data %>% 
  select(date_occ) %>% 
  group_by(Year=year(date_occ)) %>% 
  count(date_occ) %>% 
  ggplot(aes(x=date_occ,y=n)) +
  geom_point() + geom_smooth()


# Crime Female vs Male

# Age group most affected by crime 

# Months affected by crime most

# what time does crime happen most.

# What area has the most crime

# 