library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(reshape2)

data<-read_csv(
  "https://data.lacity.org/resource/2nrs-mtv8.csv?$limit=527897&$offset=100")

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


# _______________________EDA Table

# EDA data table 2020-2021
eda_Data <- data %>% 
  filter(year(date_occ)<2022)


# Rank Crime In LA (by year)
eda_Data %>% 
  select(crm_cd_desc,date_occ) %>% 
  group_by(crm_cd_desc,Year=factor(year(date_occ))) %>% 
  count(crm_cd_desc) %>% 
  ungroup() %>% 
  slice_max(n,n=20) %>% 
  ggplot(aes(x=crm_cd_desc,y=n)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Year, scales="free_x") +
  labs(title = "Top 10 Crimes In LA (2020-2021)",
       subtitle="Top ten most committed crimes in Los Angeles",
       x="CRIME", y="Cases",
       fill="Year",
       caption="Data Provided By Los Angeles Police Department") +
  theme(axis.text.x = element_text(angle = 0,vjust =.5,size=7)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))
# Top ten crimes remained the same throught 2020 and 2021
#####################

# Comparison of crime from 2020-to present
eda_Data %>% 
  select(date_occ) %>% 
  group_by(Year=year(date_occ)) %>% 
  count(date_occ) %>% 
  ggplot(aes(x=date_occ,y=n)) +
  geom_point() + geom_smooth() +
  labs(title = "Crime Through 2020 & 2021",
       subtitle="Cases of crime through timespan of 2 years",
       x="Time", y="Cases",
       caption="Data Provided By Los Angeles Police Department")


# Crime Female vs Male
eda_Data %>% 
  select(vict_sex,dr_no) %>% 
  group_by(vict_sex) %>% 
  count(vict_sex)
  # gave me an ouput of X = Unkowns, and NA = Unkowns, H = Unknowns
# I beleive those are callers that did not give out their idenetity
# I will filter these out still enough meaningful data

# Male vs Female victims.
eda_Data %>% 
  filter(vict_sex != "H",
         vict_sex != "X",
         vict_sex != is.na(vict_sex)) %>% 
  group_by(Year=year(date_occ),vict_sex) %>% 
  summarise(cases = n()) %>% 
  ggplot(aes(x=vict_sex,y=cases,fill=factor(Year))) +
  geom_bar(stat="identity",position="dodge") +
  labs(title = "Victims By Female Vs Male",
       subtitle="Case Count Between Male & Women Victims 2020-2021",
       x="Sex", y="Cases Count",
       fill="Year",
       caption="Data Provided By Los Angeles Police Department") +
  geom_text(aes(label=cases),position=position_dodge(1),vjust=-.5) +
  scale_fill_manual(values = c("#0F4C5C","#FB8B24"))

# Age group most affected by crime 
eda_Data %>% 
  filter(vict_sex != "H",
         vict_sex != "X",
         vict_sex != is.na(vict_sex),
         age_group != "Unkown") %>% 
  group_by(age_group,vict_sex) %>% 
  count(age_group) %>% 
  ungroup() %>% 
  mutate(perc =(n/sum(n)) *100) %>% 
  ggplot(aes(x=age_group,y=n,fill=vict_sex)) +
  geom_bar(stat="identity",position="dodge") +
  geom_text(aes(label=n),position = position_dodge(1),vjust=-.5) +
  labs(title = "Victims By Age Groups & Sex",
       subtitle="Case Count Between Age Groups And Sex 2020-2021",
       x="Age Group", y="Cases",
       fill="Sex",
       caption="Data Provided By Los Angeles Police Department") +
  scale_fill_manual(values = c("#0F4C5C","#FB8B24"))

# What race is most affected by crime in LA
eda_Data %>% 
  filter(vict_sex != "H",
         vict_sex != "X",
         vict_sex != is.na(vict_sex),
         age_group != "Unkown") %>% 
  group_by(vict_descent,vict_sex) %>% 
  count(vict_descent,vict_descent) %>% 
  ggplot(aes(x=vict_descent,y=n,fill=vict_sex)) +
  geom_bar(stat="identity")
  



# What area has the most crime
sum(is.na(eda_data$area_name)) #contains no nulls or empty cells

eda_Data %>% 
  group_by(area_name,Year=year(date_occ)) %>% 
  count(area_name) %>% 
  ggplot(aes(x=area_name,y=n,fill=factor(Year))) +
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5)) +
  geom_text(aes(label=n),vjust=1.5,
            position = "stack",
            size=4,
            color="White",
            fontface="bold") +
  labs(title = "Areas & Crime",
       subtitle="Crime Cases In Particular Areas Of LA 2020-2021",
       x="Area", y="Cases",
       fill="Year",
       caption="Data Provided By Los Angeles Police Department") +
  scale_fill_manual(values = c("#0F4C5C","#FB8B24"))










