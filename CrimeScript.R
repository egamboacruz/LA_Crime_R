library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(tidyr)
library(reshape2)

################################## Original data ################################
#if you want to see original data table run this
original_data<-read_csv(
  "https://data.lacity.org/resource/2nrs-mtv8.csv?$limit=527897&$offset=100")
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

#dr_no is a case number should be a string not numerical
data$dr_no <- as.character(data$dr_no)

# crime description didnt have spaces it was affecting text wrap in plot
data$crm_cd_desc[data$crm_cd_desc == 
                   "THEFT-GRAND ($950.01 & OVER)EXCPT,GUNS,FOWL,LIVESTK,PROD"] <-
  "THEFT-GRAND ($950.01 & OVER) EXCPT, GUNS, FOWL, LIVESTK, PROD"

################################ End OF Data Clean #############################

############################### Amend To Data ##################################
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
print(data %>% 
  group_by(hour(datetime_occ),time_period) %>% 
  summarise(),n=29)
################################# End Of Amending  #############################



##################### Exploratory Data Analysis ################################
# _________EDA Table
# EDA data table 2020-2021
eda_Data <- data %>% 
  filter(year(date_occ)<2022)
# ___________________________________________


# Comparison of crime from 2020-to present
eda_Data %>% 
  group_by(Month =month(date_occ,label = TRUE,abbr = TRUE),
           Year=factor(year(datetime_occ))) %>% 
  summarise(cases=n()) %>% 
  ggplot(aes(Month,y=cases,fill=Year)) +
  geom_bar(stat="identity",position = position_dodge(.7),width = .8) +
  facet_wrap(~ Year) +
  geom_text(aes(label=cases),
            position = position_dodge(1),
            vjust=-.5,hjust=.5, size=3) +
  labs(title = "Crime Comparison Through 2020-2021",
       x="MONTH", y="CASES",
       caption="Data Provided By Los Angeles Police Department") +
  scale_fill_manual(values = c("#87BFFF","#2667FF")) +
  theme(plot.title = element_text(lineheight=.8, face="bold"))
#_____________________________________________________________________________#

# Rank Crime In LA (by year)
# Top ten crimes remained the same throught 2020 and 2021
eda_Data %>% 
  select(crm_cd_desc,date_occ) %>% 
  group_by(crm_cd_desc,Year=factor(year(date_occ))) %>% 
  count(crm_cd_desc) %>% 
  ungroup() %>% 
  slice_max(n,n=20) %>% 
  ggplot(aes(x=reorder(crm_cd_desc,n),y=n,fill=Year)) +
  geom_bar(stat = "identity") +
  facet_wrap(~Year, scales="free_x") +
  geom_text(aes(label=n),vjust=-.5,hjust=.5, size=4) +
  labs(title = "Top 10 Crimes In LA (2020-2021)",
       subtitle="Top ten most committed crimes in Los Angeles",
       x="CRIME", y="CASES",
       fill="SEX",
       caption="Data Provided By Los Angeles Police Department") +
  theme(axis.text.x = element_text(angle = 0,vjust =.5,size=7)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5)) +
  scale_fill_manual(values = c("#87BFFF","#2667FF")) +
  theme(plot.title = element_text(lineheight=.8, face="bold"))
#_____________________________________________________________________________#

# Crime Female vs Male
eda_Data %>% 
  select(vict_sex,dr_no) %>% 
  group_by(vict_sex) %>% 
  count(vict_sex)
  # gave me an ouput of X = Unknowns, and NA = Unknowns, H = Unknowns
# I believe those are callers that did not give out their identity
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
  labs(title = "Victims By Sex",
       subtitle="Count Between Male & Women Victims 2020-2021",
       x="SEX", y="CASES",
       fill="YEAR",
       caption="Data Provided By Los Angeles Police Department") +
  geom_text(aes(label=cases),position=position_dodge(1),vjust=-.5) +
  scale_fill_manual(values = c("#87BFFF","#2667FF")) +
  theme(plot.title = element_text(lineheight=.8, face="bold"))

# top ten crimes that affect male and female the most.
eda_Data %>% 
  filter(vict_age != -1,
         vict_age != 0,
         vict_sex != "H",
         vict_sex != "X",) %>% 
  group_by(vict_sex,crm_cd_desc) %>% 
  summarise(cases=n()) %>% 
  top_n(n = 10, wt = cases) %>% 
  ggplot(aes(x=reorder(crm_cd_desc,cases),y=cases,fill=vict_sex)) +
  geom_col() +
  facet_wrap(~vict_sex, scales = "free") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5)) +
  theme(axis.text = element_text(size = 7)) +
  geom_text(aes(label=cases),vjust=-0.5) +
  labs(title = "TOP 10 CRIME BY SEX",
       subtitle="Top 10 crimes committed on Females and Males the most in (2020-2021)",
       x="CRIME", y="CASES",
       fill="SEX",
       caption="Data Provided By Los Angeles Police Department") +
  scale_fill_manual(values = c("#87BFFF","#2667FF")) +
  theme(plot.title = element_text(lineheight=.8, face="bold"))



# Age group most affected by crime 
eda_Data %>% 
  filter(vict_sex != "H",
         vict_sex != "X",
         vict_sex != is.na(vict_sex),
         age_group != "Unkown") %>% 
  group_by(age_group,vict_sex,Year=year(datetime_occ)) %>% 
  count(age_group) %>% 
  ungroup() %>% 
  mutate(perc =(n/sum(n)) *100) %>% 
  ggplot(aes(x=factor(age_group,
                      levels = c("2-4","5-12","13-19","20-39","40-59","60+"))
             ,y=n,fill=vict_sex)) +
  geom_bar(stat="identity",position="dodge") +
  facet_wrap(~Year) +
  geom_text(aes(label=n),position = position_dodge(1),vjust=-.5) +
  labs(title = "Victims By Age Groups & Sex",
       subtitle="Case Count Between Age Groups And Sex 2020-2021",
       x="Age Group", y="Cases",
       fill="Sex",
       caption="Data Provided By Los Angeles Police Department") +
  scale_fill_manual(values = c("#87BFFF","#2667FF")) +
  theme(plot.title = element_text(lineheight=.8, face="bold"))

# Top Ten Crimes That affect Age groups




# What race is most affected by crime in LA
eda_Data %>% 
  filter(vict_sex != "H",
         vict_sex != "X",
         vict_sex != is.na(vict_sex),
         vict_descent != is.na(vict_descent)) %>% 
  group_by(vict_descent,vict_sex,Year=year(date_occ)) %>% 
  count(vict_descent,vict_descent) %>% 
  ggplot(aes(x=reorder(vict_descent,n),y=n,fill=vict_sex)) +
  geom_bar(stat="identity") +
  geom_text(aes(label=n), 
            position = "stack",
            hjust=0,
            size=2.5,
            color="black",
            fontface="bold") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 18)) +
  coord_flip() +
  facet_wrap(~ vict_sex + Year) +
  labs(title = "Descent & Crime",
       subtitle="Crime Cases By Victim Descent In Los Angeles 2020-2021",
       x="Descent", y="Cases",
       fill="Sex",
       caption="Data Provided By Los Angeles Police Department") +
  scale_fill_manual(values = c("#87BFFF","#2667FF")) +
  theme(plot.title = element_text(lineheight=.8, face="bold"))

# Top ten crimes that affect different races.



  

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
            size=3,
            color="White",
            fontface="bold") +
  labs(title = "Areas & Crime",
       subtitle="Crime Cases In Particular Areas Of LA 2020-2021",
       x="Area", y="Cases",
       fill="Year",
       caption="Data Provided By Los Angeles Police Department") +
  scale_fill_manual(values = c("#87BFFF","#2667FF")) +
  theme(plot.title = element_text(lineheight=.8, face="bold"))


# Top Five Crimes that affect the different areas
eda_Data %>% 
  group_by(area_name,crm_cd_desc) %>% 
  summarise(cases=n()) %>% 
  top_n(n=5,wt=cases) %>% 
  ggplot(aes(x=reorder(area_name,cases),
             y=cases,
             fill=reorder(crm_cd_desc,-cases))) +
  geom_bar(stat="identity") +
  coord_flip() +
  labs(title = "Top Crimes Committed By Area",
       subtitle = "Top 5 Crimes Committed in all areas reporting to the LAPD",
       x="AREA",y="CASES",fill="CRIME") +
  scale_fill_manual(values = c("#54478C","#2C699A",
                             "#048BA8","#0DB39E",
                             "#16DB93","#83E377",
                             "#B9E769","#EFEA5A",
                             "#F1C453","#F29E4C")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))



############################### Time Analysis ##################################
# Times it happens the most
eda_Data %>% 
  group_by(time=hour(datetime_occ),Year=factor(year(date_occ))) %>% 
  summarise(cases=n()) %>% 
  ggplot(aes(x=time,y=cases,fill=Year)) +
  geom_histogram(stat = "identity") +
  facet_wrap(~Year) +
  labs(title = "Time Of Day & Crime",
       subtitle="What TIme Of The Day Does Most Crime Happen In LA 2020-2021",
       x="Time", y="Cases",
       fill="Year",
       caption="Data Provided By Los Angeles Police Department") +
  scale_fill_manual(values = c("#87BFFF","#2667FF")) +
  theme(plot.title = element_text(lineheight=.8, face="bold"))


# The later in the day the more crime that happens BUT 
# It seems that there is an out-lair 1300 or 1:00 PM 
# Seems like a lot of crime happens at 1200 or 12:00 PM 
# What I will search for 

# What type of crime is happening
print(eda_Data %>% 
        filter(hour(datetime_occ) == 12) %>% 
        group_by(Year=year(datetime_occ),crm_cd_desc) %>% 
        summarise(cases=n()) %>% 
        mutate(perc = (cases/sum(cases))*100 ) %>% 
        arrange(desc(cases)),n=214)

# Top 5 crime for the time 12:00 PM
eda_Data %>% 
  filter(hour(datetime_occ) == 12) %>% 
  group_by(Year=year(datetime_occ),crm_cd_desc) %>% 
  summarise(cases=n()) %>% 
  mutate(perc = (cases/sum(cases))*100 ) %>% 
  arrange(desc(cases)) %>% 
  top_n(n = 10, wt = cases)  %>% 
  ggplot(aes(x=reorder(crm_cd_desc,cases),y=cases,fill=factor(Year))) +
  geom_bar(stat="identity",position = position_dodge(1)) +
  facet_wrap(~Year) +
  labs(title = "Top 5 Crimes at 12PM",
       subtitle="Top 5 crimes that occurr at 12PM",
       x="Crime", y="Cases",
       fill="Year",
       caption="Data Provided By Los Angeles Police Department") +
  scale_fill_manual(values = c("#87BFFF","#2667FF")) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 5))
## identity theft makes up 12.2 percent of the crime committed in the afternoon 
# I need to check if that's just the time the department decides to put or if its
# really occurring at that time.
eda_Data %>% 
  select(datetime_occ,crm_cd_desc) %>% 
  filter(crm_cd_desc == "THEFT OF IDENTITY")
eda_Data %>% 
  select(datetime_occ,crm_cd_desc) %>% 
  filter(crm_cd_desc == "THEFT OF IDENTITY") %>% 
  tail()
# It seems that identity theft happens at all time of the day but mostly at 12:00PM
eda_Data %>% 
  filter(hour(datetime_occ) == 12,crm_cd_desc == "THEFT OF IDENTITY") %>% 
  group_by(Year=year(datetime_occ),crm_cd_desc,age_group) %>% 
  summarise(cases=n()) %>% 
  mutate(perc = (cases/sum(cases))*100 ) %>% 
  arrange(desc(cases))


# What ages are being affected
eda_Data %>% 
  filter(hour(datetime_occ) == 12) %>% 
  group_by(Year=year(datetime_occ),crm_cd_desc,age_group) %>% 
  summarise(cases=n()) %>% 
  arrange(desc(cases))

  
# What descent

############################### End Of Time Analysis ###########################





 
############################### End Of EDA #####################################



