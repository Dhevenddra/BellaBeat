#install packages
install.packages('skimr')
install.packages('tidyverse')
install.packages('janitor')
install.packages('lubridate')
install.packages('ggplot2')
install.packages('dplyr')
install.packages('geosphere')
install.packages('readr')

#load packages
library(tidyverse)
library(janitor)
library(lubridate)
library(ggplot2)
library(dplyr)
library(geosphere)
library(readr)
library(skimr)

#read csv files
daily_activity <- read.csv("C:/Users/dhev0/OneDrive/Desktop/GoogleCS2/Fitabase Data 4.12.16-5.12.16/dailyActivity_merged.csv")
daily_sleep <- read.csv("C:/Users/dhev0/OneDrive/Desktop/GoogleCS2/Fitabase Data 4.12.16-5.12.16/sleepDay_merged.csv")
weight_log <- read.csv("C:/Users/dhev0/OneDrive/Desktop/GoogleCS2/Fitabase Data 4.12.16-5.12.16/weightLogInfo_merged.csv")

#Exaxmine the structure of the data
str(daily_activity)
str(daily_sleep)
str(weight_log)

#clean
daily_activity <- clean_names(daily_activity)
daily_sleep <- clean_names(daily_sleep)
weight_log <- clean_names(weight_log)

#change into date format and logical format
daily_activity$activity_date <- as.Date(daily_activity$activity_date,'%m/%d/%y')
daily_sleep$sleep_day <- as.Date(daily_sleep$sleep_day, '%m/%d/%y')
weight_log$date <- parse_date_time(weight_log$date, '%m/%d/%y %H:%M:%S %p')
weight_log$is_manual_report <- as.logical(weight_log$is_manual_report)

#Add day of week column
#Remove fat column
#Add time take to sleep column
#Convert values to hour and round off
daily_activity$day_of_week <- wday(daily_activity$activity_date, label = T, abbr = T)
daily_activity$total_active_hours = round((daily_activity$very_active_minutes + daily_activity$fairly_active_minutes + daily_activity$lightly_active_minutes)/60, digits = 2)
daily_activity$sedentary_hours = round((daily_activity$sedentary_minutes)/60, digits = 2)

daily_sleep$hours_in_bed = round((daily_sleep$total_time_in_bed)/60, digits = 2)
daily_sleep$hours_asleep = round((daily_sleep$total_minutes_asleep)/60, digits = 2)
daily_sleep$time_taken_to_sleep = (daily_sleep$total_time_in_bed - daily_sleep$total_minutes_asleep)


weight_log <- weight_log %>% 
  select(-c(fat))

#Using case_when to categorize bmi values
weight_log <- weight_log %>% 
  mutate(bmi2 = case_when(
    bmi > 24.9 ~ 'Overweight',
    bmi < 18.5 ~ 'Underweight',
    TRUE ~ 'Healthy'
  ))


#Remove data where calories and active hours is 0, which means the device where not in use 
#at that time
daily_activity_cleaned <- daily_activity[!(daily_activity$calories<=0),]
daily_activity_cleaned <- daily_activity_cleaned[!(daily_activity_cleaned$total_active_hours<=0.00),]

#summary
summary(daily_activity_cleaned$total_steps)
summary(daily_activity_cleaned$sedentary_hours)
summary(daily_activity_cleaned$very_active_minutes)
summary(daily_sleep$hours_asleep)

#total steps, activity, and calories
options(scipen = 999)
ggplot(data = daily_activity_cleaned) +
  aes(x = day_of_week, y = total_steps) +
  geom_col(fill =  'blue') +
  labs(x = 'Day of week', y = 'Total steps', title = 'Totap steps taken in a week')

ggplot(data = daily_activity_cleaned) +
  aes(x = day_of_week, y = very_active_minutes) +
  geom_col(fill =  'red') +
  labs(x = 'Day of week', y = 'Total very active minutes', title = 'Total activity in a week')

ggplot(data = daily_activity_cleaned) +
  aes(x = day_of_week, y = calories) +
  geom_col(fill =  'brown') +
  labs(x = 'Day of week', y = 'Calories burned', title = 'Total calories burned in a week')

#calories_burned_vs_active_hours
ggplot(data = daily_activity_cleaned) +
  aes(x= total_active_hours, y = calories) +
  geom_point(color = 'red') +
  geom_smooth() +
  labs(x = 'Total active hours', y = 'Calories burned', title = 'Calories burned vs active hours')
#calories_burned_vs_total_steps
ggplot(data = daily_activity_cleaned) +
  aes(x= total_steps, y = calories) +
  geom_point(color = 'orange') +
  geom_smooth() +
  labs(x = 'Total steps', y = 'Calories burned', title = 'Calories burned vs total steps')
#sedentary_hours_vs_calories_burned
ggplot(data = daily_activity_cleaned) +
  aes(x= sedentary_hours, y = calories) +
  geom_point(color = 'purple') +
  geom_smooth() +
  labs(x = 'Sedentary hours', y = 'Calories burned', title = 'Calories burned vs sedentary hours')

