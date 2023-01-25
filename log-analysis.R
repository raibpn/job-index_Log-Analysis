library(tidyverse)
library(dplyr)
library(readr)
library("ggplot2")
library(lubridate)
library(scales)
library(reconstructr)

if(!require('reconstructr')){
  install.packages('reconstructr')
  library('reconstructr')
}


query_data <- read_csv("query-data.2021.csv")
industry_data <- read_csv("industry-data.csv")

head(query_data)
total_values<- nrow(query_data)

total_missing_value_in_query<-sum(is.na(query_data$QUERY))

#number of sessions count
session_count <- query_data%>%group_by(JOB_ID, CATEGORIES)%>%
  summarise(COUNT=n())

#how many session in average
mean_session_count <- mean(session_count$COUNT, na.rm = TRUE)

rm(session_lengthTime_groupedCategoreis)

#how the session last
session_length_time <- query_data%>% group_by(JOB_ID)%>%
  mutate(session_duration = max(TIMESTAMP)-min(TIMESTAMP))%>%
  summarise(CATEGORIES,Duration = seconds_to_period(session_duration))


#drop na containing row from session_length_time
session_length_time_filtered <- na.omit(session_length_time)
sum(is.na(session_length_time_filtered$CATEGORIES))

#what is the maximum session duration and which row
max_session <- session_length_time_filtered%>%
 group_by(Duration)%>%
  slice_max(Duration)%>%tail(15)



mean(session_length_time$Duration, na.rm = TRUE)
#1186 hours that is 49 days

max(session_length_time$session_duration) #who does this session length belongs to?

#JOIN QUERY DATA & INDUSTRY DATA TO VISUALIZE PER industry_sector_name

industry_query_counts <- inner_join(query_data,industry_data, by = "JOB_ID") %>%
  group_by(INDUSTRY_SECTOR_NAME)%>% 
  summarize(query_count = n())%>%arrange(desc(query_count)) #summarize multiple same data


