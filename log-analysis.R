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

session_count <- query_data%>%group_by(JOB_ID)%>%
  summarise(COUNT=n())


#how the session last in seconds
session_length_time <- query_data%>% group_by(JOB_ID)%>%
  summarise(session_duration = max(TIMESTAMP)-min(TIMESTAMP))

mean(session_length_time$session_duration/120, na.rm = TRUE)
#1186 hours that is 49 days

max(session_length_time$session_duration) #who does this session length belongs to?

sessions <- sessionise(query_data,TIMESTAMP,JOB_ID)
