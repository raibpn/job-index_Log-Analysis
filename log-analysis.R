library(tidyverse)
library(dplyr)
library(readr)
library("ggplot2")
library(lubridate)
library(scales)
library(reconstructr)


query_data <- read_csv("query-data.2021.csv")
industry_data <- read_csv("industry-data.csv")

head(query_data)
total_values<- nrow(query_data)

total_missing_value_in_query<-sum(is.na(query_data$QUERY))


#(Here if we see same queries can be seen in more than on session also
 #i.e query reformulation table row no. 89 and 105)
session_query_counts <- query_data %>%  
  group_by( JOB_ID, CATEGORIES, QUERY)%>%
  summarize(COUNT =n())%>%
  filter(COUNT <100)


#number queries in a session
query_per_session <- query_data%>%
 group_by(JOB_ID)%>%
  summarise(COUNT = n())
 

#how many session in average
mean_query_per_session <- mean(query_per_session$COUNT, na.rm = TRUE)
max(query_per_session$COUNT)
min(query_per_session$COUNT)

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



max(session_length_time$session_duration) #who does this session length belongs to?




#JOIN QUERY DATA & INDUSTRY DATA TO VISUALIZE PER industry_sector_name

industry_query_counts <- inner_join(query_data,industry_data,by=c("JOB_ID")) %>%
  group_by(INDUSTRY_SECTOR_NAME)%>%
  select(JOB_ID,INDUSTRY_SECTOR_NAME)%>%
  mutate(COUNT = n())
  


mean_query_per_session_industry <-summarise(group_by(industry_query_counts,INDUSTRY_SECTOR_NAME),
                                            MEAN = mean(COUNT))
           
  


ggplot(data=mean_query_per_session_industry,aes(x = MEAN, y= reorder(INDUSTRY_SECTOR_NAME, MEAN),
                                      fill=MEAN))+geom_bar(stat = "identity")


#most frequent queries per company (if compared above geombar is approved)
frequent_queries_per_company <- head(industry_query_counts)

#Average queries per session grouped by Industry
industy_query_count <- inner_join(industry_data,query_data, by="JOB_ID", "INDUSTRY_SECTOR_NAME")%>%
  summarise(INDUSTRY_SECTOR_NAME, COUNT = n())%>%
  summarise(INDUSTRY_SECTOR_NAME, MEAN_QUERY_COUNT = mean())

