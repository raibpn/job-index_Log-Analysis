library(tidyverse)
library(dplyr)
library(readr)
library("ggplot2")
library(lubridate)
library(scales)
library(reconstructr)


query_data <- read_csv("query-data.2021.csv")
industry_data <- read_csv("industry-data.csv")
response_data <- read_csv("response-data.2021.csv")

head(query_data)
total_values<- nrow(query_data)

total_missing_value_in_query<-sum(is.na(query_data$QUERY))


#(Here if we see same queries can be seen in more than on session also
 #i.e query reformulation table row no. 89 and 105)
session_query_counts <- query_data %>%  
  group_by( JOB_ID, CATEGORIES, QUERY)%>%
  summarize(COUNT =n())%>%
  filter(COUNT <100)

filtered_session_query_counts <- na.omit(session_query_counts)
#repeated query-query repated count
repeated_query <- filtered_session_query_counts%>%
  group_by(QUERY)%>%
  summarise(Count = n())%>%arrange(desc(Count))


#number queries in a session
query_per_session <- query_data%>%
 group_by(JOB_ID)%>%
  summarise(COUNT = n())
 

#how many queries per session in average
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

#session length time grouped by categories and mean session length time produced
session_length_by_categories <- session_length_time_filtered%>%group_by(CATEGORIES)%>%
  summarise(mean_duration = mean(Duration))







#session length by industry
session_length_by_industry <- inner_join(session_length_time_filtered,industry_data,by=c("JOB_ID" = "JOB_ID"))%>%
  group_by(INDUSTRY_SECTOR_NAME)%>%
  summarise(mean_length_seconds = seconds_to_period(mean(Duration)))

session_length_industry_summary <- session_length_by_industry%>%arrange(desc(mean_length))


#visualize session_length_industry_summary in histogram
ggplot(data=session_length_industry_summary,aes(x = mean_length, y= reorder(INDUSTRY_SECTOR_NAME, mean_length),
                                 fill=mean_length))+geom_bar(stat = "identity")+
  labs(x="MEAN_LENGTH_SECONDS", y="INDUSTRY_MEAN_DURATION", title = "Industry Success Rate")








#what is the maximum session duration and which row
max_session <- session_length_time_filtered%>%
 group_by(Duration)%>%
  slice_max(Duration)%>%tail(15)



max(session_length_time$session_duration) #who does this session length belongs to?

#df<-filter(industry_data, JOB_ID == 538240)

#JOIN QUERY DATA & INDUSTRY DATA TO VISUALIZE PER industry_sector_name

industry_mean_query_counts <- inner_join(query_data,industry_data,by=c("JOB_ID" = "JOB_ID")) %>%
  group_by(INDUSTRY_SECTOR_NAME)%>%
  select(JOB_ID,INDUSTRY_SECTOR_NAME)%>%
  mutate(query_count = n())%>%
  summarise(MEAN=mean(query_count))%>%
  arrange(desc(MEAN))
  

ggplot(data=industry_mean_query_counts,aes(x = MEAN, y= reorder(INDUSTRY_SECTOR_NAME, MEAN),
                  fill=MEAN))+geom_bar(stat = "identity")+
                  labs(x="MEAN", y="INDUSTRY_SECTOR_NAME", title = "MEAN Query Per Company")

#most frequent queries per company (if compared above geombar is approved)
frequent_queries_per_company <- industry_mean_query_counts%>%head(5)


#distribution of queries by type

  

#Average queries per session grouped by Industry
industy_query_count <- inner_join(industry_data,query_data, by="JOB_ID", "INDUSTRY_SECTOR_NAME")%>%
  group_by(INDUSTRY_SECTOR_NAME)%>%
  summarise(QUERY_COUNT = n())

#query success rate
response_df <- response_data%>%
  mutate(success = ifelse(RESPONSE_TYPE ==1, "Recruiting","Matching"))

filter(response_df, JOB_ID == 153505)

#query_success <- inner_join(industry_data, response_df,by=c("JOB_ID" = "JOB_ID"))%>%
 # group_by(INDUSTRY_SECTOR_NAME)%>%
  #count(RESPONSE_TYPE)%>%
  #mutate(total = sum(n))%>%
  #mutate(rate = n/total)%>%
  #summarise(mean_rate = mean(rate))

query_success <- inner_join(industry_data, response_df,by=c("JOB_ID" = "JOB_ID"))%>%
  group_by(INDUSTRY_SECTOR_NAME)%>%
  mutate(successful_responses = sum(RESPONSE_TYPE == 1), 
            total_responses = n())%>%
  summarise(success_rate = mean(successful_responses))


industry_summary <- query_success%>%
  arrange(desc(success_rate))


ggplot(data=industry_summary,aes(x = success_rate, y= reorder(INDUSTRY_SECTOR_NAME, success_rate),
                                           fill=success_rate))+geom_bar(stat = "identity")+
  labs(x="SUCESS_RATE", y="INDUSTRY_SECTOR_NAME", title = "Industry Success Rate")
  
 


#CORRELATION BETWEEN QUERY LENGTH AND SUCCESS RATE

