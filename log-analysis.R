install.packages("ggpubr")
install.packages("dplyr")

library(tidyverse)
library(dplyr)
library(readr)
library("ggplot2")
library(lubridate)
library(scales)
library(reconstructr)
library("ggpubr")
library(hrbrthemes)

#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#IMPORT ALL THE DATA SETS
query_data <- read_csv("query-data.2021.csv")
industry_data <- read_csv("industry-data.csv")
response_data <- read_csv("response-data.2021.csv")


head(query_data)
total_values<- nrow(query_data)

total_missing_value_in_query<-sum(is.na(query_data$QUERY))
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#TOP 10 SEARCH RESULTS#

search_logs <- query_data

clean_search_logs <- search_logs %>%
  filter(!is.na(query)) %>%
  select(QUERY,TIMESTAMP_FORMATTED)
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
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

#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#number queries in a session
query_per_session <- query_data%>%
 group_by(JOB_ID)%>%
  summarise(COUNT = n())
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
query_per_session1 <- query_data%>%
  group_by(JOB_ID)
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#how many queries per session in average
mean_query_per_session <- mean(query_per_session$COUNT, na.rm = TRUE)
max(query_per_session$COUNT)
min(query_per_session$COUNT)
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#how the session last
session_length_time <- query_data%>% group_by(JOB_ID)%>%
  mutate(session_duration = max(TIMESTAMP)-min(TIMESTAMP))%>%
  summarise(CATEGORIES,Duration = seconds_to_period(session_duration))


#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#drop na containing row from session_length_time
session_length_time_filtered <- na.omit(session_length_time)
sum(is.na(session_length_time_filtered$CATEGORIES))
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#<-------Max time per session----------------------------->#
sessionn_time_no_lubridate <- query_data%>% group_by(JOB_ID)%>%
  mutate(session_duration = max(TIMESTAMP)-min(TIMESTAMP))%>%
  summarise(Duration =(session_duration))

average_session_time <- mean(sessionn_time_no_lubridate$Duration, na.rm = TRUE)

average_session_time_formatted <- seconds_to_period(average_session_time)


print(average_session_time_formatted)

#session length time grouped by categories and mean session length time produced
session_length_by_categories <- session_length_time_filtered%>%group_by(CATEGORIES)%>%
  summarise(mean_duration = mean(Duration))


#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#session length by industry
session_length_by_industry <- inner_join(session_length_time_filtered,industry_data,by=c("JOB_ID" = "JOB_ID"))%>%
  group_by(INDUSTRY_SECTOR_NAME)%>%
  summarise(mean_length_seconds = seconds_to_period(mean(Duration)))

session_length_industry_summary <- session_length_by_industry%>%arrange(desc(mean_length))


#visualize session_length_industry_summary in histogram
ggplot(data=session_length_industry_summary,aes(x = mean_length, y= reorder(INDUSTRY_SECTOR_NAME, mean_length),
                                 fill=mean_length))+geom_bar(stat = "identity")+
  labs(x="MEAN_LENGTH_SECONDS", y="INDUSTRY_SECTOR_NAME", title = "Industry Success Rate")



#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#

#what is the maximum session duration and which row
max_session <- session_length_time_filtered%>%
 group_by(Duration)%>%
  slice_max(Duration)%>%tail(15)



max(session_length_time$session_duration) #who does this session length belongs to?

#df<-filter(industry_data, JOB_ID == 538240)

#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
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

#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#most frequent queries per company (if compared above geombar is approved)
frequent_queries_per_company <- industry_mean_query_counts%>%head(5)



#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#Average queries per session grouped by Industry
industy_query_count <- inner_join(industry_data,query_data, by="JOB_ID", "INDUSTRY_SECTOR_NAME")%>%
  group_by(INDUSTRY_SECTOR_NAME)%>%
  summarise(QUERY_COUNT = n())

#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#query success rate
response_df <- response_data%>%
  mutate(success = ifelse(RESPONSE_TYPE ==1, "Recruiting","Matching"))

filter(response_df, JOB_ID == 153505)

#query_success <- inner_join(industry_data, response_df,by=c("JOB_ID" = "JOB_ID"))%>%
 #group_by(INDUSTRY_SECTOR_NAME)%>%
  #count(RESPONSE_TYPE)%>%
  #mutate(total = sum(n))%>%
  #mutate(rate = n/total)%>%
  #summarise(mean_rate = mean(rate))

#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
query_success <- inner_join(industry_data, response_df,by=c("JOB_ID" = "JOB_ID"))%>%
  group_by(INDUSTRY_SECTOR_NAME)%>%
  mutate(successful_responses = sum(RESPONSE_TYPE == 1), 
            total_responses = n())%>%
  summarise(success_rate = mean(successful_responses))

query_success1 <- inner_join(industry_data, response_df,by=c("JOB_ID" = "JOB_ID"))%>%
  group_by(INDUSTRY_SECTOR_NAME)%>%
  mutate(successful_responses = sum(RESPONSE_TYPE == 1), 
         total_responses = n())


industry_summary <- query_success%>%
  arrange(desc(success_rate))


ggplot(data=industry_summary,aes(x = success_rate, y= reorder(INDUSTRY_SECTOR_NAME, success_rate),
                                           fill=success_rate))+geom_bar(stat = "identity")+
  labs(x="SUCESS_RATE", y="INDUSTRY_SECTOR_NAME", title = "Industry Success Rate")
  
 



#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#CORRELATION BETWEEN LOCATION AND SUCCESS RATE
correlation_query_success_location <- inner_join(query_per_session1, query_success1,by=c("JOB_ID" = "JOB_ID"))%>%
  group_by(JOB_ID)%>%
  select(LOCATION_IDS, successful_responses)

filtered_query_correlation_location <- na.omit(correlation_query_success_location)

summarised_query_cor <- filtered_query_correlation_location%>%group_by(LOCATION_IDS)%>%
  summarise(mean_rate = mean(successful_responses), n=n())

#top_query_cor_location<-filtered_query_correlation_location%>%head(10)
ggscatter(summarised_query_cor, x = "LOCATION_IDS", y = "successful_responses", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "Miles/(US) gallon", ylab = "Weight (1000 lbs)")
  
cor(top_query_cor_location$LOCATION_IDS, top_query_cor_location$successful_responses)


plot(top_query_cor_location$LOCATION_IDS, top_query_cor_location$successful_responses)
cor(correlation_query_success_location$LOCATION_IDS, correlation_query_success_location$successful_responses)


#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#CORRELATION BETWEEN SALARY AND SUCCESS RATE
correlation_query_success_salary <- inner_join(query_per_session1, query_success1,by=c("JOB_ID" = "JOB_ID"))%>%
  group_by(JOB_ID)%>%
  select(SALARY_MAX, successful_responses)

filtered_query_correlation_salary <- na.omit(correlation_query_success_salary)

summarised_query_cor_salary <- filtered_query_correlation_salary%>%group_by(SALARY_MAX)%>%
  summarise(mean_success = mean(successful_responses), n=n())

ggscatter(summarised_query_cor_salary, x = "SALARY_MAX", y = "mean_success", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MAX SALARY", ylab = "MEAN_SUCCESS")

cor(summarised_query_cor_salary$SALARY_MAX, summarised_query_cor_salary$mean_success)

#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#CORRELATION BETWEEN MINIMUM WORK EXPERIENCE AND SUCCESS RATE
correlation_query_success_min_exp <- inner_join(query_per_session1, query_success1,by=c("JOB_ID" = "JOB_ID"))%>%
  group_by(JOB_ID)%>%
  select(WORK_EXP_MIN, successful_responses)

filtered_query_correlation_min_exp <- na.omit(correlation_query_success_min_exp)

summarised_query_cor_min_exp <- filtered_query_correlation_min_exp%>%group_by(WORK_EXP_MIN)%>%
  summarise(mean_success = mean(successful_responses), n=n())

ggscatter(summarised_query_cor_min_exp, x = "WORK_EXP_MIN", y = "mean_success", 
          add = "reg.line", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson",
          xlab = "MIN EXPERIENCE", ylab = "MEAN_SUCCESS")


#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#CORRELATION BETWEEN LANGUAGE SKILLS AND SUCCESS RATE
correlation_query_success_language <- inner_join(query_per_session1, query_success1,by=c("JOB_ID" = "JOB_ID"))%>%
  group_by(JOB_ID)%>%
  select(LANGUAGE_SKILLS, successful_responses)

filtered_query_correlation_language <- na.omit(correlation_query_success_language)

summarised_query_cor_language <- filtered_query_correlation_language%>%group_by(LANGUAGE_SKILLS)%>%
  summarise(mean_success = mean(successful_responses), n=n())


#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#MOST FREQUENT KEYWORDS OR CATEGORIES IN SUCCESSFULL QUERIES
frequent_query_success <- inner_join(query_per_session1, query_success1,by=c("JOB_ID" = "JOB_ID"))%>%
  group_by(JOB_ID)%>%
  select(QUERY, successful_responses)

filtered_frequent_query_success <- na.omit(frequent_query_success)

summarised_frequent_query_success <- filtered_frequent_query_success%>%group_by(QUERY)%>%
  summarise(mean_success = mean(successful_responses), n=n())

arranged_frequent_query_success <- summarised_frequent_query_success%>%arrange(desc(mean_success))
top_frequent_query_success <- arranged_frequent_query_success%>%head(5)
#unique(arranged_frequent_query_success$mean_success)


#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#QUERY SUCCESS BEHAVIOR CHANGED OVER TIME
query_success_behavior_time <- inner_join(query_per_session1, query_success1,by=c("JOB_ID" = "JOB_ID"))%>%
  group_by(JOB_ID)%>%
  select(TIMESTAMP_FORMATTED.x, successful_responses)

filtered_query_success_behavior_time <- na.omit(query_success_behavior_time)

#summarised_query_success_time <- filtered_query_success_behavior_time%>%group_by(TIMESTAMP_FORMATTED.x)%>%
 # summarise(mean_success = mean(successful_responses), n=n())

#arranged_query_success_time <- summarised_query_success_time%>%arrange(desc(mean_success))
filtered_query_success_behavior_time$TIMESTAMP_FORMATTED.x <- as.POSIXct(filtered_query_success_behavior_time$TIMESTAMP_FORMATTED.x, format = "%Y-%m-%d %H:%M:%S")
#EXTRACT MONTH
#filtered_query_success_behavior_time$month <- as.Date(floor_date(filtered_query_success_behavior_time$TIMESTAMP_FORMATTED.x, "month"))
filtered_query_success_behavior_time$month <- format(filtered_query_success_behavior_time$TIMESTAMP_FORMATTED.x, "%b")

#order month 
filtered_query_success_behavior_time$month <- factor(filtered_query_success_behavior_time$month , levels = month.abb)

filtered_query_success_behavior_time_month <- filtered_query_success_behavior_time%>%
   group_by(month)%>%summarise(mean_success = mean(successful_responses))

summary(filtered_query_success_behavior_time_month)


ggplot(filtered_query_success_behavior_time_month, aes(x = month, y = mean_success, group=1)) + 
  geom_line() + 
  labs(x = "Month", y = "Mean success")+geom_point()
 

