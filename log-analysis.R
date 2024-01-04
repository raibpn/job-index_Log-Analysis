install.packages("ggpubr")
install.packages("dplyr")
install.packages("wordcloud")

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
  filter(!is.na(QUERY)) %>%
  select(QUERY,TIMESTAMP_FORMATTED)

search_term_count <- clean_search_logs %>%
  group_by(QUERY)%>%
  summarise(count = n())%>%
  arrange(desc(count))

# Bar chart of top 10 search terms
ggplot(head(search_term_count, 10), aes(x = count, y = QUERY)) +
  geom_col(fill = "blue") +
  xlab("Count") +
  ylab("Search term") +
  ggtitle("Top 10 search terms")

# Line chart of search trends over time
search_logs %>%
  mutate(date = as.Date(TIMESTAMP_FORMATTED, "%Y-%m-%d")) %>%
  count(date) %>%
  ggplot(aes(x = date, y = n)) +
  geom_line(color = "blue") +
  xlab("Date") +
  ylab("Count") +
  ggtitle("Search trends over time")
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#IF THE RECRUITERS ARE USING FILTER OR JUST RELYING ON KEYWORDS WHILE SEARCHING?
search_logs <- query_data
# Remove irrelevant columns
clean_search_logs <- search_logs[,c("QUERY", "TIMESTAMP_FORMATTED")]
# Filter out incomplete data
clean_search_logs <- clean_search_logs %>%
  filter(!is.na(QUERY)) %>%
  filter(!is.na(TIMESTAMP_FORMATTED))

# Format data types
clean_search_logs$TIMESTAMP_FORMATTED <- as.POSIXct(clean_search_logs$TIMESTAMP_FORMATTED, tz = "UTC")

# Load tidytext package
library(tidytext)

# Tokenize search queries
search_tokens <- clean_search_logs %>%
  unnest_tokens(word, QUERY)

# Count the frequency of each keyword
keyword_counts <- search_tokens %>%
  count(word, sort = TRUE)

# Look for patterns in the search queries
filter_queries <- search_tokens %>%
  filter(word %in% c("location", "title", "company", "salary", "experience"))

# Count the frequency of each filter keyword
filter_counts <- filter_queries %>%
  count(word, sort = TRUE)

# Look for queries that do not include any filter keywords
no_filter_queries <- search_tokens %>%
  anti_join(filter_queries, by = "word")

# Count the frequency of each non-filter keyword
no_filter_counts <- no_filter_queries %>%
  count(word, sort = TRUE)
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




##################################################
##################################################
#<----LATER WITH REFINED RESEARCH QUESTIONS----->
##################################################
##################################################
#<------Merge dataframes on common columns (Job_ID and MESSAGE_ID)---->
merged_data <- merge(query_data, industry_data, by = c('JOB_ID', 'MESSAGE_ID'), all.x =TRUE)

#Remove missing values from the df
merged_data <- na.ommit(merged_data)

#Analyze most common job titles and categories
top_job_titles <- query_data %>%
  count(JOB_TITLES, sort = TRUE) %>%
  head(20)

top_categories <- query_data %>%
  count(CATEGORIES, sort = TRUE) %>%
  head(20)

# Visualization (bar plots)
barplot(top_job_titles$n, names.arg = top_job_titles$JOB_TITLES, 
        main = 'Top 10 Job Titles Searched by Recruiters', col = 'skyblue',
        horiz = TRUE)  # Rotate x-axis labels for better visibility


barplot(top_categories$n, names.arg = top_categories$CATEGORIES, 
        main = 'Top 10 Categories Searched by Recruiters', col = 'skyblue',
        horiz = TRUE)  # Rotate x-axis labels for better visibility


# Analyze Search Behavior Across Industry Sectors
industry_query_counts <- merged_data %>%
  group_by(INDUSTRY_SECTOR_NAME) %>%
  summarise(query_count = n())

# Create a horizontal barplot
barplot(industry_query_counts$query_count, 
        names.arg = industry_query_counts$INDUSTRY_SECTOR_NAME, 
        main = 'Search Behavior Across Industry Sectors', 
        col = rainbow(length(unique(industry_query_counts$INDUSTRY_SECTOR_NAME))),
        horiz = TRUE)  # Create a horizontal barplot with different colors

#<------ The above bars show different sub-categories inside industry sectors----->#
# Let's see what are those sub-categories#
# Analyze Subcategories Within Each Industry Sector

subcategories_counts <- merged_data %>%
  group_by(INDUSTRY_SECTOR_NAME, INDUSTRY_DIVISION_NAME) %>%
  summarise(query_count = n())

# Select the top 10 subcategories based on total query count
top_subcategories <- subcategories_counts %>%
  group_by(INDUSTRY_SECTOR_NAME) %>%
  top_n(20, wt = query_count) %>%
  ungroup()

# Create a horizontal barplot with different colors for each subcategory
barplot(top_subcategories$query_count, 
        names.arg = paste(top_subcategories$INDUSTRY_SECTOR_NAME, top_subcategories$INDUSTRY_DIVISION_NAME, sep = " - "), 
        main = 'Top 10 Subcategories Within Each Industry Sector', 
        col = rainbow(length(unique(top_subcategories$INDUSTRY_DIVISION_NAME))),
        ls = 2)

# Identify Query Patterns
query_data$KEYWORDS <- strsplit(query_data$QUERY, ' ')
# Example: Print unique keywords
unique_keywords <- unique(unlist(query_data$KEYWORDS))

# Now we visualize the unique keywords with two different methods
# 1) bar plot visualization
# Count the occurrences of each keyword
keyword_counts <- table(unlist(query_data$KEYWORDS))

# Sort keywords by frequency
sorted_keywords <- sort(keyword_counts, decreasing = TRUE)

# Create a bar plot
barplot(sorted_keywords[1:20], main = "Top 20 Keywords", horiz = TRUE, col = rainbow(20))

# 2) word cloud visualization
library(wordcloud)

# Select the top 15 keywords
top_keywords <- head(sorted_keywords, 15)

# Generate a word cloud for the top 15 keywords
wordcloud(names(top_keywords), freq = top_keywords, min.freq = 1, scale = c(3, 0.5), colors = brewer.pal(8, "Dark2"), cex = 1.5)






#<------Research Question: 5 Librarian effect----->#
# How does the querying behavior of recruiters evolve during a search session? Do recruiters tend to issue more specific queries towards the end
# of a session, akin to the "librarian effect" observed in library searches? ##

# Assuming 'query_data' dataframe is available with columns: 'TIMESTAMP', 'QUERY'
# Convert TIMESTAMP to POSIXct format
query_data$TIMESTAMP <- as.POSIXct(query_data$TIMESTAMP, origin="1970-01-01", format="%Y-%m-%d %H:%M:%S")

# Calculate session duration for each query
query_data_library <- query_data_library %>%
  arrange(TIMESTAMP) %>%
  group_by(JOB_ID, MESSAGE_ID) %>%
  mutate(session_duration = difftime(TIMESTAMP, first(TIMESTAMP), units = "secs"))

# Normalize session duration by query count
query_data_library <- query_data_library %>%
  mutate(normalized_duration = session_duration / n())

# Plotting Query Behavior Over Session Duration
plot(query_data_library$normalized_duration, seq_along(query_data_library$normalized_duration),
     type = 'l', xlab = 'Normalized Session Duration', ylab = 'Query Count',
     main = 'Query Behavior Over Session Duration',
     col = 'blue', lwd = 2)

# Add vertical line to indicate the midpoint of the session
abline(v = 0.5, col = 'red', lty = 2)

# Interpretation:
# The plot visualizes how the number of queries evolves throughout the normalized session duration.
# If there's an increase in specificity towards the end of sessions, it may suggest a librarian-like behavior.

