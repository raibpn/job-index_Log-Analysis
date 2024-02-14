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
library(gridExtra)
library(tidytext)
library(wordcloud)
library(RColorBrewer)


#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#IMPORT ALL THE DATA SETS
query_data <- read_csv("query-data.2021.csv")
industry_data <- read_csv("industry-data.csv")
response_data <- read_csv("response-data.2021.csv")
summary(query_data)

head(query_data)
total_values<- nrow(query_data)
total_missing_value_in_query<-sum(is.na(query_data$QUERY))

#<-------PERCENTAGE OF MISSING VALUES----------->
missing_percentage <- colMeans(is.na(query_data))*100
print(missing_percentage)
typeof(query_data$NOTICE_PERIOD)
unique(query_data$NOTICE_PERIOD)

total_values_industry <- nrow(industry_data)
total_missing_value_in_industry <- sum(is.na(industry_data))
missing_percentage_industry <- colMeans(is.na(industry_data))*100
print(missing_percentage_industry)

missing_percentage_response <- colMeans(is.na(response_data))*100
print(missing_percentage_response)
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#TOP 10 SEARCH RESULTS#

search_logs <- query_data


# Remove irrelevant queries
query_without_meaning <- c("()", "+()", "+")

clean_search_logs <- search_logs %>%
  filter(!is.na(QUERY)) %>%
  select(QUERY,TIMESTAMP_FORMATTED)

clean_search_logs <- search_logs %>%
  filter(!is.na(QUERY) && !QUERY %in% query_without_meaning) %>%
  select(QUERY,TIMESTAMP_FORMATTED)



#<---------------------------------------------->#
#<------ WHAT ARE THE MOST QUERIED TERMS THAT IS UNIQUE -------->#
#<---------------------------------------------->#
search_term_count <- clean_search_logs %>%
  group_by(QUERY)%>%
  summarise(count = n())%>%
  arrange(desc(count))


# Bar chart of top 20 search terms with customized aesthetics
ggplot(head(search_term_count, 20), aes(x = count, y = reorder(QUERY, -count), fill = count)) +
  geom_col() +
  scale_fill_viridis_c() +  # Use a color scale for better separation
  labs(title = "Top 20 Search Terms",
       x = "Search Term Count",
       y = "Search Term") +
  theme_minimal() +
  theme(axis.text.y = element_text(size = 8))  


#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#IF THE RECRUITERS ARE USING FILTER OR JUST RELYING ON KEYWORDS WHILE SEARCHING?
# <------- VISUALIZE THE KEYWORDS IN WORDCLOUD ------->#

search_logs <- query_data
# Remove irrelevant columns
clean_search_logs <- search_logs[,c("QUERY", "TIMESTAMP_FORMATTED")]

# Tokenize search querieshttp://127.0.0.1:34093/graphics/63178fbd-d073-4aea-9edc-94e5de9a9dc5.png
keyword_counts <- clean_search_logs %>%
  filter(!is.na(QUERY)) %>% # should have already filtered..
  unnest_tokens(word, QUERY) %>%
  count(word, sort = TRUE)

# Generate word cloud
# Generate word cloud with enhanced visuals
wordcloud(words = keyword_counts$word,
          freq = keyword_counts$n,
          scale = c(3, 0.5),
          min.freq = 1,
          colors = brewer.pal(8, "Dark2"),
          random.order = FALSE,  # Display words in descending frequency order
          rot.per = 0.35,        # Control rotation of words
          min.word.length = 3,   # Set a minimum word length
          random.color = TRUE)   # Use random colors for words

# ggtitle("Kewords in Search Queries") DON'T KNOW HOW TO PROVIDE TITLE FOR THE WORDCLOUD




#<--------------------------------------------------------------->#
#<----THE BELOW CODE GIVES US FACTORS WHICH ARE MOSTLY USED------>#
# Count the frequency of each filter keyword
#<--------------------------------------------------------------->#
# Look for patterns in the search queries
search_tokens <- clean_search_logs %>%
  unnest_tokens(word, QUERY)

filter_queries <- search_tokens %>%
  filter(word %in% c("location", "title", "company", "salary", "experience", "education"))

filter_counts <- filter_queries %>%
  count(word, sort = TRUE)

# Calculate percentages
filter_counts <- filter_counts %>%
  mutate(percentage = n / sum(n) * 100)

# VISUALIZE IN GGPLOT
ggplot(filter_counts, aes(x = reorder(word, -n), y = n)) +
  geom_col(fill = "blue") +
  geom_text(aes(label = paste0(round(percentage, 1), "%")), vjust = -0.5, color = "black") +
  xlab("Search Factors") +
  ylab("Count") +
  ggtitle("Frequency of Search Factors") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust x-axis labels for better readability

# Look for queries that do not include any filter keywords
no_filter_queries <- search_tokens %>%
  anti_join(filter_queries, by = "word")

# Count the frequency of each non-filter keyword
no_filter_counts <- no_filter_queries %>%
  count(word, sort = TRUE)
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#(Here if we see same queries can be seen in more than one session also
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

session_length_industry_summary <- session_length_by_industry%>%arrange(desc(mean_length_seconds))


#visualize session_length_industry_summary in histogram
ggplot(data=session_length_industry_summary,aes(x = mean_length_seconds, y= reorder(INDUSTRY_SECTOR_NAME, mean_length_seconds),
                                 fill=mean_length_seconds))+geom_bar(stat = "identity")+
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
  summarise(success_rate = mean(successful_responses)) #CHECK IF TOP 10 SUCCESS RATE IS FOR COMPANY WHICH HAVE HIGHEST QUERIES OR

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
# CORRELATION IS -0.6510223 RESEARCH WHAT DOES THIS MEAN?

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

# Calculate percentage BUT IT IS NOT WORKING NOW
#top_frequent_query_success$percentage_success <- top_frequent_query_success$mean_success * 100

ggplot(top_frequent_query_success, aes(x = reorder(QUERY, -mean_success), y = mean_success)) +
  geom_bar(stat = "identity", fill = "skyblue") +
  labs(title = "Top Most Successful Queries",
       x = "Queries",
       y = "Mean Success Rate") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        axis.title.x = element_blank(),  # Remove x-axis label for better readability
        plot.margin = margin(0.2, 0.2, 0.2, 0.2, "cm"))  # Adjust plot margin for a smaller plot

#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#QUERY SUCCESS BEHAVIOR CHANGED OVER TIME - IS IT GIVING TIME SERIES SUCCESS RATE BY MONTH? THEN NEED TO FIND QUERY COUNTER IN THE SAME TIME-FRAME
# TO FIND OUT IT THE SUCCESS RATE IS DEPENDENT TO THE NUMBER OF QUERY?
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

# RQ.1 -> What is the most used query or search terms used by recruiters?
# RQ.2 -> How does the session length/duration vary in different industry sectors?
# RQ.3 -> Search behavior of Recruiters across industry sectors?
# RQ.4 -> What are the factors determining search success rate? language skill, education level, salary preferences etc
# RQ.5 -> What is the impact of pandemic in search behavior of recruiters? Did it have any effect?
# RQ.6 -> Are there any specific patterns on how the recruiters search for candidates? Do they reformulate/reuse the same queries?
# RQ.7 -> How does the behavior or recruiter vary in search-session? Does it show any library-effect for example they start using more complex-
# and specific queries during the end of the session?

##################################################
##################################################
#POSSIBLE RESEARCH QUESTION -> MOST COMMON FILTERS USED AND HOW THEY VARY?
######<---START---->########
# Merge data
merged_data <- merge(query_data, industry_data, by = c('JOB_ID', 'MESSAGE_ID'), all.x = TRUE)
# Analyze filter usage
# Analyze filter usage
filter_counts <- merged_data %>%
  group_by(CATEGORIES, EMPLOYMENT_GROUPS) %>%
  summarise(query_count = n())

# Get the top 20 categories based on query_count
top_20_categories <- filter_counts %>%
  arrange(desc(query_count)) %>%
  slice_head(n = 20)

print(top_20_categories)

# Bar plot for the top 20 categories
bar_plot_top_20 <- ggplot(top_20_categories, aes(x = CATEGORIES, y = query_count, fill = EMPLOYMENT_GROUPS)) +
  geom_bar(stat = "identity", position = "dodge") +
  labs(title = 'Top 20 Filter Usage Analysis',
       x = 'Categories',
       y = 'Query Count',
       fill = 'Employment Groups') +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))  # Adjust x-axis labels for better readability

print(bar_plot_top_20)
############<--END--->#####################

#<------Merge dataframes on common columns (Job_ID and MESSAGE_ID)---->
merged_data <- merge(query_data, industry_data, by = c('JOB_ID', 'MESSAGE_ID'), all.x =TRUE)

#Remove missing values from the df
merged_data <- na.omit(merged_data)

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


########################################
# <----- Identify Query Patterns ------>#
#########################################

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
install.packages("tm")
library(wordcloud)
# Select the top 15 keywords
top_keywords <- head(sorted_keywords, 15)

# Generate a word cloud for the top 15 keywords
wordcloud(names(top_keywords), freq = top_keywords, min.freq = 1, scale = c(3, 0.5), colors = brewer.pal(8, "Dark2"), cex = 1.5)

# Analyze Query Length
query_data$QUERY_LENGTH <- nchar(query_data$QUERY)

# Visualization (histogram)
# Set up a smaller plotting device
par(mfrow = c(1, 1), mar = c(3, 3, 2, 1))

hist(query_data$QUERY_LENGTH, breaks = 20, main = 'Distribution of Query Lengths', xlab = 'Query Length')

# Explore Geographic Trends
# Count the occurrences of each location
location_counts <- table(unlist(strsplit(as.character(query_data$LOCATION_IDS), ',')))

# Sort locations by frequency
sorted_locations <- sort(location_counts, decreasing = TRUE)

# Create a bar plot
barplot(sorted_locations[1:15], main = "Top 15 Locations", las = 2, col = rainbow(15))

# Create a pie chart
pie(sorted_locations[1:15], labels = names(sorted_locations)[1:15], main = "Top 15 Locations", col = rainbow(15))


# Conduct Advanced Text Analysis (NLP)
# Example: Using the 'tm' package for text mining
# (This requires installing the 'tm' package)
# install.packages("tm")
library(tm)

# Create a corpus from query text
corpus <- Corpus(VectorSource(query_data$QUERY))
corpus <- tm_map(corpus, content_transformer(tolower))
corpus <- tm_map(corpus, removePunctuation)
corpus <- tm_map(corpus, removeNumbers)
corpus <- tm_map(corpus, removeWords, stopwords('english'))

# Example: Create a word cloud
wordcloud(words = unlist(corpus), min.freq = 5, scale=c(3,0.5), random.order=FALSE, colors=brewer.pal(8, "Dark2"))

# Cross-Industry Analysis
# Find the top 10 sectors and categories
top_sectors <- head(names(sort(table(merged_data$INDUSTRY_SECTOR_NAME), decreasing = TRUE)), 10)
top_categories <- head(names(sort(table(merged_data$CATEGORIES), decreasing = TRUE)), 10)

# Subset data to include only the top 10 sectors and categories
subset_data <- merged_data[merged_data$INDUSTRY_SECTOR_NAME %in% top_sectors & 
                             merged_data$CATEGORIES %in% top_categories, ]

# Create a table for the subset data
subset_cross_analysis <- table(subset_data$CATEGORIES, subset_data$INDUSTRY_SECTOR_NAME)


# Visualization (heatmap)
# Set up a smaller plotting device with reduced margin
par(mfrow = c(1, 1), mar = c(6, 6, 4, 2))
heatmap(subset_cross_analysis, 
        col = heat.colors(length(subset_cross_analysis)), 
        cexCol = 1.2, 
        cexRow = 1.2, 
        keysize = 1.5,
        main = "Top 10 Sectors and Categories Heatmap")




#<------Research Question:7 Librarian effect----->#
# How does the querying behavior of recruiters evolve during a search session? Do recruiters tend to issue more specific queries towards the end
# of a session, akin to the "librarian effect" observed in library searches? ##

# Assuming 'query_data' dataframe is available with columns: 'TIMESTAMP_FORMATTED', 'JOB_ID', 'MESSAGE_ID', 'normalized_duration'

# Convert TIMESTAMP_FORMATTED to POSIXct format
#query_data$TIMESTAMP_FORMATTED <- as.POSIXct(query_data$TIMESTAMP_FORMATTED, format="%Y-%m-%d %H:%M:%S")

print(class(query_data$TIMESTAMP_FORMATTED))

# Calculate session duration for each query
# Calculate session duration for each query
query_data_library <- query_data %>%
  arrange(TIMESTAMP_FORMATTED) %>%
  group_by(JOB_ID, MESSAGE_ID) %>%
  mutate(session_duration = difftime(TIMESTAMP_FORMATTED, first(TIMESTAMP_FORMATTED), units = "secs"),
         normalized_duration = session_duration / n())


# Check and convert 'normalized_duration' to numeric
query_data_library$normalized_duration <- as.numeric(as.character(query_data_library$normalized_duration))

# Find the range of normalized_duration
min_value <- min(query_data_library$normalized_duration, na.rm = TRUE)
max_value <- max(query_data_library$normalized_duration, na.rm = TRUE)

# Create a histogram with breaks spanning the range of normalized_duration
hist_data <- hist(query_data_library$normalized_duration, breaks = seq(min_value, max_value, by = 0.25), plot = FALSE)

# Plot the histogram with time frame on the y-axis and query counts on the x-axis
barplot(hist_data$counts, names.arg = hist_data$breaks[-length(hist_data$breaks)],
        xlab = 'Normalized Session Duration', ylab = 'Query Count',
        main = 'Query Behavior Across Session Duration',
        col = rainbow(length(hist_data$counts)), border = 'blue')



##############################################################
##############################################################
##############################################################

# Assuming 'query_data_library' dataframe is available with columns: 'normalized_duration'

# Calculate session duration for each query
query_data_library <- query_data %>%
  arrange(TIMESTAMP_FORMATTED) %>%
  group_by(JOB_ID, MESSAGE_ID) %>%
  mutate(session_duration = difftime(TIMESTAMP_FORMATTED, first(TIMESTAMP_FORMATTED), units = "secs"),
         normalized_duration = session_duration / n())

# Check and convert 'normalized_duration' to numeric
query_data_library$normalized_duration <- as.numeric(as.character(query_data_library$normalized_duration))

# Find the range of normalized_duration
min_value <- min(query_data_library$normalized_duration, na.rm = TRUE)
max_value <- max(query_data_library$normalized_duration, na.rm = TRUE)

# Create a histogram with breaks spanning the range of normalized_duration
hist_data <- hist(query_data_library$normalized_duration, breaks = seq(min_value, max_value, by = 0.25), plot = FALSE)

# Print the top 10 values
top_10_values <- head(sort(hist_data$counts, decreasing = TRUE), 10)
cat("Top 10 Values:", top_10_values, "\n")

# Create a new histogram with top 10 values
top_10_hist_data <- hist_data
top_10_hist_data$counts <- head(hist_data$counts, 10)

# Plot the histogram with time frame on the y-axis and query counts on the x-axis
barplot(top_10_hist_data$counts, names.arg = top_10_hist_data$breaks[-length(top_10_hist_data$breaks)],
        xlab = 'Normalized Session Duration', ylab = 'Query Count',
        main = 'Query Behavior Across Session Duration (Top 10)',
        col = rainbow(length(top_10_hist_data$counts)), border = 'blue')

###########################################################################
###########################################################################
#<----- RQ.5 Impact of the Pandemic on Recruiter Success ------>
###########################################################################
###########################################################################

# Assuming 'response_data' dataframe is available
str(response_data$TIMESTAMP_FORMATTED)

# Bar plot using ggplot2
# Convert TIMESTAMP_FORMATTED to Date if needed
response_data$TIMESTAMP_FORMATTED <- as.Date(response_data$TIMESTAMP_FORMATTED)
query_data$TIMESTAMP_FORMATTED <- as.Date(query_data$TIMESTAMP_FORMATTED)

# Create 'pandemic_impact_analysis' dataframe
pandemic_impact_analysis <- query_data %>%
  left_join(response_data, by = "JOB_ID") %>%
  mutate(pandemic_period = ifelse(
    pandemic_impact_analysis$TIMESTAMP_FORMATTED >= as.Date('2020-01-01') &
      pandemic_impact_analysis$TIMESTAMP_FORMATTED <= as.Date('2021-12-31'),
    'During Pandemic',
    'Before Pandemic'
  ))

# Create a dataframe with success rate during and before the pandemic
success_rate_data <- pandemic_impact_analysis %>%
  group_by(pandemic_period) %>%
  summarise(success_rate = sum(RESPONSE_TYPE == 1, na.rm = TRUE) / n())

# Bar plot for success rate during and before the pandemic
bar_plot <- ggplot(success_rate_data, aes(x = pandemic_period, y = success_rate, fill = pandemic_period)) +
  geom_bar(stat = "identity") +
  labs(title = 'Success Rate During and Before Pandemic',
       x = 'Pandemic Period',
       y = 'Success Rate',
       fill = 'Pandemic Period') +
  scale_fill_manual(values = c('lightblue', 'lightgreen')) +
  scale_y_continuous(labels = scales::percent_format(scale = 1))  # Format y-axis labels as percentages

print(bar_plot)

#<-------------------->
#<-------------------->
#<-------------BEFORE PANDEMIC--------------->
#<-------------------->

# Convert TIMESTAMP_FORMATTED to Date if needed
response_data$TIMESTAMP_FORMATTED <- as.Date(response_data$TIMESTAMP_FORMATTED)
query_data$TIMESTAMP_FORMATTED <- as.Date(query_data$TIMESTAMP_FORMATTED)

# Create 'before_pandemic_data' dataframe
before_pandemic_data <- query_data %>%
  left_join(response_data, by = "JOB_ID") %>%
  filter(
    query_data$TIMESTAMP_FORMATTED < as.Date('2020-03-01') &  # Date before the pandemic
      !is.na(RESPONSE_TYPE)  # Exclude rows without a response
  )

# Print the first few rows of before_pandemic_data
print(head(before_pandemic_data))


# Create a dataframe with success rate before the pandemic
success_rate_data_before_pandemic <- before_pandemic_data %>%
  group_by(JOB_ID) %>%
  summarise(success_rate = sum(RESPONSE_TYPE == 1, na.rm = TRUE) / n())

# Bar plot for success rate before the pandemic
bar_plot_before_pandemic <- ggplot(success_rate_data_before_pandemic, aes(x = success_rate)) +
  geom_histogram(binwidth = 0.05, fill = 'lightblue', color = 'black', alpha = 0.7) +
  labs(title = 'Success Rate Before Pandemic',
       x = 'Success Rate',
       y = 'Frequency')

print(bar_plot_before_pandemic)



#<--------------------------->
#<----------###################----------------->
#<--------------------------->
#<------ Impact on success rate during pandemic -------> 
# Time series plot using ggplot2
# Time series plot for success rate during the pandemic (month-wise) and before pandemic
monthly_success_rate_data <- pandemic_impact_analysis %>%
  filter(RESPONSE_TYPE == 1, !is.na(RESPONSE_TYPE)) %>%
  group_by(pandemic_period, month = format(TIMESTAMP_FORMATTED, "%Y-%m")) %>%
  summarise(success_rate = sum(RESPONSE_TYPE == 1) / n())

# Time series plot using ggplot2 with facets
time_series_plot <- ggplot(monthly_success_rate_data, aes(x = month, y = success_rate)) +
  geom_line() +
  labs(title = 'Success Rate - Monthly Basis',
       x = 'Month',
       y = 'Success Rate') +
  facet_wrap(~pandemic_period, scales = 'free_y', nrow = 2)  # Facet by pandemic period

# Combine bar plot and time series plot
library(gridExtra)
grid.arrange(bar_plot, time_series_plot, ncol = 1)

language_data <- query_data

language_level_data <- language_data %>%
  group_by(LANGUAGE_LEVEL) %>%
  summarise(count = n())%>%
  arrange(desc(count))


language_type_level_data <- language_data %>%
  group_by(LANGUAGE_SKILLS, LANGUAGE_LEVEL) %>%
  summarise(count = n())%>%
  arrange(desc(count))



language_skills_count <- language_data %>%
  length(str_split(LANGUAGE_SKILLS, "|")) %>%
  summarise(count = n())%>%
  arrange(desc(count))


length(unlist(str_split("da|en","\\|")))

# filter na later
language_skills_count <- language_data %>%
  group_by(langs = (str_count(LANGUAGE_SKILLS,"\\|")+1)) %>%
  summarise(count = n())%>%
  arrange(desc(count))

language_skills_count <- language_data %>%
  group_by((Langs = (length(unlist(str_split(LANGUAGE_SKILLS,"\\|")))))) %>%
  summarise(count = n()) %>%
  arrange(desc(count))


language_type_level_data <- language_data %>%
  group_by(LANGUAGE_SKILLS, LANGUAGE_LEVEL) %>%
  summarise(count = n())%>%
  arrange(desc(count))

summary_qry <- summary(query_data)

colSums(is.na(query_data))

filters_used <- colSums(!is.na(query_data))

query_data_job_cnt <-  query_data %>%
  add_count(JOB_ID)

query_session_length_pct <- query_data_job_cnt %>%
  mutate(QUERY_LENGTH = length(QUERY),
         normalized_duration = session_duration / n)

job_id_max_min_ts <- query_data %>%
  group_by(JOB_ID) %>%
  summarise(min_ts = min(TIMESTAMP),
            max_ts =max(TIMESTAMP))

query_session_length_pct <- query_data %>%
  inner_join_by(JOB_ID,)


query_session_length_pct <- inner_join(query_data,job_id_max_min_ts, by="JOB_ID")%>%
  mutate(QUERY_LENGTH = nchar(ifelse(is.na(QUERY),"",QUERY)),
         duration_pct = round((((TIMESTAMP - min_ts) / ifelse((max_ts - min_ts) > 0, (max_ts - min_ts), 1)) * 100), digits = 0))

query_session_length_pct_grp <- query_session_length_pct %>%
  group_by(duration_pct) %>%
  summarize(avg_query_length = mean(QUERY_LENGTH))

# Bar plot for success rate before the pandemic
bar_plot <- ggplot(query_session_length_pct_grp, aes(x = duration_pct, y=avg_query_length)) +
  geom_bar(stat="identity")
#  labs(title = 'Success Rate Before Pandemic',
 #      x = 'Success Rate',
  #     y = 'Frequency')

print(bar_plot)



# Create new table with query length counts
# Create duration in percent by job_id
# Join by job_id
# What to plot -> Each query run (1 to 100 convert) to %
# Another axis query length
# Normalize how? to 1 to 100





##########################
##########################
#########################
#<-------------------- CLEAR ONE ------------------>#
##########################
##########################
#########################
###########################
##########################
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
library(gridExtra)
library(tidytext)
library(wordcloud)
library(RColorBrewer)


#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#<-------------------------------------------------------->#
#IMPORT ALL THE DATA SETS
query_data <- read_csv("query-data.2021.csv")
industry_data <- read_csv("industry-data.csv")
response_data <- read_csv("response-data.2021.csv")
summary(query_data)
#GROUP 3
session_duration <- query_data %>%
  group_by(JOB_ID) %>%
  summarize(duration = (max(TIMESTAMP)- min(TIMESTAMP))/60)

#Session duration types
session_duration_types <- session_duration %>%
  #Make sure sessions longer than 4 hours are filtered out
  filter(duration < 240) %>%
  #Add new column that either contains the label 'matching' or 'recruiting'
  mutate(session_type = ifelse(duration > 60, "recruiting","matching"))

#Figure 1 : Histogram of recruiting session duration (minutes)
session_duration_types %>% 
  filter(session_type == "recruiting")%>%
  ggplot(aes(x=duration, fill = "recruiting"))+
  geom_histogram(position = "identity", binwidth = 1, color = "black")+
  labs(title = "Histogram of recruiting session duration (minutes)",
       x= "Session duration (minutes)",
       y= "Frequency")+
  scale_x_continuous(breaks = c(0, 1, 100, 150, 200, 240))+
  scale_fill_manual(values = c("BLUE"), name= "Session Type")+
  theme_minimal()+
  theme(legend.position = "top")



#Figure 2" Histogram of matching session duration (minutes)
session_duration_types %>% 
  filter(session_type == "matching")%>%
  ggplot(aes(x=duration, fill = "matching"))+
  geom_histogram(position = "identity", binwidth = 1, color = "black")+
  labs(title = "Histogram of matching session duration (minutes)",
       x= "Session duration (minutes)",
       y= "Frequency")+
  scale_x_continuous(breaks = c(0, 20, 40, 60))+
  scale_fill_manual(values = c("#56B4E9"), name= "Session Type")+
  theme_minimal()+
  theme(legend.position = "top")

#RESPONSE RATE ANALYSIS
#<---------Contacted Candidates Analysis------------------>
responded <- response_data %>%
  filter(RESPONSE_TYPE %in% c("-1", "1")) %>%
  group_by(JOB_ID, RESPONSE_TYPE) %>%
  summarise(count = n()) %>%
  group_by(JOB_ID) %>%
  summarise(responded_total = sum(count))

contacted <- response_data %>%
  group_by(JOB_ID, RESPONSE_TYPE) %>%
  summarise(count = n()) %>%
  group_by(JOB_ID) %>%
  summarise(contacted_total = sum(count))

result_table <- left_join(responded, contacted, by = "JOB_ID")

result_table <- result_table %>%
  mutate(rate = (responded_total / contacted_total) * 100)

session_duration_types_result_table <- inner_join(session_duration_types, result_table)

mean_response_data_session_type <- session_duration_types_result_table %>%
  group_by(session_type) %>%
  summarise(mean_rate = mean(rate, na.rm = TRUE))

mean_response_data <- response_data %>%
  group_by(RESPONSE_TYPE) %>%
  summarise(mean_rate = mean(rate))


session_duration_types_response_data <- inner_join(session_duration_types, response_data)

response_data <- response_data %>%
  count(JOB_ID, RESPONSE_TYPE) %>%
  group_by(JOB_ID) %>%
  mutate(total = sum(n)) %>%
  mutate(rate=(n/total) * 100)

response_data$RESPONSE_TYPE[response_data$RESPONSE_TYPE == -1] <- 'negative'
response_data$RESPONSE_TYPE[response_data$RESPONSE_TYPE == 0] <- 'none'
response_data$RESPONSE_TYPE[response_data$RESPONSE_TYPE == 1] <- 'positive'

#Figure 3: Mean response rate (%) by session types
mean_response_data_session_type <- session_duration_types_response_data %>%
  group_by(session_type) %>%
  summarise(mean_rate = mean(rate))

mean_response_data_session_type %>%
  ggplot()+
  labs(title = "Mean of the contacted candidates (%) by session types",
       x = "Session type",
       y = "Mean of the contacted candidates (%) issued per session",
       fil = "Session type")+
  geom_bar(aes(x= session_type, y = mean_rate, fill = session_type),
           stat = "identity", show.legend = FALSE) +
  scale_fill_manual(values = c("#0072B2", "#56B4E9"))+
  theme_minimal()

#Figure:4 Mean total count of responses by session types
mean_total_responses <- session_duration_types_response_data %>%
  group_by(session_type) %>%
  summarize(mean_total = mean(total))

ggplot(mean_total_responses, aes(x = session_type, y = mean_total,
                                 fill = session_type)) + 
  geom_bar(stat = "identity", position = "dodge", show.legend = FALSE) + 
  labs(title = "Mean total count of contacted candidates by session types", 
       x = "Session type", 
       y = "Mean total count of contacted candidates issued per session")+
  scale_fill_manual(values = c("#0072B2", "#56B4E9"))+
  theme_minimal()

#Calculate the Response rate
#<---------Response Rate Analysis------------------>
#Calculate the total number of contacted candidates
total_contacted <- sum(response_data$RESPONSE_TYPE != -1,0,1) %>%view

print(total_contacted)

#Calculate the number of responded candidates
responded_candidates <- sum(response_data$RESPONSE_TYPE == 1 |
response_data$RESPONSE_TYPE == -1) %>%view

print(responded_candidates)

#Calculate response rate
response_rate <- responded_candidates / total_contacted
print(response_rate)

#Figure 8: Bar plot mean response rate
mean_response_data_session_type %>%
  ggplot(aes(x=session_type, y=mean_rate, fill = session_type))+
  geom_bar(stat = "identity")+
  geom_text(aes(label = paste0(round(mean_rate, 1), "%")), 
            position = position_stack(vjust = 0.5), 
            size = 5, color = "black") +  
  labs(title = "Mean response rate(%) of responded candidates by session types",
       x= "Session type",
       y= "Mean response rate(%)",
       fill = "Session type")+
  scale_fill_manual(values = c("#0072B2", "#56B4E9"))+
  theme_minimal()

#<-----------------------------------NUMBER OF QUERIES------------------------------------------------------------->

#Counts how many queries were executed during a search session
session_query_counts <- query_data %>%
  group_by(JOB_ID)%>%
  summarise(query_count = n())

session_duration_types_query_counts <- inner_join(session_duration_types, session_query_counts)

#Histogram Figure: 9
session_duration_types_query_counts %>%
  #Filter out all job ads with more than 100 queries
  filter(query_count <= 100) %>%
  ggplot(aes(x=query_count, fill= session_type))+
  geom_histogram(binwidth = 1, color= "black")+
  #this makes the x-axis go from 0 to 100 in steps of 20
  scale_x_continuous(breaks = seq(0, 100, 20))+
  #change the text of the axis labels and the title
  labs(x = "Query total count issued per session",
       y = "Frequency",
       title= "Query total count distribution by session type",
       fill = "Session type")+
  scale_fill_manual(values = c("#0072B2", "#56B4E9"))+
  theme_minimal()

#<------------------------------------TYPE OF QUERIES---------------------------------------------------------------->
#Counts how many characters were used during a search session
session_character_counts <- query_data %>%
  group_by(JOB_ID) %>%
  summarise(character_count = nchar(QUERY))

session_duration_types_character_counts <- inner_join(session_duration_types,
                                                      session_character_counts)
#Figure 13: Scatter plot
session_duration_types_character_counts %>%
  filter(character_count <= 1000) %>%
  ggplot(aes(x=duration, y= character_count, color = session_type))+
  geom_point()+
  labs(title = "Scatter olot of Duration vs. Character Count",
       x = "Duration",
       y = "Character Count")

#Histogram for above scatter plot
# Histogram Figure: 14
session_duration_types_character_counts %>%
  filter(character_count <= 1000) %>%
  ggplot(aes(x=character_count, fill = session_type))+
  geom_histogram(binwidth = 50, color= "black", position = "dodge")+
  scale_fill_manual(values = c("#0072B2", "#56B4E9"))+
  labs(title = "Character Count Distribution by Session Type",
       x = "Character Count",
       y = "Frequency",
       fill = "Session Type")+
  theme_minimal()




#<-------------------------------------FILTER APPLICATION AND TIME ALLOCATION------------------------------------------>
session_duration <- query_data %>%
  group_by(JOB_ID) %>%
  summarize(duration = (max(TIMESTAMP)- min(TIMESTAMP))/60)

#Session duration types
session_duration_types <- session_duration %>%
  #Make sure sessions longer than 4 hours are filtered out
  filter(duration < 240) %>%
  #Add new column that either contains the label 'matching' or 'recruiting'
  mutate(session_type = ifelse(duration > 60, "recruiting","matching"))

#Extracting relevant columnns used for flter
filtered_data <- query_data %>%
  select(JOB_ID, 
         LOCATION_IDS, 
         JOB_TITLES, 
         CATEGORIES, 
         EDU_LEVEL_MIN, EDU_LEVEL_MAX,
         WORK_EXP_MIN, WORK_EXP_MAX,
         MGR_EXP_MIN, MGR_EXP_MAX,
         SALARY_MIN, SALARY_MAX,
         LANGUAGE_SKILLS,
         LANGUAGE_LEVEL,
         NOTICE_PERIOD,
         EMPLOYMENT_GROUPS)

#Replacing null values with a specific label (e.g., "unused)
search_data_filled <- filtered_data %>%
  mutate(across(everything(), ~ifelse(is.na(.), "unused", .)))

search_data_long <- search_data_filled %>%
  mutate(across(-JOB_ID, as.character)) %>%
  pivot_longer(cols = -JOB_ID, names_to = "filter_type", values_to = "filter_value")

#Count occurences
usage_changes <- search_data_long %>%
  filter(filter_value != 'unused') %>%
  group_by(JOB_ID, filter_type) %>%
  summarise(total_count = n())

#Print the summary
print(usage_changes)

session_duration_types_filters_counts <- inner_join(session_duration_types, usage_changes)

#<----------SCATTER PLOT---------------->
session_duration_types_filters_counts %>%
  filter(total_count <= 100) %>%
  ggplot(aes(x = duration, y = total_count, color = session_type)) +
  geom_point() +
  labs(title = "Scatter Plot of Duration vs. Filter Count",
       x = "Duration",
       y = "Filter COunt") + 
  theme_minimal()

#Downsampled scatter plot with smoothing curve

session_duration_types_filters_counts %>%
  filter(total_count <= 100) %>%
  ggplot(aes(x = duration, y = total_count, color = session_type)) +
  geom_point() +
  geom_smooth(method = "auto, se = FALSE") +
  geom_smooth(color = "black") +
  labs(title = "Scatter Plot of Duration vs. Filter Count",
       x = "Duration",
       y = "Filter Count") +
  theme_minimal()

#Downsampled scatter plot with smoothing curve
session_duration_types_filters_counts %>%
  filter(total_count <= 100) %>%
  sample_n(5000, replace = FALSE) %>%
  ggplot(aes(x = duration, y = total_count, color = session_type)) + 
  geom_point(alpha = 0.5) +
  geom_smooth(method = "auto", se = FALSE) +
  geom_smooth(color = "black") +
  labs(title = "Downsampled Scatter Plot of Duration with Smoothing Curve vs Filter Count",
       x="Duration",
       y = "Filter Count") +
  scale_color_manual(values = c("#0072B2", "#56B4E9", "turquoise")) +
  theme_minimal()
  
  
#<----------------------------HISTOGRAM-------------------------------->
session_duration_types_filters_counts %>%
  filter(total_count <=40) %>%
  ggplot(aes(x = total_count, fill =session_type)) +
  geom_histogram(binwidth = 5, position = "identity", alpha = 0.6, color = "black") +
  geom_density(alpha = 0.3, aes(color = session_type, fill = session_type)) + 
  labs(title = "Histogram of Filter COunt by Session TYpe",
       x = "Filter Count",
       y = "Frequency") + 
  scale_fill_brewer(palette = "Set2") +
  scale_color_brewer(palette = "Set2") +
  theme_minimal() +
  theme(legend.position = "right")
  
#<----------------------------BAR CHART----------------------------------->
ggplot(session_duration_types_filters_counts, aes(x = session_type, y = total_count, fill = session_type)) +
  geom_bar(stat = "identity") +
  theme_minimal() + 
  labs(x = "Session Type", y = "Filter total count issued per session") +
  ggtitle("Filter Total Count by Session Type")
  
#Calculating mean filter count by session type
session_duration_types_filters_counts_mean <- session_duration_types_filters_counts %>%
  group_by(session_type) %>%
  summarise(mean_count = mean(total_count))

#Bar chart of mean counts by session types
ggplot(session_duration_types_filters_counts_mean, aes(x = session_type, y = mean_count, fill = session_type)) +
  geom_col()+
  theme_minimal() + 
  labs(x = "Session TYpe", y = "Mean Filter Count Issued per Session") +
  ggtitle("Mean Filter Count by Session Type")

mean_session_duration_types_filters_counts <- session_duration_types_filters_counts %>%
  group_by(filter_type, session_type) %>%
  summarise(mean_filter_total_count = mean(total_count)) %>%
  arrange(desc(mean_filter_total_count))

# Grouped Horizontal Bar plot
mean_session_duration_types_filters_counts %>%
  ggplot(aes(x = reorder(filter_type, mean_filter_total_count),
             y = mean_filter_total_count,
             fill = session_type)) +
  geom_bar(stat = "identity", position = "dodge", show.legend = TRUE) +
  theme_minimal() +
  theme(axis.text.x = element_text(size = 12)) +
  ggtitle("Mean filter total count by filter type") +
  labs(x = "Filter type",
       y = "Mean filter total count issued per session") +
  coord_flip()

  
  
  
  
  
  
  









  