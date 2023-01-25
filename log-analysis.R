library(tidyverse)

query_data <- read_csv("query-data.2021.csv")
industry_data <- read_csv("industry-data.csv")

head(query_data)
total_values<- nrow(query_data)

total_missing_value_in_query<-sum(is.na(query_data$QUERY))

