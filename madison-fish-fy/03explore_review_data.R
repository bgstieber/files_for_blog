library(tidyverse)
library(tidytext)

review_data <- read_csv("full_review_data.csv")

review_data <- review_data %>%
  mutate(recommendation = ifelse(grepl("barely", recommendation),
                                 "barely recommend",
                                 ifelse(grepl("highly", recommendation),
                                        "highly recommend",
                                        ifelse(grepl("not", recommendation),
                                                     "not recommend", "recommend"))),
         review_date = lubridate::mdy(review_date)) %>%
  filter(!is.na(review)) # get rid of NA reviews

review_data_distinct <- review_data %>%
  group_by(rest_name, review) %>%
  slice(which.min(review_date)) %>%
  ungroup() %>%
  mutate(review_length = nchar(review)) 


