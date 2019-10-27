library(tidyverse)
library(tidytext)
library(zoo)

review_data <- read_csv("review_analysis_data.csv")

review_data <- review_data %>%
  mutate(recommendation = ifelse(grepl("barely", recommendation),
                                 "barely recommend",
                                 ifelse(grepl("highly", recommendation),
                                        "highly recommend",
                                        ifelse(grepl("not", recommendation),
                                                     "not recommend", "recommend")))) %>%
  mutate(recommend_indicator = ifelse(recommendation %in% c("highly recommend",
                                                            "recommend"), 1,0),
         recommend_numeric = ifelse(
           recommendation == 'highly recommend', 4,
           ifelse(recommendation == 'recommend', 3,
                  ifelse(recommendation == 'barely recommend', 2, 1)))) %>%
  filter(!is.na(review)) %>% # get rid of NA reviews
  mutate(review_month_start_date = lubridate::floor_date(review_date, "month"))
  
  
review_data_distinct <- review_data %>%
  group_by(rest_name, review) %>%
  slice(which.min(review_date)) %>%
  ungroup() 

# monthly data
monthly_summary <- review_data %>%
  group_by(review_month_start_date) %>%
  summarise(count_review = n(),
            percent_recommended = mean(recommend_indicator),
            recommend_indicator = sum(recommend_indicator),
            recommend_numeric = sum(recommend_numeric),
            length_written = sum(review_length),
            avg_polarity = mean(polarity)) %>%
  ungroup() %>%
  mutate_all(list('rollmean_6' = ~ rollmean(., k = 7, fill = NA)))


monthly_summary %>%
  ggplot(aes(review_month_start_date, count_review))+
  geom_line(alpha = 0.5)+
  geom_hline(aes(yintercept = mean(count_review)),linetype = 'dashed')+
  geom_line(aes(y = count_review_rollmean_6), colour = 'blue', size = 1.2)



monthly_summary %>%
  ggplot(aes(review_month_start_date, percent_recommended))+
  geom_line(alpha = 0.5)+
  geom_hline(aes(yintercept = mean(percent_recommended)),linetype = 'dashed')+
  geom_line(aes(y = percent_recommended_rollmean_6),
            colour = 'blue', size = 1.2)

review_data_distinct %>%
  filter(review_length >= 100) %>%
  ggplot(aes(recommend_numeric, polarity, group = recommend_numeric))+
  geom_boxplot()

review_data_distinct %>%
  filter(review_length >= 100) %>%
  ggplot(aes(recommend_indicator, polarity, 
             group = recommend_indicator))+
  geom_boxplot()

fit1 <- lm(polarity ~ factor(recommend_numeric), 
          data = review_data_distinct %>% filter(review_length >= 100))

review_data_distinct %>%
  filter(review_length >= 100) %>%
  ggplot(aes(polarity, recommend_indicator))+
  geom_jitter(alpha = 0.25) +
  stat_smooth()

glm1 <- glm(recommend_indicator ~ polarity,
            data = review_data_distinct %>%
              filter(review_length >= 100),
            family = 'binomial')



tokenized_by_recommend <- review_data_distinct %>%
  filter(review_length >= 100) %>%
  select(review, recommend_indicator) %>%
  mutate(recommend_indicator = ifelse(recommend_indicator == 1, 'Y', 'N')) %>%
  unnest_tokens(word, review) %>%
  anti_join(stop_words)

count_total <- tokenized_by_recommend %>%
  group_by(recommend_indicator) %>%
  summarise(total = n())

frequency <- tokenized_by_recommend %>%
  count(word, recommend_indicator) %>%
  group_by(word) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  spread(recommend_indicator, n, fill = 0) %>%
  mutate_if(is.numeric, list(~(. + 1) / (sum(.) + 1))) %>%
  mutate(logratio = log(Y / N)) %>%
  arrange(desc(logratio))


