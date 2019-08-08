library(tidyverse)
library(splines)

full_data_files <- dir("data")[grepl("_FULL", dir("data"))]

full_data <- paste0("data//", full_data_files) %>%
  map(~read_csv(.x)) %>%
  set_names(full_data_files)

measure_and_column <- c("avg_driv_dist", 
                        "gir_pct", 
                        "pct_total_money_won", 
                        "sg_app_green", 
                        "sg_around_green", 
                        "sg_off_tee", 
                        "sg_putting", 
                        "sg_tee_to_green", 
                        "total_driving")

money <- full_data$pct_total_money_won_FULL.csv %>%
  mutate(money_won = parse_number(`OFFICIAL MONEY WON`))

money_scaled <-money %>%
  select(year, `PLAYER NAME`, money_won, EVENTS) %>%
  group_by(year) %>%
  mutate(scaled_money = scale(log1p(money_won))) %>%
  ungroup()

sg_tee_to_green <- full_data$sg_tee_to_green_FULL.csv

sg_tee_to_green_minimal <- sg_tee_to_green %>%
  select(year, `PLAYER NAME`,
         'SG:TTG' = AVERAGE, `SG:OTT`, `SG:APR`,`SG:ARG`)

sg_putting <- full_data$sg_putting_FULL.csv

sg_putting_minimal <- sg_putting %>%
  select(year, `PLAYER NAME`, 'SG:PUTT' = AVERAGE)

yoy_performance <- sg_tee_to_green %>%
  select(year, `PLAYER NAME`, AVERAGE) %>%
  arrange(year) %>%
  group_by(`PLAYER NAME`) %>%
  mutate(last_year = lag(year),
         last_year_avg = lag(AVERAGE)) %>%
  filter((last_year + 1) == year) %>%
  ungroup()

yoy_performance %>%
  mutate(change_from_last_year = (AVERAGE - last_year_avg)) %>%
  arrange(desc(abs(change_from_last_year))) %>%
  top_n(25, abs(change_from_last_year))



money_and_sg <- money_scaled %>%
  inner_join(sg_tee_to_green_minimal,
             by = c("year", "PLAYER NAME")) %>%
  inner_join(sg_putting_minimal,
             by = c("year", "PLAYER NAME"))

fit1 <- lm(scaled_money ~ -1 + bs(`SG:TTG`, df = 4), data = money_and_sg)


money_and_sg$pred <- predict(fit1)


money_and_sg %>%
  filter(EVENTS >= 10) %>%
  select(year, scaled_money:`SG:PUTT`) %>%
  gather(variable, value, -year, -scaled_money) %>%
  group_by(year, variable) %>%
  summarise(cor_value = cor(scaled_money, value)) %>%
  ggplot(aes(year, cor_value, colour = variable))+
  geom_line(size = 1.2)+
  stat_smooth(method = 'lm')+
  facet_wrap(~variable)


# create analysis data set
# variables to create
# lagged versions of all variables: 1, 2, and 3 years
# change in lag versions: change from 2 to 1, 3 to 2

lag1 <- function(x) lag(x, 1)

lag2 <- function(x) lag(x, 2)

lag3 <- function(x) lag(x, 3)

lag12_delta <- function(x) lag(x, 1) - lag(x, 2)

lag23_delta <- function(x) lag(x, 2) - lag(x, 3)


money_and_sg2 <- money_and_sg %>%
  filter(EVENTS >= 10) %>%
  arrange(year) %>%
  group_by(`PLAYER NAME`) %>%
  mutate_at(vars(year, scaled_money:`SG:PUTT`, EVENTS),
            list(lag_1 = lag1, lag_2 = lag2, lag_3 = lag3)) %>%
  mutate_at(vars(scaled_money:`SG:PUTT`),
            list(lag12_delta = lag12_delta,
                 lag23_delta = lag23_delta))


full_model_data <- money_and_sg2 %>%
  select(year, `PLAYER NAME`,
         money_won,
         EVENTS,
         scaled_money,
         contains("lag")) %>%
  na.omit() %>%
  filter(year <= 2018) %>%
  filter((year == (year_lag_1 + 1)),
         (year == (year_lag_2 + 2)),
         (year == (year_lag_3 + 3))) %>%
  select(-year_lag_1, -year_lag_2, -year_lag_3) %>%
  group_by(year) %>%
  mutate(rank_money = percent_rank(money_won)) %>%
  mutate(quartile_group = ifelse(
    rank_money <= 0.25, "0-25%",
    ifelse(rank_money <= 0.5, "25-50%",
           ifelse(rank_money <= .75, "50-75%", "75-100%")))) %>%
  ungroup() %>%
  mutate(money_per_event = money_won / EVENTS,
         log_money_per_event = log(money_per_event),
         scaled_money_per_event = scaled_money / EVENTS)

write_data <- FALSE

if(write_data){
  
  write_csv(full_model_data,
            "data//modeling_data.csv")
  
}

full_model_data %>%
  ungroup() %>%
  select(scaled_money, contains("lag")) %>%
  gather(variable, value, -scaled_money) %>%
  group_by(variable) %>%
  summarise(cor_value = cor(scaled_money, value)) %>%
  arrange(desc(cor_value)) %>%
  ggplot(aes(reorder(variable, cor_value), cor_value))+
  geom_col()+
  coord_flip()

# make training and test split
set.seed(1)

training_samps <- sample(nrow(full_model_data),
                         0.75 * nrow(full_model_data))

train_data <- full_model_data[training_samps,]

train_X <- train_data %>%
  select(contains("lag"))

train_X_lag1 <- train_data %>%
  select(contains("lag_1"))

train_X_lag1_2 <- train_data %>%
  select(contains("lag_1"), contains("lag_2"), contains("lag12_delta"))

train_X_mat <- as.matrix(train_X)

train_y <- train_data$quartile_group

train_y_top50_perc_flag <- as.character(train_data$rank_money >= 0.5)

train_y_top50_perc_flag_factor <- as.factor(train_y_top50_perc_flag)

test_data <- full_model_data[-training_samps,]

test_X <- test_data %>%
  select(contains("lag"))

test_X_mat <- as.matrix(test_X)

test_Y <- test_data$quartile_group

test_y_top50_perc_flag <- as.character(test_data$rank_money >= 0.5)

test_y_top50_perc_flag_factor <- as.factor(test_y_top50_perc_flag)

library(glmnet)

cv_glm1 <- cv.glmnet(x = train_X_mat,
                     y = train_y_top50_perc_flag,
                     family = 'binomial')

glmn1 <- glmnet(x = train_X_mat,
                y = train_y_top50_perc_flag,
                family = 'binomial',
                lambda = cv_glm1$lambda.min)

preds_glmn <- predict(glmn1, newx = test_X_mat, type = 'response')
preds_glmn_flag <- preds_glmn >= 0.5

library(randomForest)
library(e1071)
## random forest training

rf1 <- randomForest(x = train_X_mat,
                    y = train_y_top50_perc_flag_factor,
                    ntree = 1000,
                    mtry = 7,
                    importance = TRUE)

preds_rf <- predict(rf1, newdata = test_X_mat, type = 'prob')[,2]

preds_rf_flag <- preds_rf >= 0.5

avg_test_preds <- as.vector((preds_glmn + preds_rf) / 2)

test_data$avg_preds <- avg_test_preds

plot(test_y_top50_perc_flag == 'TRUE',
     avg_preds)

