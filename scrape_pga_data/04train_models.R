library(tidyverse)
library(glmnet)
library(e1071)
library(randomForest)
library(keras)

full_model_data <- read_csv("data//modeling_data.csv")

set.seed(1)

training_samps <- sample(nrow(full_model_data),
                         0.75 * nrow(full_model_data))
# create training data
train_data <- full_model_data[training_samps,]

train_X <- train_data %>%
  select(contains("lag"))

train_X_lag1 <- train_data %>%
  select(contains("lag_1"))

train_X_lag1_2 <- train_data %>%
  select(contains("lag_1"), contains("lag_2"), contains("lag12_delta"))

train_X_mat <- as.matrix(train_X)
train_X_mat_lag1 <- as.matrix(train_X_lag1)
train_X_mat_lag1_2 <- as.matrix(train_X_lag1_2)

train_y <- train_data$quartile_group

train_y_top50_perc_binary <- as.numeric(train_data$rank_money >= 0.5)

train_y_top50_perc_flag <- as.character(train_data$rank_money >= 0.5)

train_y_top50_perc_flag_factor <- as.factor(train_y_top50_perc_flag)

# create testing data
test_data <- full_model_data[-training_samps,]

test_X <- test_data %>%
  select(contains("lag"))

test_X_lag1 <- test_data %>%
  select(contains("lag_1"))

test_X_lag1_2 <- test_data %>%
  select(contains("lag_1"), contains("lag_2"), contains("lag12_delta"))

test_X_mat <- as.matrix(test_X)
test_X_mat_lag1 <- as.matrix(test_X_lag1)
test_X_mat_lag1_2 <- as.matrix(test_X_lag1_2)

test_Y <- test_data$quartile_group

test_y_top50_perc_binary <- as.numeric(test_data$rank_money >= 0.5)

test_y_top50_perc_flag <- as.character(test_data$rank_money >= 0.5)

test_y_top50_perc_flag_factor <- as.factor(test_y_top50_perc_flag)

# train glmnet models
cv_glmn_full <- cv.glmnet(x = train_X_mat,
                          y = train_y_top50_perc_flag,
                          family = 'binomial',
                          nlambda = 500)

cv_glmn_lag1 <- cv.glmnet(x = train_X_mat_lag1,
                          y = train_y_top50_perc_flag,
                          family = 'binomial',
                          nlambda = 500)

cv_glmn_lag1_2 <- cv.glmnet(x = train_X_mat_lag1_2,
                          y = train_y_top50_perc_flag,
                          family = 'binomial',
                          nlambda = 500)

# train randomForest models
rf_full <- randomForest(x = train_X_mat,
                        y = train_y_top50_perc_flag_factor,
                        ntree = 1000,
                        importance = TRUE)

rf_lag1 <- randomForest(x = train_X_mat_lag1,
                        y = train_y_top50_perc_flag_factor,
                        ntree = 1000,
                        importance = TRUE)

rf_lag1_2 <- randomForest(x = train_X_mat_lag1_2,
                          y = train_y_top50_perc_flag_factor,
                          ntree = 1000,
                          importance = TRUE)



# train neural net

min_max_matrix <- function(mat){
  apply(mat, 2, FUN = function(x) (x - min(x)) / (max(x) - min(x)))
}

model <- keras_model_sequential()

model %>%
  layer_dense(units = 64, activation = 'relu') %>%
  layer_dropout(rate = .5) %>%
  layer_dense(units = 16, activation = 'relu') %>%
  layer_dropout(rate = .3) %>%
  layer_dense(units = 16, activation = 'relu') %>%
  layer_dropout(rate = .1) %>%
  layer_dense(units = 1, activation = "sigmoid")

model %>% compile(
  optimizer = 'adam', 
  loss = 'binary_crossentropy',
  metrics = c('accuracy')
)

model %>%
  fit(train_X_mat,
      as.matrix(train_y_top50_perc_binary),
      validation_data = list(test_X_mat,
                             as.matrix(test_y_top50_perc_binary)),
      epochs = 25,
      batch_size = 8)
