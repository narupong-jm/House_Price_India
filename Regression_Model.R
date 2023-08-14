library(tidyverse)
library(ggplot2)
library(readxl)
library(caret)

dataset = read_excel('House Price India.xlsx')
View(dataset)

ggplot(dataset, aes(Price)) +
  geom_histogram(bins = 50)
  
tf_data <- dataset %>%
  select(Date, `number of bedrooms`,`number of bathrooms`,`number of floors`,`number of views`,
         `living area`,`grade of the house`,`Area of the house(excluding basement)`,`Area of the basement`,
         `Distance from the airport`,Price) %>%
  mutate(price_log = log(Price))
View(tf_data)

ggplot(tf_data, aes(price_log)) +
  geom_histogram(bins = 30)

# 1. Split Data
split_train_test <- function(data) {
  set.seed(42)
  n <- nrow(data)
  id <- sample(n, size = 0.8*n)
  train_data <- data[id, ]
  test_data <- data[-id, ]
  return(list(train_data,test_data))
}
split_data <- split_train_test(tf_data)
train_dataset <- split_data[[1]]
test_dataset <- split_data[[2]]

train_dataset
test_dataset
# 2. Train Model
lm_model <- train(price_log ~ `living area` + `Distance from the airport` + 
                    `grade of the house` + `number of views` +
                    `Area of the house(excluding basement)`,
                  train_dataset, method = "lm")

# 3. Score Model
p_train <- predict(lm_model, newdata = train_dataset)
unlog_p_train <- exp(p_train)
p_test <- predict(lm_model, newdata = test_dataset)
unlog_p_test <- exp(p_test)

# 4. Evaluate Model
rmse_train <- sqrt(mean((train_dataset$Price - unlog_p_train)**2))
rmse_test <- sqrt(mean((test_dataset$Price - unlog_p_test)**2))

rmse_train
rmse_test
