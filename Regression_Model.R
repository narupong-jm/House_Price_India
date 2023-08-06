# Import library
library(tidyverse)
library(ggplot2)
library(readxl)
library(caret)

# Load Dataset
dataset = read_excel('House Price India.xlsx')
View(dataset)

# Visualize predict variable before prepare data
ggplot(dataset, aes(Price)) +
  geom_histogram(bins = 50)

# 0. Prepare Data
tf_data <- dataset %>%
  select(Date, `number of bedrooms`,`number of bathrooms`,`number of floors`,`number of views`,
         `living area`,`grade of the house`,`Area of the house(excluding basement)`,`Area of the basement`,
         `Distance from the airport`,Price) %>%
  mutate(price_log = log(Price))
View(tf_data)

# Visualize predict variable after prepare data
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
p <- predict(lm_model, newdata = test_dataset)

# 4. Evaluate Model
error <- test_dataset$price_log - p
rmse <- sqrt(mean(error**2))

rmse # result = 0.3342832
lm_model # result = 0.3355295
