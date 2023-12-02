rm(list=ls())
# Libraries ---------------------------------------------------------------

library(tidyquant)
library(tidyr)
library(rvest)
library(dplyr)
library(fastDummies) 
library(randomForest)

# Data cleaning -----------------------------------------------------------

# URL of the Wikipedia page containing the list of S&P 500 companies
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"

# Read the HTML content of the webpage
html_content <- read_html(url)

# Extract the symbols from the first column of the table
top100_symbols <- html_content %>%
  html_nodes("table.wikitable tbody tr td:first-child") %>%
  html_text() %>%
  head(137) # Select the top 100 symbols

# Convert the symbols to character strings
top100_symbols <- as.character(top100_symbols)

# Clean up symbols to remove trailing newline characters
top100_symbols <- sub("\n", "", top100_symbols)

# Define the time period yyyy-mm-dd
start_date <- "1995-01-01"
end_date <- "2000-12-31"

# Download historical data for the top 100 symbols
df <- tq_get(top100_symbols, from = start_date, to = end_date, 
             source = "yahoo")
df$date <- as.Date(df$date, format = "%Y-%m-%d")

# Extract unique symbols from the 'Symbol' column in your dataset
stocks <- data.frame(ValueColumn = unique(df$symbol))

rm(url, html_content, start_date, end_date, top100_symbols)

write.csv(df, file = "raw_data.csv", row.names = F)

# Lagged returns ----------------------------------------------------------

df <- df %>% 
  mutate(returns = diff(open) / lag(open), .by = symbol)

# Remove unwanted variables
df <- subset(df, select = -c(open, high, low, close, volume, adjusted))
df <- na.omit(subset(df, date > "1995-12-31"))

# Clone the dataset to use later in the randomized variable test
rand_df <- df

# Add lagged returns
df <- df %>% 
  mutate(lr_1 = lag(returns, n = 1),
         lr_2 = lag(returns, n = 2),
         lr_3 = lag(returns, n = 3),
         lr_4 = lag(returns, n = 4),
         lr_5 = lag(returns, n = 5),
         lr_10 = lag(returns, n = 10),
         lr_21 = lag(returns, n = 21),
         lr_42 = lag(returns, n = 42),
         lr_63 = lag(returns, n = 63),
         lr_126 = lag(returns, n = 126),
         lr_252 = lag(returns, n = 252), .by = symbol)
df <- na.omit(subset(df, date > "1995-12-31"))

# Market returns ----------------------------------------------------------

df <- df %>% 
  group_by(date) %>% 
  mutate(market_return = mean(returns), .after = returns)

df <- df %>% 
  mutate(returns_greater_than_market = case_when(returns > market_return ~ 1, 
                                                 T ~ 0), .after = market_return)
# Day of the week
df <- df %>% 
  mutate(weekday = wday(date, label = T, abbr = F), .after = date)
df <- dummy_cols(df, select_columns = "weekday")

# Month of the year
df <- df %>% 
  mutate(month = month(date), .after = weekday)
df <- dummy_cols(df, select_columns = "month")

# Stock dummies
df <- dummy_cols(df, select_columns = "symbol")

# Using 1997-1999 as training set -----------------------------------------------

start_date <- as.Date("1997-01-01")
end_date <- as.Date("1999-12-31")
train_period <- df[df$date >= start_date & df$date <= end_date, ]

# Random forest with the alternative training sample (1997-1999)
set.seed(1435289)
min_samples <- ceiling(0.0005 * nrow(train_period))
train_period$returns_greater_than_market <- as.factor(train_period$returns_greater_than_market)
rf_2 <- randomForest(returns_greater_than_market ~ lr_1 + lr_2 + lr_3 + lr_4 + 
                       lr_5 + lr_10 + lr_21 + lr_42 + lr_63 + lr_126 + lr_252,
                     data = train_period, ntree = 500, nodesize = min_samples,
                     na.action = na.omit, importance = T)
summary(rf_2)
varImpPlot(rf_2) 
print(rf_2)

# Classification
df_test <- df[df$date > end_date, ]
yhat.rf_2 <- predict(rf_2 , newdata = df_test)
table(yhat.rf_2, df_test$returns_greater_than_market)

# Modify the threshold (we want more true positives than true negatives)
predicted_probabilities <- predict(rf_2, newdata = df_test, type = "prob")[, 2]
new_threshold <- 0.6
yhat.rf_2_thresholded <- ifelse(predicted_probabilities > new_threshold, 1, 0)
table(yhat.rf_2_thresholded, df_test$returns_greater_than_market)

# Random Forest with extra variables ----------------------------------------

# Reduce sample to solve computational limitations
start_date <- as.Date("1997-01-01")
end_date <- as.Date("1997-12-31")
limit_date <- as.Date("2000-01-01")
train_period <- df[df$date >= start_date & df$date <= end_date, ]
df_test <- df[df$date > end_date & df$date < limit_date, ]
variable_names <- names(df)[8:136]
formula_str <- paste("returns_greater_than_market ~", paste(variable_names, collapse = " + "))
formula <- as.formula(formula_str)
rf_dummies <- randomForest(formula, 
             data = train_period, 
             ntree = 200, 
             nodesize = min_samples,
             na.action = na.omit, 
             importance = TRUE)

summary(rf_dummies)
varImpPlot(rf_dummies) 
print(rf_dummies)

# Classification
yhat.rf_dummies <- predict(rf_dummies, newdata = df_test)
table(yhat.rf_dummies, df_test$returns_greater_than_market)
predicted_probabilities <- predict(rf_dummies, newdata = df_test, type = "prob")[, 2]
new_threshold <- 0.6
yhat.rf_dummies_thresholded <- ifelse(predicted_probabilities > new_threshold, 1, 0)
table(yhat.rf_dummies_thresholded, df_test$returns_greater_than_market)

# RF with lagged returns and time dummies 
variable_names <- names(df)[8:37]
formula_str <- paste("returns_greater_than_market ~", paste(variable_names, collapse = " + "))
formula <- as.formula(formula_str)
rf_time_dummies <- randomForest(formula, 
                           data = train_period, 
                           ntree = 200, 
                           nodesize = min_samples,
                           na.action = na.omit, 
                           importance = TRUE)

summary(rf_time_dummies)
varImpPlot(rf_time_dummies) 
print(rf_time_dummies)

# Classification
yhat.rf_time_dummies <- predict(rf_time_dummies, newdata = df_test)
table(yhat.rf_time_dummies, df_test$returns_greater_than_market)
predicted_probabilities <- predict(rf_time_dummies, newdata = df_test, type = "prob")[, 2]
new_threshold <- 0.6
yhat.rf_time_dummies_thresholded <- ifelse(predicted_probabilities > new_threshold, 1, 0)
table(yhat.rf_time_dummies_thresholded, df_test$returns_greater_than_market)

# RF with lagged returns and stock id dummies
variable_names <- names(df)[c(8:18, 38:136)]
formula_str <- paste("returns_greater_than_market ~", paste(variable_names, collapse = " + "))
formula <- as.formula(formula_str)
rf_id_dummies <- randomForest(formula, 
                           data = train_period, 
                           ntree = 200, 
                           nodesize = min_samples,
                           na.action = na.omit, 
                           importance = TRUE)

summary(rf_id_dummies)
varImpPlot(rf_id_dummies) 
print(rf_id_dummies)

# Classification
yhat.rf_id_dummies <- predict(rf_id_dummies, newdata = df_test)
table(yhat.rf_id_dummies, df_test$returns_greater_than_market)
predicted_probabilities <- predict(rf_id_dummies, newdata = df_test, type = "prob")[, 2]
new_threshold <- 0.6
yhat.rf_id_dummies_thresholded <- ifelse(predicted_probabilities > new_threshold, 1, 0)
table(yhat.rf_id_dummies_thresholded, df_test$returns_greater_than_market)

# Randomized information ---------------------------------------------------

# We randomized the results variable
num_rows <- nrow(rand_df)
rand_df$returns <- round(runif(num_rows, min = -1, max = 1), 9)

# With the randomized results variable, we create the lag variables
rand_df <- rand_df %>% 
  mutate(lr_1 = lag(returns, n = 1),
         lr_2 = lag(returns, n = 2),
         lr_3 = lag(returns, n = 3),
         lr_4 = lag(returns, n = 4),
         lr_5 = lag(returns, n = 5),
         lr_10 = lag(returns, n = 10),
         lr_21 = lag(returns, n = 21),
         lr_42 = lag(returns, n = 42),
         lr_63 = lag(returns, n = 63),
         lr_126 = lag(returns, n = 126),
         lr_252 = lag(returns, n = 252), .by = symbol)

# We delete NAs and create the dependent variable
rand_df <- na.omit(subset(rand_df, date > "1995-12-31"))
rand_df <- rand_df %>% 
  group_by(date) %>% 
  mutate(market_return = mean(returns), .after = returns)
rand_df <- rand_df %>% 
  mutate(returns_greater_than_market = case_when(returns > market_return ~ 1, 
                                                 T ~ 0), .after = market_return)
rand_df$returns_greater_than_market <- as.factor(rand_df$returns_greater_than_market)

# We create the training sample [1997-2000) and do the RF 
train_period_rand <- rand_df[rand_df$date >= start_date & rand_df$date <= end_date, ]
set.seed(1435289)
rf_rand <- randomForest(returns_greater_than_market ~ lr_1 + lr_2 + lr_3 + lr_4 + 
                          lr_5 + lr_10 + lr_21 + lr_42 + lr_63 + lr_126 + lr_252,
                        data = train_period_rand, ntree = 500, nodesize = min_samples,
                        na.action = na.omit, importance = T)

summary(rf_rand)
  
print(rf_rand)

# Classification
df_test_rand <- rand_df[rand_df$date > end_date, ]
yhat.rf_rand <- predict(rf_rand, newdata = df_test_rand)
table(yhat.rf_rand, df_test_rand$returns_greater_than_market)

# Hyperparameter optimization -----------------------------------------------
install.packages("caret")
library(caret)
train_control <- trainControl(method = "cv", 
                              number = 5,
                              verboseIter = TRUE,
                              search = "grid"
)
hyper_grid <- expand.grid(
  mtry = c(2, floor(sqrt(ncol(train_period))), 
           floor(ncol(train_period)/2))
  )

set.seed(1435289)
rf_grid_search <- train(
  returns_greater_than_market ~ lr_1 + lr_2 + lr_3 + lr_4 + 
    lr_5 + lr_10 + lr_21 + lr_42 + lr_63 + lr_126 + lr_252,
  data = train_period,
  method = "rf", 
  metric = "Accuracy",
  trControl = train_control, 
  tuneGrid = hyper_grid,
  na.action = na.omit,
  ntree = 500
)
print(rf_grid_search)

# Random forest, first attempt -----------------------------------------------------------
# Training set
train <- sample(1:nrow(df), nrow(df) / 2)

# Make returns_greater_than_market binary 
df$returns_greater_than_market <- as.factor(df$returns_greater_than_market)

# Random forest
set.seed(1435289)
min_samples <- ceiling(0.0005 * length(train))

rf <- randomForest(returns_greater_than_market ~ lr_1 + lr_2 + lr_3 + lr_4 + 
                     lr_5 + lr_10 + lr_21 + lr_42 + lr_63 + lr_126 + lr_252,
                   data = df, ntree = 500, nodesize = min_samples, subset = train,
                   na.action = na.omit, importance = T)
summary(rf)
varImpPlot(rf)
print(rf)

# Classification
yhat.rf <- predict(rf , newdata = df[-train, ])
table(yhat.rf, df$returns_greater_than_market[-train])

# Top 10 stock prediction ---------------------------------------------------
df <- df %>%
  group_by(date) %>%  # Group by date
  arrange(desc(returns)) %>%  # Arrange the data in descending order of returns within each date
  mutate(rank = row_number(),  # Assign ranks based on order
         top_10 = as.integer(rank <= 10)) %>%  # Create binary variable for top 10
  ungroup()

# Create the RF
set.seed(1435289)
train_period$top_10 <- as.factor(train_period$top_10)
rf_3 <- randomForest(top_10 ~ lr_1 + lr_2 + lr_3 + lr_4 + 
                       lr_5 + lr_10 + lr_21 + lr_42 + lr_63 + lr_126 + lr_252,
                     data = train_period, ntree = 500,
                     na.action = na.omit, importance = T)
summary(rf_3)
varImpPlot(rf_3) 
print(rf_3)

# Classification
df_test <- df[df$date > end_date, ]
yhat.rf_3 <- predict(rf_3 , newdata = df_test)
table(yhat.rf_3, df_test$returns_greater_than_market)

# Modify the threshold 
predicted_probabilities <- predict(rf_3, newdata = df_test, type = "prob")[ ,2]
new_threshold <- 0.5
yhat.rf_3_thresholded <- ifelse(predicted_probabilities > new_threshold, 1, 0)
table(yhat.rf_3_thresholded, df_test$top_10)


# Modify the threshold 
predicted_probabilities <- predict(rf_3, newdata = df_test, type = "prob")[ ,2]
new_threshold <- 0.5
yhat.rf_3_thresholded <- ifelse(predicted_probabilities > new_threshold, 1, 0)
table(yhat.rf_3_thresholded, df_test$top_10)
