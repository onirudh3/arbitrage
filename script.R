
# Libraries ---------------------------------------------------------------

library(tidyquant)
library(tidyr)
library(rvest)
library(dplyr)
library(fastDummies) # Create dummy variables
library(randomForest)
library(caret) # For hyperparameter optimization


# Data cleaning -----------------------------------------------------------

# URL of the Wikipedia page containing the list of S&P 500 companies
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"

# Read the HTML content of the webpage
html_content <- read_html(url)

# Extract the symbols from the first column of the table
top50_symbols <- html_content %>%
  html_nodes("table.wikitable tbody tr td:first-child") %>%
  html_text() %>%
  head(51) # Select the top 50 symbols

# Convert the symbols to character strings
top50_symbols <- as.character(top50_symbols)

# Clean up symbols to remove trailing newline characters
top50_symbols <- sub("\n", "", top50_symbols)

# Define the time period yyyy-mm-dd
start_date <- "1995-01-01"
end_date <- "2015-12-31"

# Download historical data for the top 50 symbols
df <- tq_get(top50_symbols, from = start_date, to = end_date, 
             source = "yahoo")
df$date <- as.Date(df$date, format = "%Y-%m-%d")

# Extract unique symbols from the 'Symbol' column in your dataset
stocks <- data.frame(ValueColumn = unique(df$symbol))

rm(url, html_content, start_date, end_date, top50_symbols)


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


# Define 1997-1999 data as training set -----------------------------------

start_date <- as.Date("1997-01-01")
end_date <- as.Date("1999-12-31")
train_period <- df[df$date >= start_date & df$date <= end_date, ]
train_period$returns_greater_than_market <- as.factor(train_period$returns_greater_than_market)


# Analysis in period 2000-2005  -------------------------------------------

df_test <- df[df$date > end_date & df$date <= as.Date("2005-01-01"), ]


## 1. Random forest with lagged returns ----

set.seed(1435289)
min_samples <- ceiling(0.0005 * nrow(train_period)) # Criteria used in paper
rf <- randomForest(returns_greater_than_market ~ lr_1 + lr_2 + lr_3 + lr_4 + 
                     lr_5 + lr_10 + lr_21 + lr_42 + lr_63 + lr_126 + lr_252,
                   data = train_period, ntree = 500, nodesize = min_samples,
                   na.action = na.omit, importance = T)
summary(rf)
varImpPlot(rf) 
print(rf)

# Classification
yhat.rf <- predict(rf, newdata = df_test)
table(yhat.rf, df_test$returns_greater_than_market)

# Modify the threshold (we want more true positives than true negatives)
predicted_probabilities <- predict(rf, newdata = df_test, type = "prob")[, 2]
new_threshold <- 0.66
yhat.rf_thresholded <- ifelse(predicted_probabilities > new_threshold, 1, 0)
table(yhat.rf_thresholded, df_test$returns_greater_than_market)

# True/False ratios
x <- table(yhat.rf_thresholded, df_test$returns_greater_than_market)
x[1, 1] / x[1, 2] # For negatives
x[2, 2] / x[2, 1] # For positives


## 2. Random Forest with all variables ----

variable_names <- names(df)[8:87]
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

yhat.rf_dummies <- predict(rf_dummies, newdata = df_test)
table(yhat.rf_dummies, df_test$returns_greater_than_market)

# Modified threshold
predicted_probabilities <- predict(rf_dummies, newdata = df_test, type = "prob")[, 2]
new_threshold <- 0.6
yhat.rf_dummies_thresholded <- ifelse(predicted_probabilities > new_threshold, 1, 0)
table(yhat.rf_dummies_thresholded, df_test$returns_greater_than_market)


## 3. RF with lagged returns and time dummies ----

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


## 4. RF with lagged returns and stock id dummies ----

variable_names <- names(df)[c(8:18, 38:87)]
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

yhat.rf_id_dummies <- predict(rf_id_dummies, newdata = df_test)
table(yhat.rf_id_dummies, df_test$returns_greater_than_market)
predicted_probabilities <- predict(rf_id_dummies, newdata = df_test, type = "prob")[, 2]
new_threshold <- 0.6
yhat.rf_id_dummies_thresholded <- ifelse(predicted_probabilities > new_threshold, 1, 0)
table(yhat.rf_id_dummies_thresholded, df_test$returns_greater_than_market)


# Analysis in period 2010-2015  -------------------------------------------

df_test <- df[df$date > as.Date("2010-01-01") & df$date <= as.Date("2015-12-31"), ]


## Random forest with lagged returns ----

# Classification
yhat.rf <- predict(rf, newdata = df_test)
table(yhat.rf, df_test$returns_greater_than_market)

# Modify the threshold (we want more true positives than true negatives)
predicted_probabilities <- predict(rf, newdata = df_test, type = "prob")[, 2]
new_threshold <- 0.66
yhat.rf_thresholded <- ifelse(predicted_probabilities > new_threshold, 1, 0)
table(yhat.rf_thresholded, df_test$returns_greater_than_market)

# True/False ratios
x <- table(yhat.rf_thresholded, df_test$returns_greater_than_market)
x[1, 1] / x[1, 2] # For negatives
x[2, 2] / x[2, 1] # For positives


# Randomized information --------------------------------------------------

# We randomize the results variable
num_rows <- nrow(rand_df)
rand_df$returns <- round(runif(num_rows, min = -1, max = 1), 9)
summary(rand_df$returns)
summary(df$returns)

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

# Just a small check on NAs
na_rows_2 <- apply(is.na(df), 1, any)
rand_df_na <- df[na_rows_2, ]

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


# Hyperparameter optimization ---------------------------------------------

train_control <- trainControl(method = "cv", 
                              number = 5,
                              verboseIter = TRUE,
                              search = "grid")

hyper_grid <- expand.grid(mtry = c(2, floor(sqrt(ncol(train_period))), 
                                   floor(ncol(train_period) / 2)))

# RF with lagged returns
set.seed(1435289)
rf_grid_search <- train(returns_greater_than_market ~ lr_1 + lr_2 + lr_3 + lr_4 + 
                          lr_5 + lr_10 + lr_21 + lr_42 + lr_63 + lr_126 + lr_252,
                        data = train_period,
                        method = "rf", 
                        metric = "Accuracy",
                        trControl = train_control, 
                        tuneGrid = hyper_grid,
                        na.action = na.omit,
                        ntree = 500)

print(rf_grid_search)
