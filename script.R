# Libraries ---------------------------------------------------------------

library(tidyquant)
library(tidyr)
library(rvest)
library(dplyr)
library(fastDummies) # Create dummy variables
library(randomForest)
library(lubridate) # For annual returns

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


# Computing the profits

# Get the returns
df_test$predicted_probability <- predicted_probabilities
relevant_data <- df_test[, c("symbol", "date", "predicted_probability", "returns")]
sorted_data <- relevant_data %>%
  group_by(date) %>%
  arrange(date, desc(predicted_probability))
top_flop_stocks <- sorted_data %>%
  group_by(date) %>%
  summarise(
    top_5_symbols = list(head(symbol, 5)),
    flop_5_symbols = list(tail(symbol, 5)),
    .groups = 'drop'
  )

df_test_selected <- df_test[, c("symbol", "date", "returns")]

# Expanding top 5 symbols
top_stocks_expanded <- top_flop_stocks %>%
  select(date, top_5_symbols) %>%
  unnest(cols = top_5_symbols) %>%
  rename(symbol = top_5_symbols)

# Expanding bottom 5 symbols
flop_stocks_expanded <- top_flop_stocks %>%
  select(date, flop_5_symbols) %>%
  unnest(cols = flop_5_symbols) %>%
  rename(symbol = flop_5_symbols)

top_stocks_expanded$type <- "Top 5"
flop_stocks_expanded$type <- "Flop 5"
expanded_stocks <- rbind(top_stocks_expanded, flop_stocks_expanded)

# Join for top 5 stocks
top_stocks_with_returns <- expanded_stocks %>%
  filter(type == "Top 5") %>%
  select(date, symbol) %>%
  left_join(df_test_selected, by = c("date", "symbol"))

# Join for bottom 5 stocks
flop_stocks_with_returns <- expanded_stocks %>%
  filter(type == "Flop 5") %>%
  select(date, symbol) %>%
  left_join(df_test_selected, by = c("date", "symbol"))

# Get the daily average returns
average_daily_returns_top <- top_stocks_with_returns %>%
  group_by(date) %>%
  summarise(average_returns = mean(returns, na.rm = TRUE))
average_daily_returns_flop <- flop_stocks_with_returns %>%
  group_by(date) %>%
  summarise(average_returns = mean(returns, na.rm = TRUE))

# Flop are short-traded, meaning we need to change the sign
average_daily_returns_flop$average_returns <- -1 * average_daily_returns_flop$average_returns

# Now let's combine short and long portfolio returns
total_returns <- left_join(average_daily_returns_top, average_daily_returns_flop, by = "date", suffix = c("_top", "_flop"))
total_returns <- total_returns %>%
  mutate(total_average_returns = average_returns_top + average_returns_flop) %>%
  select(date, total_average_returns)
sum(total_returns$total_average_returns, na.rm = TRUE)

total_returns$date <- as.Date(total_returns$date)

# Extract the year from the date and create a new column for it
total_returns <- total_returns %>%
  mutate(year = year(date))

# Sum total_average_returns for each year
annual_returns <- total_returns %>%
  group_by(year) %>%
  summarise(percentage_return = sum(total_average_returns, na.rm = TRUE))

rm(top_flop_stocks, top_stocks_expanded, expanded_stocks,
   average_daily_returns_flop, average_daily_returns_top,
   flop_stocks_expanded, top_stocks_with_returns, 
   flop_stocks_with_returns, sorted_data, 
   relevant_data, df_test_selected)

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

# True/False ratios
x <- table(yhat.rf_thresholded, df_test$returns_greater_than_market)
x[1, 1] / x[1, 2] # For negatives
x[2, 2] / x[2, 1] # For positives

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

# True/False ratios
x <- table(yhat.rf_thresholded, df_test$returns_greater_than_market)
x[1, 1] / x[1, 2] # For negatives
x[2, 2] / x[2, 1] # For positives

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

# True/False ratios
x <- table(yhat.rf_thresholded, df_test$returns_greater_than_market)
x[1, 1] / x[1, 2] # For negatives
x[2, 2] / x[2, 1] # For positives

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

# Computing the profits

# Get the returns
df_test$predicted_probability <- predicted_probabilities
relevant_data <- df_test[, c("symbol", "date", "predicted_probability", "returns")]
sorted_data <- relevant_data %>%
  group_by(date) %>%
  arrange(date, desc(predicted_probability))
top_flop_stocks <- sorted_data %>%
  group_by(date) %>%
  summarise(
    top_5_symbols = list(head(symbol, 5)),
    flop_5_symbols = list(tail(symbol, 5)),
    .groups = 'drop'
  )

df_test_selected <- df_test[, c("symbol", "date", "returns")]

# Expanding top 5 symbols
top_stocks_expanded <- top_flop_stocks %>%
  select(date, top_5_symbols) %>%
  unnest(cols = top_5_symbols) %>%
  rename(symbol = top_5_symbols)

# Expanding bottom 5 symbols
flop_stocks_expanded <- top_flop_stocks %>%
  select(date, flop_5_symbols) %>%
  unnest(cols = flop_5_symbols) %>%
  rename(symbol = flop_5_symbols)

top_stocks_expanded$type <- "Top 5"
flop_stocks_expanded$type <- "Flop 5"
expanded_stocks <- rbind(top_stocks_expanded, flop_stocks_expanded)

# Join for top 5 stocks
top_stocks_with_returns <- expanded_stocks %>%
  filter(type == "Top 5") %>%
  select(date, symbol) %>%
  left_join(df_test_selected, by = c("date", "symbol"))

# Join for bottom 5 stocks
flop_stocks_with_returns <- expanded_stocks %>%
  filter(type == "Flop 5") %>%
  select(date, symbol) %>%
  left_join(df_test_selected, by = c("date", "symbol"))

# Get the daily average returns
average_daily_returns_top <- top_stocks_with_returns %>%
  group_by(date) %>%
  summarise(average_returns = mean(returns, na.rm = TRUE))
average_daily_returns_flop <- flop_stocks_with_returns %>%
  group_by(date) %>%
  summarise(average_returns = mean(returns, na.rm = TRUE))

# Flop are short-traded, meaning we need to change the sign
average_daily_returns_flop$average_returns <- -1 * average_daily_returns_flop$average_returns

# Now let's combine short and long portfolio returns
total_returns_2010 <- left_join(average_daily_returns_top, average_daily_returns_flop, by = "date", suffix = c("_top", "_flop"))
total_returns_2010 <- total_returns_2010 %>%
  mutate(total_average_returns = average_returns_top + average_returns_flop) %>%
  select(date, total_average_returns)
sum(total_returns$total_average_returns, na.rm = TRUE)

total_returns$date <- as.Date(total_returns$date)

# Extract the year from the date and create a new column for it
total_returns <- total_returns %>%
  mutate(year = year(date))

# Sum total_average_returns for each year
annual_returns_2010 <- total_returns %>%
  group_by(year) %>%
  summarise(percentage_return = sum(total_average_returns, na.rm = TRUE))

rm(top_flop_stocks, top_stocks_expanded, expanded_stocks,
   average_daily_returns_flop, average_daily_returns_top,
   flop_stocks_expanded, top_stocks_with_returns, 
   flop_stocks_with_returns,
   sorted_data, relevant_data, df_test_selected)

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
varImpPlot(rf_rand)

# Classification
df_test_rand <- rand_df[rand_df$date > end_date & rand_df$date <= as.Date("2005-01-01"), ]
yhat.rf_rand <- predict(rf_rand, newdata = df_test_rand)
table(yhat.rf_rand, df_test_rand$returns_greater_than_market)
predicted_probabilities <- predict(rf_rand, newdata = df_test_rand, type = "prob")[, 2]

# True/False ratios
x <- table(yhat.rf_rand, df_test_rand$returns_greater_than_market)
x[1, 1] / x[1, 2] # For negatives
x[2, 2] / x[2, 1] # For positives

# Get the returns
df_test_rand$predicted_probability <- predicted_probabilities
relevant_data <- df_test_rand[, c("symbol", "date", "predicted_probability", "returns")]
sorted_data <- relevant_data %>%
  group_by(date) %>%
  arrange(date, desc(predicted_probability))
top_flop_stocks <- sorted_data %>%
  group_by(date) %>%
  summarise(
    top_5_symbols = list(head(symbol, 5)),
    flop_5_symbols = list(tail(symbol, 5)),
    .groups = 'drop'
  )

df_test_selected <- df_test_rand[, c("symbol", "date", "returns")]

# Expanding top 5 symbols
top_stocks_expanded <- top_flop_stocks %>%
  select(date, top_5_symbols) %>%
  unnest(cols = top_5_symbols) %>%
  rename(symbol = top_5_symbols)

# Expanding bottom 5 symbols
flop_stocks_expanded <- top_flop_stocks %>%
  select(date, flop_5_symbols) %>%
  unnest(cols = flop_5_symbols) %>%
  rename(symbol = flop_5_symbols)

top_stocks_expanded$type <- "Top 5"
flop_stocks_expanded$type <- "Flop 5"
expanded_stocks <- rbind(top_stocks_expanded, flop_stocks_expanded)

# Join for top 5 stocks
top_stocks_with_returns <- expanded_stocks %>%
  filter(type == "Top 5") %>%
  select(date, symbol) %>%
  left_join(df_test_selected, by = c("date", "symbol"))

# Join for bottom 5 stocks
flop_stocks_with_returns <- expanded_stocks %>%
  filter(type == "Flop 5") %>%
  select(date, symbol) %>%
  left_join(df_test_selected, by = c("date", "symbol"))

# Get the daily average returns
average_daily_returns_top <- top_stocks_with_returns %>%
  group_by(date) %>%
  summarise(average_returns = mean(returns, na.rm = TRUE))
average_daily_returns_flop <- flop_stocks_with_returns %>%
  group_by(date) %>%
  summarise(average_returns = mean(returns, na.rm = TRUE))

# Flop are short-traded, meaning we need to change the sign
average_daily_returns_flop$average_returns <- -1 * average_daily_returns_flop$average_returns

# Now let's combine short and long portfolio returns
total_returns <- left_join(average_daily_returns_top, average_daily_returns_flop, by = "date", suffix = c("_top", "_flop"))
total_returns <- total_returns %>%
  mutate(total_average_returns = average_returns_top + average_returns_flop) %>%
  select(date, total_average_returns)
total_returns$date <- as.Date(total_returns$date)

# Extract the year from the date and create a new column for it
total_returns <- total_returns %>%
  mutate(year = year(date))

# Sum total_average_returns for each year
annual_returns_rand <- total_returns %>%
  group_by(year) %>%
  summarise(annual_sum = sum(total_average_returns, na.rm = TRUE))

rm(top_flop_stocks, top_stocks_expanded, expanded_stocks,
   average_daily_returns_flop, average_daily_returns_top,
   flop_stocks_expanded, top_stocks_with_returns, 
   flop_stocks_with_returns, total_returns,
   sorted_data, relevant_data, df_test_selected)

# Sum total_average_returns for each year
annual_sums <- combined_data %>%
  group_by(year) %>%
  summarise(annual_sum = sum(total_average_returns, na.rm = TRUE))
