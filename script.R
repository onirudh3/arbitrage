rm(list=ls())
# Libraries ---------------------------------------------------------------

library(tidyquant)
library(tidyr)
library(rvest)
library(dplyr)

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
end_date <- "2005-12-31"

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
na_rows <- apply(is.na(df), 1, any)
df_na <- df[na_rows,]
df <- na.omit(subset(df, date > "1995-12-31"))
# Market returns ----------------------------------------------------------

df <- df %>% 
  group_by(date) %>% 
  mutate(market_return = mean(returns), .after = returns)

df <- df %>% 
  mutate(returns_greater_than_market = case_when(returns > market_return ~ 1, 
                                                 T ~ 0), .after = market_return)

# Random forest -----------------------------------------------------------

# Training set
train <- sample(1:nrow(df), nrow(df) / 2)

# Make returns_greater_than_market binary 
df$returns_greater_than_market <- as.factor(df$returns_greater_than_market)

# Random forest
set.seed(1435289)
rf <- randomForest(returns_greater_than_market ~ lr_1 + lr_2 + lr_3 + lr_4 + 
                     lr_5 + lr_10 + lr_21 + lr_42 + lr_63 + lr_126 + lr_252,
                   data = df, ntree = 500, subset = train,
                   na.action = na.omit, importance = T)
summary(rf)
varImpPlot(rf)

summary(rf)
varImpPlot(rf)
print(rf)

# Classification

yhat.rf <- predict(rf , newdata = df[-train, ])
table(stock_pred, df$returns_greater_than_market[-train])

# A different approach for the training set, in line with the paper: use the first 3 years (1995 and 1996 excluded because of NA/s)

start_date <- as.Date("1997-01-01")
end_date <- as.Date("1999-12-31")
train_period <- df[df$date >= start_date & df$date <= end_date, ]

# Random forest with the alternative training sample (1997-1999)
set.seed(1435289)
rf_2 <- randomForest(returns_greater_than_market ~ lr_1 + lr_2 + lr_3 + lr_4 + 
                     lr_5 + lr_10 + lr_21 + lr_42 + lr_63 + lr_126 + lr_252,
                   data = train_period, ntree = 500, mtry = floor(sqrt(ncol(df)-1)),
                   na.action = na.omit, importance = T)
summary(rf)
varImpPlot(rf) 
print(rf)

# Classification
df_2 <- df[df$date > end_date, ]
yhat.rf_2 <- predict(rf_2 , newdata = df_2)
table(yhat.rf_2, df_2$returns_greater_than_market)


# Creating a randomized dataset (we only need to randomize the returns variable)
num_rows <- nrow(rand_df)
rand_df$returns <- round(runif(num_rows, min = -1, max = 1), 9)
summary(rand_df$returns)
summary(df$returns)
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
na_rows_2 <- apply(is.na(df), 1, any)
rand_df_na <- df[na_rows_2, ]
rand_df <- na.omit(subset(rand_df, date > "1995-12-31"))
rand_df <- rand_df %>% 
  group_by(date) %>% 
  mutate(market_return = mean(returns), .after = returns)
rand_df <- rand_df %>% 
  mutate(returns_greater_than_market = case_when(returns > market_return ~ 1, 
                                                 T ~ 0), .after = market_return)
train_period_rand <- rand_df[rand_df$date >= start_date & rand_df$date <= end_date, ]

set.seed(1435289)
rf_rand <- randomForest(returns_greater_than_market ~ lr_1 + lr_2 + lr_3 + lr_4 + 
                       lr_5 + lr_10 + lr_21 + lr_42 + lr_63 + lr_126 + lr_252,
                     data = train_period_rand, ntree = 500, mtry = floor(sqrt(ncol(df)-1)),
                     na.action = na.omit, importance = T)
