
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

# Extract unique symbols from the 'Symbol' column in your dataset
stocks <- data.frame(ValueColumn = unique(df$symbol))

rm(url, html_content, start_date, end_date, top100_symbols)

write.csv(df, file = "raw_data.csv", row.names = F)

# Lagged returns ----------------------------------------------------------

df <- df %>% 
  mutate(returns = diff(close) / lag(close), .by = symbol)

# Remove unwanted variables
df <- subset(df, select = -c(open, high, low, close, volume, adjusted))

# Remove NA values
df <- na.omit(df)

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



# Market returns ----------------------------------------------------------

df <- df %>% 
  group_by(date) %>% 
  mutate(market_return = mean(returns), .after = returns)

df <- df %>% 
  mutate(returns_greater_than_market = case_when(returns > market_return ~ 1, 
                                                 T ~ 0), .after = market_return)

# Random forest -----------------------------------------------------------

# We need one year of data
df <- na.omit(subset(df, date > "1995-12-31"))

# Training set
train <- sample(1:nrow(df), nrow(df) / 2)

# Make returns_greater_than_market binary 
df$returns_greater_than_market <- as.factor(df$returns_greater_than_market)

# Random forest
set.seed(1435289)
rf <- randomForest(returns_greater_than_market ~ lr_1 + lr_2 + lr_3 + lr_4 + 
                     lr_5 + lr_10 + lr_21 + lr_42 + lr_63 + lr_126 + lr_252,
                   data = df, ntree = 500, mtry = floor(sqrt(ncol(df)-1)), subset = train,
                   na.action = na.omit, importance = T)

# Output
summary(rf)
print(rf)

# Variable importance
varImpPlot(rf)

# Classification
print(rf)
yhat.rf <- predict(rf , newdata = df[-train, ])
df_test <- df[-train, "returns"]
mean((yhat.rf - df_test$returns) ^ 2)

# A different approach for the training set, in line with the paper: use the first 3 years (1995 and 1996 excluded because of NA/s)
df$date <- as.Date(df$date, format = "%Y-%m-%d")
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
