
# Libraries ---------------------------------------------------------------

library(tidyquant)
library(rvest)
library(dplyr)

# URL of the Wikipedia page containing the list of S&P 500 companies
url <- "https://en.wikipedia.org/wiki/List_of_S%26P_500_companies"

# Read the HTML content of the webpage
html_content <- read_html(url)

# Extract the symbols from the first column of the table
top100_symbols <- html_content %>%
  html_nodes("table.wikitable tbody tr td:first-child") %>%
  html_text() %>%
  head(138) # Select the top 100 symbols

# Convert the symbols to character strings
top100_symbols <- as.character(top100_symbols)

# Clean up symbols to remove trailing newline characters
top100_symbols <- sub("\n", "", top100_symbols)

df <- data.frame(top100_symbols)

# Define the time period yyyy-mm-dd
start_date <- "2000-01-01"
end_date <- "2000-12-31"

# Download historical data for the top 100 symbols
top100_data <- tq_get(top100_symbols, from = start_date, to = end_date, 
                      source = "yahoo")

# Extract unique symbols from the 'Symbol' column in your dataset
unique_symbols <- unique(top100_data$symbol)
stocks <- data.frame(ValueColumn = unique_symbols)

