library(jsonlite)
library(dplyr)
library(lubridate)
library(purrr)
# options(digits = 22)


# Assign these variables once
country <- "AU"
inflation_dataframe <- retrieve_inflation_data(country)
countries_dataframe <- show_countries()


# Required for some error messages
available_inflation_data <- inflation_dataframe %>% .[[2]] %>% na.omit

price <- 10


