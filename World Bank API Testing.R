


library(jsonlite)
library(dplyr)



# Function to take url and transform it into one with all the results on one page

url_all_results <- function(original_url) {
  
  
# Append "?format=json" if url doesn't already have it
  if(!grepl("\\?", original_url)) {
    
    original_url <- paste0(original_url, "?format=json")
    
  }
  
  
  total_results <- original_url %>% fromJSON %>% .[[1]] %>% .$total
  
  url_with_all_results <- paste0(original_url, "&per_page=", total_results)
  
  return(url_with_all_results)
}



# Tests
original_url <- "https://api.worldbank.org/v2/country"
original_url <- "https://api.worldbank.org/v2/country?format=json"


url_all_results(original_url) %>% fromJSON
  








# Testing out the World Bank API

url <- "https://api.worldbank.org/v2/country/br?format=json"
fromJSON(url)

list_of_all_countries <- "https://api.worldbank.org/v2/country"
list_of_all_countries %>% paste0(., "?format=json") %>% fromJSON()
list_of_all_countries %>% paste0(., "?format=json") %>% fromJSON() %>% .[[2]] %>% .$name


list_of_indicators <- "https://api.worldbank.org/v2/indicators"
indicators <- list_of_indicators %>% paste0(., "?format=json", "&per_page=15650") %>% fromJSON %>% .[[2]] 

indicators$topics %>% .[1:20]


countries <- "https://api.worldbank.org/v2/country?format=json"
countries <- "https://api.worldbank.org/v2/country?format=json" %>% fromJSON(.)






# Inflation code 
inf_code <- "FP.CPI.TOTL.ZG"
# which is from: https://api.worldbank.org/v2/indicators?format=json&per_page=15650


# Example
# From: 
http://api.worldbank.org/countries/BGD/indicators/NY.GDP.MKTP.KD.ZG?per_page=11&date=2000:2010



# Inflation example:
# https://api.worldbank.org/countries/AU/indicators/FP.CPI.TOTL.ZG


# Inflation example with all results

country <- 
url <- "https://api.worldbank.org/countries/AU/indicators/FP.CPI.TOTL.ZG"











