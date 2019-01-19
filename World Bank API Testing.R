


library(jsonlite)
library(dplyr)



#----- Function to take url and transform it into one with all the results on one page -----#

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
  

#----- END -----#






#----- Function to show user available country codes -----#

show_countries <- function() {
  
  countries_url <- "https://api.worldbank.org/v2/country?format=json" %>% url_all_results(.)
  countries <- countries_url %>% fromJSON(.)
  
  countries <- data.frame(countries[[2]]$iso2Code, countries[[2]]$name, stringsAsFactors = FALSE)
  colnames(countries) <- c("Country", "iso2Code")
  
  return(countries)
}


# Test
show_countries()

#----- END -----#




#----- Function to retrieve inflation data given country -----#

get_inflation_data <- function(country) {

  country <- country %>% toupper(.)
  inflation_url <- paste0("https://api.worldbank.org/countries/", country, "/indicators/FP.CPI.TOTL.ZG")
  
  inflation_url <- inflation_url %>% url_all_results
  
  inflation_data <- inflation_url %>% fromJSON(.)
  
  return(inflation_data)
}


# Test
country <- "AU"
get_inflation_data(country)

#----- END -----#






#----- Function that uses inflation data to in/deflate prices -----#

inflate <- function(price, country, from_date, to_date)

# From inspection of WB's values for AU 2017 data, it appears they mean FY rather than calendar year (although the numbers aren't exactly conclusive)




#----- END -----#




#----- Function to convert prices into present day dollars -----#

get_real <- function(price, country, from_date) {
  
  to_date <- Sys.Date()
  
  price <- inflate(price, country, from_date, to_date)
  
  return(price)
}


#----- END -----#









# Testing out the World Bank API

url <- "https://api.worldbank.org/v2/country/br?format=json"
fromJSON(url)

list_of_all_countries <- "https://api.worldbank.org/v2/country"
list_of_all_countries %>% paste0(., "?format=json") %>% fromJSON()
list_of_all_countries %>% paste0(., "?format=json") %>% fromJSON() %>% .[[2]] %>% .$name


list_of_indicators <- "https://api.worldbank.org/v2/indicators"
indicators <- list_of_indicators %>% paste0(., "?format=json", "&per_page=15650") %>% fromJSON %>% .[[2]] 

indicators$topics %>% .[1:20]


countries_url <- "https://api.worldbank.org/v2/country?format=json" %>% url_all_results(.)
countries <- countries_url %>% fromJSON(.)

countries <- as.data.frame(countries[[2]]$iso2Code, countries[[2]]$name, stringsAsFactors = FALSE)



# Inflation code 
inf_code <- "FP.CPI.TOTL.ZG"
# which is from: https://api.worldbank.org/v2/indicators?format=json&per_page=15650


# Example
# From: http://blogs.worldbank.org/opendata/the-top-5-ways-to-access-world-bank-data
example <- "http://api.worldbank.org/countries/BGD/indicators/NY.GDP.MKTP.KD.ZG?per_page=11&date=2000:2010"



# Inflation example:
# https://api.worldbank.org/countries/AU/indicators/FP.CPI.TOTL.ZG


# Inflation example with all results

country <- 
url <- "https://api.worldbank.org/countries/AU/indicators/FP.CPI.TOTL.ZG"











