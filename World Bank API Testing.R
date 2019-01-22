


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
url_all_results(original_url) %>% fromJSON # Should have more than 50 rows

original_url <- "https://api.worldbank.org/v2/country?format=json"
url_all_results(original_url) %>% fromJSON # Should have more than 50 rows


#----- END -----#






#----- Function to show user available country codes -----#

show_countries <- function() {
  
  countries_url <- "https://api.worldbank.org/v2/country?format=json" %>% url_all_results(.)
  countries <- countries_url %>% fromJSON(.)
  
  countries <- data.frame(countries[[2]]$iso2Code, countries[[2]]$name, stringsAsFactors = FALSE)
  colnames(countries) <- c("iso2Code", "country_name")
  
  return(countries)
}

show_countries() %>% head(20)

#----- END -----#


#----- Helper function: determine country input type (i.e. whether it's iso2Code or country name) -----#

countries <- show_countries()

country_input_type <- function(country_input, countries_dataframe) {
  
  # Logic: if the country_input is found among the iso2Codes, assume it's an iso2Code, 
  # if the country_input if not found in iso2Codes, then check to see if it's found among country_names, 
  # and if not, return 'invalid' 
  
  if (which(countries_dataframe$iso2Code %in% country_input) %>% length > 0) {
    country_input_type <- "iso2Code"
  } else if (which(countries_dataframe$country_name %in% country_input) %>% length > 0) {
    country_input_type <- "country_name"
  } else { 
    country_input_type <- "invalid"
    }
  return(country_input_type)
}

country_input_type("AU", countries)
country_input_type("Australia", countries)
country_input_type("AustraTYPOlia", countries)

#--- END ----- Helper function: determine country input type (i.e. whether it's iso2Code or country name) -----#




#----- Function to retrieve inflation data given iso2Code OR country_name -----#

retrieve_inflation_data <- function(country) {
  
  country_for_country_input_type <- country
  
  countries <- show_countries()
  
  country_input_type <- country_input_type(country_for_country_input_type, countries)
  
  if(country_input_type == "invalid") { stop("Invalid country input - please see select valid country from show_countries()") }
  
  if(country_input_type == "country_name") {
    
    index_of_country_in_countries <- which(countries$country_name %in% country)
    country <- countries$iso2Code[index_of_country_in_countries]
  }
  
  inflation_url <- paste0("https://api.worldbank.org/countries/", country, "/indicators/FP.CPI.TOTL.ZG")
  
  inflation_url <- inflation_url %>% url_all_results
  
  inflation_data <- inflation_url %>% fromJSON(.)
  
  return(inflation_data)
}


# Test
country <- "AU"
retrieve_inflation_data(country)

country <- "Australia"
retrieve_inflation_data(country)



#----- END -----#



country <- "AUsdf"
country <- "AU"
country <- "San Marino"
test_prices <- c(10, 10, 10, 10)
test_dates <- c(Sys.time()-(60 * 60 * 24 * 365 * 10), Sys.time()-(60 * 60 * 24 * 365 * 8), Sys.time()-(60 * 60 * 24 * 365 * 6), Sys.Date()- (60 * 60 * 24 * 365 * 6))

#----- Function that uses inflation data to in/deflate prices -----#

inflate <- function(price, country, from_date, to_date)
  # Later, it would be great to include a parameter for 'extrapolate = TRUE' - this could project for earlier and later dates, rather than returning NA
  library(lubridate)
  library(dplyr)
  

  # Declare a function that will accept any number of inflation values and produce one multiplier
  make_multiplier <- function(inflation_values) {
    multiplier <- inflation_values %>% {. / 100} %>% {. + 1} %>% prod(.)
    return(multiplier)
  }

  # Testing
  make_multiplier(c(1.23))
  make_multiplier(1.324)
  make_multiplier(c(1.23, 2.3))
  make_multiplier(c())
  
  

  
  # Validate that there are as many dates as prices (or just one date)
  if(length(price) != (length(from_date) | 1)) {
    stop("from_date must be a date or a vector of dates of the same length as the price(s)")
  }
  
  
  # If no to_date is provided, assume conversion into present day dollars is intended
  if(missing(to_date)) {
    to_date <- rep(Sys.Date, length(price))
  }
  
  # From inspection of WB's values for AU 2017 data, it appears they mean FY rather than calendar year (although the numbers aren't exactly conclusive)
  
  #----- Check that the selected country is valid, error if it isn't -----#


  
  
  # PLACEHOLDER - determine country input type
  
  
  
  iso2Code <- which(countries$iso2Code %in% country)
  
  # If an iso2Code wasn't provided, check that a country name wasn't
  name_of_country <- c()
  if (iso2Code %>% length == 0) { 
    
    name_of_country <- which(countries$country_name %in% country)
    
  }
  
  if(length(iso2Code) == 0 & length(name_of_country) == 0) {
    stop("Please provide a valid country name or iso2Code\n Run show_countries() for a comprehensive list")
  }
  
  # If it was a country name provided, grab the iso2Code
  if(length(name_of_country) > 0) {
    country <- which(countries$country_name %in% country) %>% countries[., "iso2Code"]
  }
  
  #----- END - Check that the selected country is valid, error if it isn't -----#
  
  
  # Get inflation data
  inflation_data <- retrieve_inflation_data(country)
  
  
  # A price from after the last period will return itself
  # A price from the last period will return itself
  # A price from the second last period will return itself inflated by the last period
  # Process: Identify which period the date is from, inflate by all later years
  
  year_of_to_date <- year(to_date)
  years <- from_date
  
  
  
  
  
  
  
  
  
  
  
  

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











