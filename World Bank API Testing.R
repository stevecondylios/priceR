

# A versatile function for converting past (nominal) values into current (real) values


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
  
  url_with_all_results %>% return
  
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

countries_dataframe <- show_countries()

country_input_type <- function(country_input, countries_dataframe) {
  
  # Logic: if the country_input is found among the iso2Codes, assume it's an iso2Code, 
  # if the country_input if not found in iso2Codes, then check to see if it's found among country_names, 
  # and if not, return 'invalid' 
  
  if (which(countries_dataframe$iso2Code %in% country_input) %>% length > 0) {
    country_input_type_string <- "iso2Code"
  } else if (which(countries_dataframe$country_name %in% country_input) %>% length > 0) {
    country_input_type_string <- "country_name"
  } else { 
    country_input_type_string <- "invalid"
    }
  return(country_input_type_string)
}

country_input_type("AU", countries_dataframe)
country_input_type("Australia", countries_dataframe)
country_input_type("AustraTYPOlia", countries_dataframe)

#--- END ----- Helper function: determine country input type (i.e. whether it's iso2Code or country name) -----#




#----- Convert any country input into iso2Code type -----#
# Accepts the type of country input and the country, and returns the relevant iso2Code

countries_dataframe <- show_countries()

convert_to_iso2Code <- function(country_input_type_string, country) {
  
  if(country_input_type_string == "iso2Code") { country <- country }
  
  if(country_input_type_string == "invalid") { stop(paste0('"', country, '"', " is not a valid country input - select a valid country from show_countries()")) }
  
  if(country_input_type_string == "country_name") {
    
    index_of_country_in_countries <- which(countries_dataframe$country_name %in% country)
    country <- countries_dataframe$iso2Code[index_of_country_in_countries]
  }

  return(country)
}


# Tests
country <- "AU"
country_input_type_string <- country_input_type(country, countries_dataframe)
convert_to_iso2Code(country_input_type_string, country)

country <- "Australia"
country_input_type_string <- country_input_type(country, countries_dataframe)
convert_to_iso2Code(country_input_type_string, country)

country <- "AustrTESTalia"
country_input_type_string <- country_input_type(country, countries_dataframe)
convert_to_iso2Code(country_input_type_string, country)





#----- END ----- Determine what to do based on country input type -----#









#----- Function to retrieve inflation data given iso2Code OR country_name -----#
# NOTE: this accepts only iso2Code
retrieve_inflation_data <- function(country, countries_dataframe) {
  
  if(missing(countries_dataframe)) { 
    cat("Validating iso2Code for", country, "\n")
    countries_dataframe <- show_countries() 
    }
  
  # Ensure we have an iso2Code
  country_input_type_string <- country_input_type(country, countries_dataframe)
  country <- convert_to_iso2Code(country_input_type_string, country)
  
  cat("Retrieving inflation data for", country)
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

country <- "AustraTESTlia"
retrieve_inflation_data(country)


#----- END ----- Function to retrieve inflation data given iso2Code OR country_name -----#



































#----- Function that uses inflation data to in/deflate prices -----#

adjust_for_inflation <- function(price, from, country, to, inflation_data) {
  # Later, it would be great to include a parameter for 'extrapolate = TRUE' - this could project for earlier and later dates, rather than returning NA
  library(lubridate)
  library(dplyr)
  
  # If no to_date is provided, assume conversion into present day dollars is intended
  if(missing(to_date)) {
    to_date <- rep(Sys.Date(), length(price))
  }
  
  
  # 'from' and 'to' parameters
  from_date <- from
  to_date <- to
  
  if(is.integer(to_date)) { to_date_format <- "year" }else{ to_date_format <- "date" }
  
  
  # Declare a function that will accept any number of inflation values and produce one multiplier by which to multiply the later value 
  make_multiplier <- function(inflation_values) {
    multiplier <- inflation_values %>% {. / 100} %>% {. + 1} %>% prod(.)
    return(multiplier)
  }

  # Testing
  make_multiplier(c(1.23))
  make_multiplier(1.324)
  make_multiplier(c(1.23, 2.3))

  
  

  
  # Validate that there are as many dates as prices (or just one date)
  if(length(price) != (length(from_date) | 1)) {
    stop("from_date must be a date or a vector of dates of the same length as the price(s)")
  }
  
  

  

  
  
  # Determine country input type
  countries_dataframe <- show_countries()
  country_input_type_string <- country_input_type(country, countries_dataframe)
  country <- convert_to_iso2Code(country_input_type_string, country)
  
  
  name_of_country <- which(countries_dataframe$country_name %in% country)
  
  
  
  
  iso2Code <- which(countries_dataframe$iso2Code %in% country)
  
  # If an iso2Code wasn't provided, check that a country name wasn't
  name_of_country <- c()
  if (iso2Code %>% length == 0) { 
    
    name_of_country <- which(countries_dataframe$country_name %in% country)
    
  }
  
  if(length(iso2Code) == 0 & length(name_of_country) == 0) {
    stop("Please provide a valid country name or iso2Code\n Run show_countries() for an exhaustive list")
  }
  
  # If it was a country name provided, grab the iso2Code
  if(length(name_of_country) > 0) {
    country <- which(countries_dataframe$country_name %in% country) %>% countries_dataframe[., "iso2Code"]
  }
  
  #----- END - Check that the selected country is valid, error if it isn't -----#
  
  
  
  
  
  
  
  
  # Get inflation data
  inflation_data <- retrieve_inflation_data(country)
  
  # A price from after the last period will return itself
  # A price from the last period will return itself
  # A price from the second last period will return itself inflated by the last period
  # Process: Identify which period the date is from, inflate by all later years
  
  if(missing(to_date)) { to_date <- today() }
  
  year_of_to_date <- year(to_date)
  years <- from_date
  
  
  
  
  
}
  
  


#----- END -----#



# Tests
# inflate <- function(price, country, from_date, to_date)

price <- 10
country <- "AU"
from_date <- today() - 200
inflate(price, country, from_date)








country <- "AU"
test_prices <- c(10, 10, 10, 10)
test_dates <- c(Sys.time()-(60 * 60 * 24 * 365 * 10), Sys.time()-(60 * 60 * 24 * 365 * 8), Sys.time()-(60 * 60 * 24 * 365 * 6), Sys.Date()- (60 * 60 * 24 * 365 * 6))
test_dates <- c(Sys.time()-(60 * 60 * 24 * 365 * 6), Sys.Date()- (60 * 60 * 24 * 365 * 6))


adjust_for_inflation(test_prices, test_dates) 



