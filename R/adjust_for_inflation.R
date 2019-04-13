

#' @export

# A versatile function for converting past (nominal) values into current (real) values
# function_name aims to provide a stable, reliable, and easy to use function
# for quickly converting nominal prices into real prices
# It aims to ensure the user specifies methodologies and parameters, rather than
# making assumptions, but does so while maintaining good UX by prompting the
# user to provide more information


library(jsonlite)
library(dplyr)
library(lubridate)


#----- Function to take url and transform it into one with all the results on one page -----#

url_all_results <- function(original_url) {

# Append "?format=json" if url doesn't already have it
  if(!grepl("\\?", original_url)) {

    original_url <- paste0(original_url, "?format=json")

  }

  total_results <- original_url %>% fromJSON %>% .[[1]] %>% .$total
  cat("Generating URL to request all", total_results, "results")
  url_with_all_results <- paste0(original_url, "&per_page=", total_results)

  url_with_all_results %>% return

}


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

  if(country_input_type_string == "invalid") { stop(paste0("'", country, "'", " is not a valid country input - select a valid country from show_countries()")) }

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

  cat("Retrieving inflation data for", country, "\n")
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

adjust_for_inflation <- function(price, from_date, country, to_date, inflation_dataframe, countries_dataframe,
                                 extrapolate_future_method, future_averaging_period, future_rate,
                                 extrapolate_past_method, past_averaging_period, past_rate, periodicity) {
  # Later, it would be great to include a parameter for 'extrapolate = TRUE' - this could project for earlier and later dates, rather than returning NA
  library(lubridate)
  library(dplyr)


  #----- Deal with missing parameters and define functions -----#

  if(missing(extrapolate_future_method)) { extrapolate_future <- FALSE } else { extrapolate_future <- TRUE }
  if(missing(extrapolate_past_method)) { extrapolate_past <- FALSE } else { extrapolate_past <- TRUE }

  if(extrapolate_future) {
    if(missing(extrapolate_future_method)) { stop("'extrapolate_future_method' must be specified (it can take values 'average' or 'rate')") }
    if(!extrapolate_future_method %in% c("average", "rate")) { stop("'extrapolate_future_method' must be either 'average' or 'rate'")}
    if(extrapolate_future_method == 'average') { if(missing(future_averaging_period)) {
      stop("Please specify how many years' average to use when extrapolating forward ('future_averaging_period'
           can take any positive integer or 'all' to use an average of all available years' data")}}
    if(extrapolate_future_method == 'rate') {
      if(missing(future_rate)) { stop("Please specify the assumed rate of inflation for future periods (i.e. using 'future_rate' parameter)")}
      }

  }

  if(extrapolate_past) {
    if(missing(extrapolate_past_method)) { stop("'extrapolate_past_method' must be specified (it can take values 'average' or 'rate')") }
    if(!extrapolate_past_method %in% c("average", "rate")) { stop("'extrapolate_past_method' must be either 'average' or 'rate'")}
    if(extrapolate_past_method == 'average') { if(missing(past_averaging_period)) {
    stop("Please specify how many years' average to use when extrapolating forward ('past_averaging_period'
         can take any positive integer or 'all' to use an average of all available years' data")}}
    if(extrapolate_past_method == 'rate') {
      if(missing(past_rate)) { stop("Please specify the assumed rate of inflation for past periods (i.e. using 'past_rate' parameter)")}
    }
  }



  # If no to_date is provided, assume conversion into present day dollars is intended
  if(missing(to_date)) {
    to_date <- rep(Sys.Date(), length(price)) %>% substr(., 1, 4) %>% as.integer
  } else { to_date <- to_date %>% substr(., 1, 4) %>% as.integer }


  from_date <- substr(from_date, 1, 4) %>% as.integer # Ensure to / from are just years (for now)
  to_date <- substr(to_date, 1, 4) %>% as.integer # Ensure to / from are just years (for now)


  # Validate that there are as many dates as prices (or just one date)
  if(!(length(price) == length(from_date) | length(from_date) == 1)) {
    stop("from_date must be a date or a vector of dates of the same length as the price(s)")
  }





  if(missing(countries_dataframe)) {
  message("Retreiving countries")
  # Determine country input type
  countries_dataframe <- show_countries()
  }

  country_input_type_string <- country_input_type(country, countries_dataframe)
  country <- convert_to_iso2Code(country_input_type_string, country)
  # 'country' is iso2Code from here on

  name_of_country <- which(countries_dataframe$iso2Code %in% country) %>% countries_dataframe$country_name[.]




  if(missing(inflation_dataframe)) { inflation_dataframe <- retrieve_inflation_data(country) }
  inflation_dataframe <- inflation_dataframe %>% .[[2]] %>% .[ , c("value", "date")]
  inflation_dataframe$date <- inflation_dataframe$date %>% as.integer
  inflation_dataframe$value <- inflation_dataframe$value %>% as.numeric





  #----- Extrapolation logic -----#


  available_inflation_data <- inflation_dataframe %>% na.omit
  max_year_requested <- max(to_date)
  max_year_available_without_extrapolation <- max(available_inflation_data$date)

  if(extrapolate_future & (max_year_requested > max_year_available_without_extrapolation)) {

    if(extrapolate_future_method == 'average'){

      # Take one more than requested as we are looking for a rate (difference between years)
      number_of_years_to_use_for_average <- min(future_averaging_period, nrow(available_inflation_data))

      years_to_extrapolate <- seq(max_year_available_without_extrapolation + 1, max_year_requested, 1)
      average_inflation <- available_inflation_data[1:number_of_years_to_use_for_average, "value"] %>% as.numeric %>% mean

      extrapolated_dataframe <- data.frame(value=rep(average_inflation, length(years_to_extrapolate)), years_to_extrapolate, stringsAsFactors = FALSE) %>% map_df(rev) %>% as.data.frame
      colnames(extrapolated_dataframe) <- c("value", "date")

      inflation_dataframe <- rbind(extrapolated_dataframe, available_inflation_data)

    } # End future / average

    if(extrapolate_future_method == 'rate'){

      years_to_extrapolate <- seq(max_year_available_without_extrapolation + 1, max_year_requested, 1)
      average_inflation <- future_rate

      extrapolated_dataframe <- data.frame(value=rep(average_inflation, length(years_to_extrapolate)), years_to_extrapolate, stringsAsFactors = FALSE) %>% map_df(rev) %>% as.data.frame
      colnames(extrapolated_dataframe) <- c("value", "date")

      inflation_dataframe <- rbind(extrapolated_dataframe, available_inflation_data)

    } # End future / rate

  } # End outermost else for extrapolating future





  min_year_requested <- min(to_date) # Note that this is still the 'to_date'; taking whatever values are provided into a date in the past
  min_year_available_without_extrapolation <- min(available_inflation_data$date)

  if(extrapolate_past & (min_year_requested < min_year_available_without_extrapolation)) {

    if(extrapolate_past_method == 'average'){

      # Take one more than requested as we are looking for a rate (difference between years)
      number_of_years_to_use_for_average <- min(past_averaging_period, nrow(available_inflation_data))

      years_to_extrapolate <- seq(min_year_available_without_extrapolation - 1, min_year_requested, - 1) # Note these are in reverse
      average_inflation <- available_inflation_data[(nrow(available_inflation_data)-number_of_years_to_use_for_average + 1):nrow(available_inflation_data), "value"] %>% as.numeric %>% mean

      extrapolated_dataframe <- data.frame(value=rep(average_inflation, length(years_to_extrapolate)), years_to_extrapolate, stringsAsFactors = FALSE)
      colnames(extrapolated_dataframe) <- c("value", "date")

      inflation_dataframe <- rbind(available_inflation_data, extrapolated_dataframe)

    } # End past / average

    if(extrapolate_past_method == 'rate'){

      average_inflation <- past_rate

      extrapolated_dataframe <- data.frame(value=rep(average_inflation, length(years_to_extrapolate)), years_to_extrapolate, stringsAsFactors = FALSE)
      colnames(extrapolated_dataframe) <- c("value", "date")

      inflation_dataframe <- rbind(extrapolated_dataframe, available_inflation_data)

    } # End past / rate

  } # End outermost else for extrapolating past




  # If dates are outside of avaiable data and no extrapolation is set
  if(to_date > max(available_inflation_data$date) & !extrapolate_future) {
    stop(paste0("'to_date' (", to_date, ") is later than the latest available data (",
                max(available_inflation_data$date), "). Use an earlier 'to_date' or set 'extrapolate_future' to TRUE"))
  }

  if(to_date < min(available_inflation_data$date) & !extrapolate_past) {
    stop(paste0("'to_date' (", from_date, ") is earlier than the earliest available data (",
                min(available_inflation_data$date), "). Use a later 'to_date' or set an 'extrapolate_paste to TRUE'"))
  }














  # This function takes a single from value and single to value and creates a multiplier
  # Note that we don't pass inflation_dataframe object to this as it confuses mapply/sapply and they
  # Don't understand what to do with it
  make_multiplier <- function(from_input, to_input) {
    # Note dilligent use of inequalities (rightly) prevent current year's inflation being applied
    # if(length(from_date) == 1) { from_input <- rep(from_input, length(price))}
    # if(length(to_date) == 1) { to_input <- rep(to_input, length(price))}
    inflation_dataframe %>% dplyr::filter(date >= from_input & date < to_input | date <= from_input & date > to_input ) %>%
      .$value %>% {. / 100} %>% {. + 1} %>% { ifelse(from_input < to_input, prod(.), { 1 / prod(.) }) }
  }

  # from_input <- from_date
  # to_input <- to_date
  #
  # mapply(make_multiplier, from_input = from_date, to_input = to_date)

  multipliers <- mapply(make_multiplier, from_input = from_date, to_input = to_date)

  real_price <- price * multipliers

  real_price %>% return
}










#----- END -----#



# Tests
# (price, from_date, country, to_date, inflation_dataframe, countries_dataframe,
# extrapolate_future_method, future_averaging_period, future_rate,
# extrapolate_past_method, past_averaging_period, past_rate, periodicity)

price <- 10
country <- "AU"
from_date <- today() - (365 * 28)

inflation_dataframe <- inflation_dataframe_backup #dwd

adjust_for_inflation(price, from_date, country, to_date = 2017,
                     inflation_dataframe = inflation_dataframe, countries_dataframe = countries_dataframe)

adjust_for_inflation(price, from_date, country, to_date = 2019,
                     inflation_dataframe = inflation_dataframe, countries_dataframe = countries_dataframe,
                     extrapolate_future_method = "average", future_averaging_period = 3)

# Omitting to_date (assumes current)
adjust_for_inflation(price, from_date, country,
                     inflation_dataframe = inflation_dataframe, countries_dataframe = countries_dataframe,
                     extrapolate_future_method = "average", future_averaging_period = 3)

adjust_for_inflation(price, from_date, country, to_date = 2030,
                     inflation_dataframe = inflation_dataframe, countries_dataframe = countries_dataframe)
# Error in adjust_for_inflation(price, from_date, country, to = 2030, inflation_dataframe = inflation_dataframe,  :
# 'to_date' (2030) is later than the latest available data (2017). Use an earlier 'to_date' or set an 'extrapolation_method'


adjust_for_inflation(price, from_date, country, to_date = 2030,
                     inflation_dataframe = inflation_dataframe, countries_dataframe = countries_dataframe,
                     extrapolate_future_method = "average")



adjust_for_inflation(price, from_date, country, to_date = 2030,
                     inflation_dataframe = inflation_dataframe, countries_dataframe = countries_dataframe,
                     extrapolate_future_method = "average", future_averaging_period = 3)

adjust_for_inflation(price, from_date, country, to_date = 2030,
                     inflation_dataframe = inflation_dataframe, countries_dataframe = countries_dataframe,
                     extrapolate_future_method = "rate", future_averaging_period = 3)
# Error in adjust_for_inflation(price, from_date, country, to = 2030, inflation_dataframe = inflation_dataframe,  :
# Please specify the assumed rate of inflation for future periods (i.e. using 'future_rate' parameter)

adjust_for_inflation(price, from_date, country, to_date = 2030,
                     inflation_dataframe = inflation_dataframe, countries_dataframe = countries_dataframe,
                     extrapolate_future_method = "rate", future_rate = 3)


adjust_for_inflation(price, from_date, country, to_date = 2030,
                     inflation_dataframe = inflation_dataframe, countries_dataframe = countries_dataframe,
                     extrapolate_future_method = "average", future_averaging_period = 3)


#----- Testing past -----#

adjust_for_inflation(price, from_date, country, to_date = 1930,
                     inflation_dataframe = inflation_dataframe, countries_dataframe = countries_dataframe,
                     extrapolate_past_method = "average", past_averaging_period = 3)


adjust_for_inflation(price, from_date = "1991-04-14", country, to_date = 1930,
                     inflation_dataframe = inflation_dataframe, countries_dataframe = countries_dataframe,
                     extrapolate_past_method = "rate", past_rate = 2.5)

adjust_for_inflation(price, from_date = 1990, country, to_date = 1930,
                     inflation_dataframe = inflation_dataframe, countries_dataframe = countries_dataframe,
                     extrapolate_past_method = "rate", past_rate = 2.5)

adjust_for_inflation(price, from_date = 1970, country, to_date = 1930,
                     inflation_dataframe = inflation_dataframe, countries_dataframe = countries_dataframe,
                     extrapolate_past_method = "rate", past_rate = 2.5)

# Testing weird date formats
adjust_for_inflation(price, from_date = "2009-04-09 13:09:39 AEST", country, to_date = 1930,
                     inflation_dataframe = inflation_dataframe, countries_dataframe = countries_dataframe,
                     extrapolate_past_method = "rate", past_rate = 2.5)


#----- Testing vectorisation -----#


country <- "AU"
price <- c(10, 10, 10, 10)
from_date <- c(Sys.Date()-(365 * 10), Sys.Date()-(365 * 6), Sys.Date()-(365 * 4), Sys.Date()-(365 * 7))

inflation_dataframe <- inflation_dataframe_backup #dwd


adjust_for_inflation(price = test_prices, from_date = from_date, to_date = 2017, country = country,
                     inflation_dataframe = inflation_dataframe, countries_dataframe = countries_dataframe)





















