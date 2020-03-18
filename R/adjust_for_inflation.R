
#' Generate a World Bank API URL that will return all results for a given indicator in JSON format
#'
#' results and returns JSON format
#'
#' @name url_all_results
#'
#' @usage url_all_results(original_url)
#'
#' @param original_url A World Bank API URL. E.g. "https://api.worldbank.org/v2/country".
#'
#' @import dplyr
#' @import stringr
#'
#' @export
#'
#' @examples
#'
#' # Provide a World Bank API URL and `url_all_results` will convert it into one with all results
#' # for that indicator
#'   original_url <- "https://api.worldbank.org/v2/country" # Note: no ?format=json on url
#'   url_all_results(original_url)
#' # "https://api.worldbank.org/v2/country?format=json&per_page=304"
#'
#'
#'



url_all_results <- function(original_url) {

# Append "?format=json" if url doesn't already have it
  if(!grepl("\\?", original_url)) {

    original_url <- paste0(original_url, "?format=json")

  }

  total_results <- original_url %>% fromJSON %>% .[[1]] %>% .$total
  cat("Generating URL to request all", total_results, "results\n")
  url_with_all_results <- paste0(original_url, "&per_page=", total_results)

  url_with_all_results %>% return

}








#'  Show available country codes
#'
#' `show_countries` calls the World Bank API and retrieves a list of available countries and regions
#' @name show_countries
#'
#' @usage show_countries()
#'
#' @import dplyr
#' @import stringr
#' @import gsubfn
#' @importFrom jsonlite fromJSON
#'
#' @export
#'
#' @examples
#'
#' # Simply call show_countries() to receive a dataframe of all countries (and regions) and their
#' # iso2Code
#'
#' # show_countries()
#' #       iso2Code         country_name
#' #   1         AW                Aruba
#' #   2         AF          Afghanistan
#' #   3         A9               Africa
#' #   4         AO               Angola
#' #   Etc
#'
#'

show_countries <- function() {

  countries_url <- "https://api.worldbank.org/v2/country?format=json" %>% url_all_results(.)
  countries <- countries_url %>% fromJSON(.)

  countries <- data.frame(countries[[2]]$iso2Code, countries[[2]]$name, stringsAsFactors = FALSE)
  colnames(countries) <- c("iso2Code", "country_name")

  return(countries)
}






#' Determines whether country input is a country name or iso2Code
#'
#' Determines whether a string is a country name, an iso2Code, or invalid (not a World Bank API
#' country/region)
#' @name country_input_type
#'
#' @usage country_input_type(country_input, countries_dataframe)
#'
#' @param country_input A country/region the user wishes to validate (string) E.g. "Australia".
#' @param countries_dataframe A dataframe containing available iso2Code and country_name
#' (see show_countries()).
#'
#' @import dplyr
#' @import stringr
#' @import gsubfn
#' @importFrom jsonlite fromJSON
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Assign so as to save on API calls - recommended
#' countries_dataframe <- show_countries()
#'
#'   country <- "Australia"
#'   country_input_type(country, countries_dataframe)
#' # [1] "country_name"
#'
#'   country <- "AU"
#'   country_input_type(country, countries_dataframe)
#' # [1] "iso2Code"
#'
#'   country <- "something incorrect"
#'   country_input_type(country, countries_dataframe)
#' # [1] "invalid"
#' }

country_input_type <- function(country_input, countries_dataframe) {

  # Logic: if the country_input is found among the iso2Codes, assume it's an iso2Code,
  # if the country_input if not found in iso2Codes, then check to see if it's found among
  # country_names,
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



























#' Convert any country input into its iso2Code
#'
#' `convert_to_iso2Code` accepts the type of country input and the country, and returns the relevant iso2Code
#' @name convert_to_iso2Code
#'
#' @usage convert_to_iso2Code(country_input_type_string, country, countries_dataframe)
#'
#' @param country_input_type_string Either "country_name" or "iso2Code" - use country_input_type(country, countries_dataframe) to determine or assign manually.
#' @param country A country/region name or iso2Code.
#' @param countries_dataframe The output of show_countries()
#'
#' @import dplyr
#' @import stringr
#' @import gsubfn
#' @importFrom jsonlite fromJSON
#'
#' @export
#'
#' @examples
#'
#' \dontrun{
#' # Assign so as to save on API calls (recommended)
#' countries_dataframe <- show_countries()
#'
#' country <- "Australia"
#' country_input_type_string <- "country_name"
#' convert_to_iso2Code(country_input_type_string, country, countries_dataframe)
#' # [1] "AU"
#'
#' country <- "AU"
#' country_input_type_string <- "iso2Code"
#' convert_to_iso2Code(country_input_type_string, country, countries_dataframe)
#' # [1] "AU"
#' }
#'

convert_to_iso2Code <- function(country_input_type_string, country, countries_dataframe) {

  if(country_input_type_string == "iso2Code") { country <- country }

  if(country_input_type_string == "invalid") { stop(paste0("'", country, "'", " is not a valid country input - select a valid country from show_countries()")) }

  if(country_input_type_string == "country_name") {

    index_of_country_in_countries <- which(countries_dataframe$country_name %in% country)
    country <- countries_dataframe$iso2Code[index_of_country_in_countries]
  }

  return(country)
}



























#' Retrieve historical inflation data
#'
#' Retrieve inflation data for any country/region (using iso2Code or country_name)
#' @name retrieve_inflation_data
#'
#' @usage retrieve_inflation_data(country, countries_dataframe)
#'
#' @param country A country_name or iso2code (see show_countries() for complete list of available inputs).
#' @param countries_dataframe The output from show_countries(). It is optional, but if not provided, it will be retrieved via the API.
#'
#' @import dplyr
#' @import stringr
#' @import gsubfn
#' @importFrom jsonlite fromJSON
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Retrieve inflation data for any country (or iso2Code)
#' country <- "AU"
#' inflation_dataframe <- retrieve_inflation_data(country)
#'
#' country <- "Australia"
#' countries_dataframe <- show_countries()
#' inflation_dataframe <- retrieve_inflation_data(country, countries_dataframe)
#' }
#' # inflation_dataframe
#' #    indicator.id                       indicator.value country.id country.value     value
#' #  FP.CPI.TOTL.ZG Inflation, consumer prices (annual %)         AU     Australia      <NA>
#' #  FP.CPI.TOTL.ZG Inflation, consumer prices (annual %)         AU     Australia   1.94864
#' #  FP.CPI.TOTL.ZG Inflation, consumer prices (annual %)         AU     Australia   1.27699
#' #  FP.CPI.TOTL.ZG Inflation, consumer prices (annual %)         AU     Australia   1.50836
#' #  Etc
#'
#'


retrieve_inflation_data <- function(country, countries_dataframe) {

  if(missing(countries_dataframe)) {
    cat("Validating iso2Code for", country, "\n")
    countries_dataframe <- show_countries()
    }

  # Ensure we have an iso2Code
  country_input_type_string <- country_input_type(country, countries_dataframe)
  country <- convert_to_iso2Code(country_input_type_string, country, countries_dataframe)

  cat("Retrieving inflation data for", country, "\n")
  inflation_url <- paste0("https://api.worldbank.org/countries/", country, "/indicators/FP.CPI.TOTL.ZG")

  inflation_url <- inflation_url %>% url_all_results

  inflation_data <- inflation_url %>% fromJSON(.)

  return(inflation_data)
}












#' Convert nominal prices into real prices
#'
#' Inflate/deflate prices from any year to any year, using World Bank inflation data and assumptions only where necessary.
#' Typically used for converting past (nominal) values into current (real) values. This uses World Bank inflation data where available,
#' but allows for both historical and future assumptions in extrapolation.
#' @name adjust_for_inflation
#'
#' @usage adjust_for_inflation(price, from_date, country, to_date, inflation_dataframe,
#' countries_dataframe, extrapolate_future_method, future_averaging_period, future_rate,
#' extrapolate_past_method, past_averaging_period, past_rate)
#'
#' @param price A price (or prices).
#' @param from_date A date(s) from which the prices will be converted.
#' @param country A country or region in whose currency the prices are denominated.
#' @param to_date A date(s) to which the prices will be converted.
#' @param inflation_dataframe The R object (list) representing the JSON retrieved by calling retrieve_inflation_data().
#' @param countries_dataframe The R object (data.frame) representing the JSON retreived by calling show_countries().
#' @param extrapolate_future_method The extrapolation method that shall be used if extrapolation into the future is required. Options are 'average' or 'rate'.
#' @param future_averaging_period The number of recent periods to average in order to extrapolate forward (if 'average' is method being used).
#' @param future_rate An assumed rate of inflation to use for extrapolating forward (if 'rate' is method being used).
#' @param extrapolate_past_method The extrapolation method that shall be used if extrapolation from the earliest available data to some even earlier period is required.
#' @param past_averaging_period The number of periods back from the earliest available inflation data for a given country to average in order to extrapolate into the past (if 'average' is method being used).
#' @param past_rate An assumed rate of inflation to use for extrapolating from the earliest available data to some even earlier period (if 'rate' is method being used).
#'
#'
#' @import dplyr
#' @import stringr
#' @import gsubfn
#' @importFrom jsonlite fromJSON
#' @importFrom purrr map_df
#' @importFrom stats na.omit
#'
#' @export
#'
#'
#' @examples
#' \dontrun{
#' # Assign these variables once
#' country <- "AU"
#' inflation_dataframe <- retrieve_inflation_data(country)
#' countries_dataframe <- show_countries()
#'
#' # Convert $100 from 2005 into 2017 dollars
#'
#' adjust_for_inflation(100, 2005, country, to_date = 2017,
#' inflation_dataframe = inflation_dataframe,
#' countries_dataframe = countries_dataframe)
#'
#' # [0] 134.96287 # i.e. $100 in 2005 had the same purchasing power as $134.96 in 2017
#' }
#'






adjust_for_inflation <- function(price,
                                 from_date,
                                 country,
                                 to_date,
                                 inflation_dataframe,
                                 countries_dataframe,
                                 extrapolate_future_method, # exf_method
                                 future_averaging_period, #exf_period
                                 future_rate, # exf_rate
                                 extrapolate_past_method, #exp_method
                                 past_averaging_period, # exp_period
                                 past_rate) { # exp_rate

  # Later, it would be great to include a parameter for 'extrapolate = TRUE' - this could project for earlier and later dates, rather than returning NA


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
    stop("from_date must be a date or a vector of dates of the same length as price")
  }

  if(missing(countries_dataframe)) {
  message("Retrieving countries data")
  # Determine country input type
  countries_dataframe <- show_countries()
  }

  country_input_type_string <- country_input_type(country, countries_dataframe)
  country <- convert_to_iso2Code(country_input_type_string, country, countries_dataframe)
  # 'country' is iso2Code from here on

  name_of_country <- which(countries_dataframe$iso2Code %in% country) %>% countries_dataframe$country_name[.]


  if(missing(inflation_dataframe)) { inflation_dataframe <- retrieve_inflation_data(country, countries_dataframe = countries_dataframe) }
  inflation_dataframe <- inflation_dataframe %>% .[[2]] %>% .[ , c("value", "date")]
  inflation_dataframe$date <- inflation_dataframe$date %>% as.integer
  inflation_dataframe$value <- inflation_dataframe$value %>% as.numeric



  #----- Extrapolation logic -----#

  # Extrapolating future
  available_inflation_data <- inflation_dataframe %>% na.omit
  max_year_requested <- max(c(to_date, from_date)) # from_date included here for edge case where
  # going from future from date to future to_date AND where from_date > to_date
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




  # Extrapolating past
  min_year_requested <- min(to_date, from_date) # Note that this is still the 'to_date'; taking whatever values are provided into a date in the past
  min_year_available_without_extrapolation <- min(available_inflation_data$date)

  if(extrapolate_past & (min_year_requested >= min_year_available_without_extrapolation)) {
    warning(paste0("Past extrapolation not required as data available for ", country, " back to ", min_year_available_without_extrapolation))
    }


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

      years_to_extrapolate <- seq(min_year_available_without_extrapolation - 1, min_year_requested, -1)
      average_inflation <- past_rate

      extrapolated_dataframe <- data.frame(value=rep(average_inflation, length(years_to_extrapolate)), years_to_extrapolate, stringsAsFactors = FALSE)
      colnames(extrapolated_dataframe) <- c("value", "date")

      inflation_dataframe <- rbind(available_inflation_data, extrapolated_dataframe)

    } # End past / rate

  } # End outermost else for extrapolating past




  # If dates are outside of avaiable data and no extrapolation is set
  if(max(to_date) > max(available_inflation_data$date) & !extrapolate_future) {
    stop(paste0("'to_date' (", to_date, ") is/contains a later date than the latest available data (",
                max(available_inflation_data$date), ").\nTry setting 'extrapolate_future' to TRUE or using an earlier 'to_date'"))
  }

  if(min(to_date) < min(available_inflation_data$date) & !extrapolate_past) {
    stop(paste0("'to_date' (", from_date, ") is/contains an earlier date than the earliest available data (",
                min(available_inflation_data$date), ").\nTry setting 'extrapolate_past' to TRUE or using a later 'to_date'"))
  }



  # This function takes a single from value and single to value and creates a multiplier
  # Note that we don't pass inflation_dataframe object to this as it confuses mapply/sapply and they
  # Don't understand what to do with it
  # from_input <- from_date; to_input <- to_date
  make_multiplier <- function(from_input, to_input) {
    # Note dilligent use of inequalities (rightly) prevent current year's inflation being applied
    # if(length(from_date) == 1) { from_input <- rep(from_input, length(price))}
    # if(length(to_date) == 1) { to_input <- rep(to_input, length(price))}
    inflation_dataframe %>% filter(date > from_input & date <= to_input | date < from_input & date >= to_input ) %>%
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




#' @rdname adjust_for_inflation
#' @export
afi <- adjust_for_inflation




