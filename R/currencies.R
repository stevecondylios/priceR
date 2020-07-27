




#' Display link to further information
#'
#'
#' @name display_api_info
#'
#' @usage display_api_info()
#'
#'
#' @examples
#' \dontrun{
#' # Display a message indicating where further documentation can be found
#' }
#'



display_api_info <- function() {

      if(getOption("forex_warning", TRUE)) {
      # This message appears on first use only of this/similar functions
      message(paste(
              "For full currency exchange rate API documentation visit:\n",
              "https://exchangerate.host/#/#docs\n",
              "(this message will only appear once per session)"))
      options("forex_warning"=FALSE)
  }

}








#' Retrieve available currencies and their respective symbols/codes
#' @name currencies
#'
#' @usage currencies()
#'
#' @return A data.frame of available currencies and their respective symbols/codes
#'
#' @import dplyr purrr
#' @importFrom jsonlite fromJSON
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Display available currencies and their respective symbols/codes
#' currencies()
#' #                                description code
#' # 1              United Arab Emirates Dirham  AED
#' # 2                           Afghan Afghani  AFN
#' # 3                             Albanian Lek  ALL
#' # 4                            Armenian Dram  AMD
#' # 5            Netherlands Antillean Guilder  ANG
#' # 6                           Angolan Kwanza  AOA
#' # 7                           Argentine Peso  ARS
#' }
#'
#'


currencies <- function() {

  display_api_info()

  symbols <- 'https://api.exchangerate.host/symbols' %>% fromJSON %>% .$symbols

  symbols %>%
    {
    data.frame(
          description = map_chr(., ~ .x$description ) %>% unname,
          code = map_chr(., ~ .x$code ) %>% unname
      )
    }

}














#'
#' Retrieve the latest exchange rates between the provided currency code
#' @name exchange_rate_latest
#'
#' @usage exchange_rate_latest(currency)
#'
#' @param currency A currency code (see currencies() for supported codes)
#'
#' @return A data.frame containing the latest exchange rates between the provided currency code
#'     and each other available currency
#'
#' @import dplyr purrr
#' @importFrom jsonlite fromJSON
#' @importFrom utils stack
#'
#' @export
#'
#' @examples
#' \dontrun{
#'
#' exchange_rate_latest("AUD")
#' # Daily AUD exchange rate as at end of day 2020-07-27 GMT
#' #     currency one_aud_is_equivalent_to
#' # 1        AED                  2.61894
#' # 2        AFN                 54.47724
#' # 3        ALL                 75.51799
#' # 4        AMD                343.40193
#' # 5        ANG                  1.26829
#' # 6        AOA                400.54604
#'
#'
#' # Defaults to USD
#' exchange_rate_latest()
#' # Daily USD exchange rate as at end of day 2020-07-27 GMT
#' #     currency one_usd_is_equivalent_to
#' # 1        AED                   3.6730
#' # 2        AFN                  76.4035
#' # 3        ALL                 105.9129
#' # 4        AMD                 481.6162
#' # 5        ANG                   1.7788
#' # 6        AOA                 561.7599
#'
#'
#' }
#'

exchange_rate_latest <- function(currency = "USD") {

  display_api_info()

  dat <- fromJSON(paste0("https://api.exchangerate.host/latest?base=", currency))

  cat("Daily", currency, "exchange rate as at end of day", dat$date, "GMT", "\n")

  # "EOD / End of Day historical exchange rates, which become available at
  # 00:05am GMT for the previous day and are time stamped at one second before midnight."

  # options(scipen=999)
  # options(digits=1)

  col_name <- paste0("one_", tolower(currency), "_is_equivalent_to")

  dat$rates %>%
    map_dbl(~ .x[1]) %>%
    stack %>%
    .[,c(2,1)] %>%
    `colnames<-`(c("currency", col_name))

}

















#'
#' Creates date ranges so as to batch up large API calls into many smaller ones
#' @name make_dates
#'
#' @usage make_dates(start_date, end_date, n_days)
#'
#' @param start_date A start date (of the form "2010-01-01")
#' @param end_date An end date
#' @param n_days The maximum number of days in each period
#'
#' @return A data.frame containing start and end dates for periods of length no longer than n_days
#'
#' @import dplyr
#'
#'
#' @examples
#'
#' # Simple test
#' start_date = "2010-01-01"
#' end_date = "2020-06-30"
#' n_days = 365
#' priceR:::make_dates(start_date, end_date, n_days)
#'
#' # With lots of periods
#' start_date = "2010-01-01"
#' end_date = "2020-06-30"
#' n_days = 20
#' priceR:::make_dates(start_date, end_date, n_days)
#'
#' # Less than one period
#' start_date = "2020-01-01"
#' end_date = "2020-06-30"
#' n_days = 365
#' priceR:::make_dates(start_date, end_date, n_days)
#'
#'
#'


make_dates <- function(start_date, end_date, n_days) {

  # Helper for historical_exchange_rates, since API returns max 365 days at a time

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  range_in_days <- (end_date - start_date) %>% as.integer
  days_covered_by_period <- range_in_days + 1

  # In cases where we don't need mutliple periods,
  # simply return a 1 row data.frame

  if(days_covered_by_period <= n_days) {
    dates <- data.frame(start_date=start_date,
                      end_date=end_date)
    return(dates)
  }


  from_ind <- seq(1, days_covered_by_period, n_days)
  to_ind <- seq(n_days + 1, days_covered_by_period, n_days)

  # add the end date to the to_ind (except for cases where it's an exact multiple of n_days)
  if(days_covered_by_period != last(to_ind)) {
    to_ind <- to_ind %>% c(days_covered_by_period)
  }

  date_seq <- (start_date:end_date) %>% as.Date(origin = "1970-01-01")


  dates <- data.frame(start_date=date_seq[from_ind],
                      end_date=date_seq[to_ind])

  return(dates)
}


















#' Retrieve historical exchange rates
#'
#' Retrieves historical exchange rates between a currency pair - retrieves max. 365 days' data
#' @name retrieve_historical_rates
#'
#' @usage retrieve_historical_rates(from, to, start_date, end_date)
#'
#' @param from A currency code (see currencies() for supported codes)
#' @param to A currency code
#' @param start_date A start date (of the form "2010-01-01")
#' @param end_date An end date
#'
#'
#' @return A data.frame containing exchange rate data for select currency pair
#'
#' @import dplyr purrr
#' @importFrom jsonlite fromJSON
#' @importFrom utils stack
#'
#' @examples
#' \dontrun{
#' # Note date range >365 days', yet only 365 days' returned.
#' # Use historical_exchange_rates() for > 365 days'.
#'  priceR:::retrieve_historical_rates("USD", to = "AUD",
#'                                     start_date = "2018-01-01",
#'                                     end_date = "2020-06-30")
#' }
#'
#'


retrieve_historical_rates <- function(from, to, start_date, end_date) {

  # "https://api.exchangerate.host/timeseries?start_date=2020-01-01&end_date=2020-01-04"
  endpoint <- paste0("https://api.exchangerate.host/timeseries?start_date=",
                     start_date,
                     "&end_date=",
                     end_date,
                     "&base=",
                     from,
                     "&symbols=",
                     to)

  dat <- endpoint %>%
    fromJSON

  col_name <- paste0("one_", from, "_equivalent_to_x_", to)

  dat$rates %>%
      map_dbl(~ .x[1][[1]][[1]]) %>%
      stack %>%
      .[,c(2,1)] %>%
      `colnames<-`(c("date", col_name))
}















#' Retrieve historical exchange rates
#'
#' Retrieves historical exchange rates between a currency pair
#' @name historical_exchange_rates
#'
#' @usage historical_exchange_rates(from, to, start_date, end_date)
#'
#' @param from A currency code (see currencies() for supported codes)
#' @param to A currency code
#' @param start_date A start date (of the form "2010-01-01")
#' @param end_date An end date
#'
#'
#' @return A data.frame containing exchange rate data for select currency pair
#'
#' @import dplyr purrr
#' @importFrom jsonlite fromJSON
#'
#' @export
#'
#' @examples
#' \dontrun{
#' # Note date range >365 days', and returns 912 rows (as expected)
#' historical_exchange_rates("USD", to = "AUD", start_date = "2018-01-01", end_date = "2020-06-30")
#'
#' historical_exchange_rates("USD", to = "AUD",
#'                           start_date = "2020-01-01", end_date = "2020-06-30")
#'
#'
#' historical_exchange_rates("AUD", to = "USD",
#'                           start_date = "2010-01-01", end_date = "2020-06-30")
#'
#'
#' historical_exchange_rates("AUD", to = "USD",
#'                           start_date = "2010-01-01", end_date = "2020-06-30")
#'
#'
#' }
#'
#'


historical_exchange_rates <- function(from, to, start_date, end_date) {

  display_api_info()

  api_time_splits <- make_dates(start_date, end_date, 365)

  # pmap*() variants map over rows of a data.frame
  pmap_dfr(api_time_splits, retrieve_historical_rates, from = from, to = to)


}













