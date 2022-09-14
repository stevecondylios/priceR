




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
#' # 366 days (note 2020 was a leap year)
#' start_date = "2019-07-30"
#' end_date = "2020-07-29"
#' n_days = 365
#' priceR:::make_dates(start_date, end_date, n_days)
#'
#' # 365 days
#' start_date = "2019-07-30"
#' end_date = "2020-07-28"
#' n_days = 365
#' priceR:::make_dates(start_date, end_date, n_days)
#'
#' # 1095 days (3 years)
#' start_date = "2019-07-30"
#' end_date = "2022-07-28"
#' n_days = 365
#' priceR:::make_dates(start_date, end_date, n_days)
#'


make_dates <- function(start_date, end_date, n_days) {

  # Helper for historical_exchange_rates, since API returns max 365 days at a time

  start_date <- as.Date(start_date)
  end_date <- as.Date(end_date)

  range_in_days <- (end_date - start_date) %>% as.integer %>%

    # Note that we add one since 2020-01-01 to 2020-01-02 is 2 days, yet one
    # minus the other gives a difference of only 1, so we add one to the difference

    `+`(1)

  # In cases where we don't need multiple periods,
  # simply return a 1 row data.frame

  if(range_in_days <= n_days) {
    dates <- data.frame(start_date=start_date,
                      end_date=end_date)
    return(dates)
  }


  from_ind <- seq(1, range_in_days, n_days)
  to_ind <- seq(n_days, range_in_days, n_days)

  # add the end date to the to_ind (except for cases where it's an exact multiple of n_days)
  if(range_in_days != last(to_ind)) {
    to_ind <- to_ind %>% c(range_in_days)
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
      `colnames<-`(c("date", col_name)) %>%
    mutate(date = as.Date(date))
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
#' Retrieve AUD to USD exchange rates
#' au <- historical_exchange_rates(from = "AUD", to = "USD",
#'                           start_date = "2010-01-01", end_date = "2020-06-30")
#'
#' # Retrieve AUD to EUR exchange rates
#' ae <- historical_exchange_rates(from = "AUD", to = "EUR",
#'                           start_date = "2010-01-01", end_date = "2020-06-30")
#'
#' # Combine
#' cur <- au %>% left_join(ae, by = "date")
#'
#' head(cur)
#'}
#'
#'


historical_exchange_rates <- function(from, to, start_date, end_date) {

  # Validate currencies

  # Retrieved via: priceR::currencies()
  # Hard coded to avoid excessive API calls
  valid_currency_codes <- c(
    "AED", "AFN", "ALL", "AMD", "ANG", "AOA", "ARS", "AUD", "AWG",
    "AZN", "BAM", "BBD", "BDT", "BGN", "BHD", "BIF", "BMD", "BND",
    "BOB", "BRL", "BSD", "BTC", "BTN", "BWP", "BYN", "BZD", "CAD",
    "CDF", "CHF", "CLF", "CLP", "CNH", "CNY", "COP", "CRC", "CUC",
    "CUP", "CVE", "CZK", "DJF", "DKK", "DOP", "DZD", "EGP", "ERN",
    "ETB", "EUR", "FJD", "FKP", "GBP", "GEL", "GGP", "GHS", "GIP",
    "GMD", "GNF", "GTQ", "GYD", "HKD", "HNL", "HRK", "HTG", "HUF",
    "IDR", "ILS", "IMP", "INR", "IQD", "IRR", "ISK", "JEP", "JMD",
    "JOD", "JPY", "KES", "KGS", "KHR", "KMF", "KPW", "KRW", "KWD",
    "KYD", "KZT", "LAK", "LBP", "LKR", "LRD", "LSL", "LYD", "MAD",
    "MDL", "MGA", "MKD", "MMK", "MNT", "MOP", "MRO", "MRU", "MUR",
    "MVR", "MWK", "MXN", "MYR", "MZN", "NAD", "NGN", "NIO", "NOK",
    "NPR", "NZD", "OMR", "PAB", "PEN", "PGK", "PHP", "PKR", "PLN",
    "PYG", "QAR", "RON", "RSD", "RUB", "RWF", "SAR", "SBD", "SCR",
    "SDG", "SEK", "SGD", "SHP", "SLL", "SOS", "SRD", "SSP", "STD",
    "STN", "SVC", "SYP", "SZL", "THB", "TJS", "TMT", "TND", "TOP",
    "TRY", "TTD", "TWD", "TZS", "UAH", "UGX", "USD", "UYU", "UZS",
    "VEF", "VES", "VND", "VUV", "WST", "XAF", "XAG", "XAU", "XCD",
    "XDR", "XOF", "XPD", "XPF", "XPT", "YER", "ZAR", "ZMW", "ZWL"
    )

  from = toupper(from)
  to = toupper(to)

  if (!all(c(from, to) %in% valid_currency_codes)){
    invalid_currencies <- c(from, to)[which(!c(from, to) %in% valid_currency_codes)]
    error_message = paste0("Invalid currency code(s): " ,"\"", paste0(invalid_currencies, collapse="\", \""), "\"",
". Run currencies()
  to view all ", length(valid_currency_codes), " valid currency codes.")
    stop(error_message)
  }

  display_api_info()

  api_time_splits <- make_dates(start_date, end_date, 365)

  # pmap*() variants map over rows of a data.frame
  pmap_dfr(api_time_splits, retrieve_historical_rates, from = from, to = to)


}












#'
#' Information for each of 191 currencies
#'
#'
#' @return A data.frame containing currency information for 191 currencies.
#'     Currency information includes: name, iso code, currency symbol (and
#'     alternative symbols if applicable), subunit, number of subunits per major
#'     unit, whether the currency symbol ought to appear before or after the
#'     number of units, display format, html entity, decimal mark,
#'     thousands separator, iso numeric, and smallest denomination.
#'
#'
#'
#' @examples
#' head(currency_info)

"currency_info"



### functions for `convert_currencies()`




#' Removes redundant API calls of currency pairs. That is, revmoes the need
#' to for separate calls for both
#' `from = EUR, to = USD` and `from = USD, to = EUR`
#'
#' @name pminmax
#'
#' @usage pminmax(x, y)
#'
#' @param x A currency code (see currencies() for supported codes)
#' @param y A currency code
#'
#' @return A character vector
#'
#'
#' @export
#'
#' @examples
#'
#' # See: https://stackoverflow.com/a/73254014/9059865
#'
#'
#'

pminmax <- function(x, y) {
  paste(pmin.int(x, y), pmax.int(x, y), sep = ".")
}

#' Wrapper around `priceR::historical_exchange_rates()` with slight modifications
#' to structure of inputs and output
#'
#' @name from_to_dates_rates
#'
#' @usage from_to_dates_rates(from, to, dates)
#'
#' @param from A currency code (see currencies() for supported codes)
#' @param to A currency code
#' @param dates A list of date ranges
#'
#' @return A data.frame with two columns: date (of class Date), and rate
#'     (of class numeric).
#'
#'
#' @export
#'
#' @examples
#' \dontrun{
#' library(lubridate)
#' from_to_dates_rates("AUD", "USD", dates = list(today()-10, today()))
#' }
#'

from_to_dates_rates <- function(from, to, dates) {
  historical_exchange_rates(
    from = from,
    to = to,
    start_date = dates[[1]],
    end_date = dates[[2]]
  ) %>%
    purrr::set_names("date", "rate")
}

#' Convert Currencies
#'
#' Vectorized approach to converting prices across potentially different dates
#' and between different currencies.
#'
#' @param price_start Numeric vector of prices in terms of `from` currenc(ies).
#' @param from Character vector of currenc(ies) of `price_start`.
#' @param to Character vector of currenc(ies) to convert `price_start` to.
#' @param date Date vector specifying date of exchange rate to use.
#' @param floor_unit Character string. Default is "day" meaning that `date` will
#'   be converted based on daily conversion rates. Changing to "week" will
#'   change conversions to be based on the start of the week of `date`.
#'
#' @return Numeric vector of `price_start` now in the `to` currenc(ies).
#' @export
#'
#' @examples
#' \dontrun{
#' library(dplyr)
#'
#' sales_transactions <- tibble(
#'   local_price = c(100, 80, 9200, 90),
#'   local_currency = c("USD", "EUR", "JPY", "USD"),
#'   final_currency = c("EUR", "USD", "USD", "JPY"),
#'   date_transaction = lubridate::ymd(c(20200601, 20200609, 20200614, 20200623))
#' )
#' # Some made-up sales transactions of different values and currencies
#' sales_transactions %>%
#'   mutate(
#'     converted_price = convert_currencies(
#'       price_start = local_price,
#'       from = local_currency,
#'       to = final_currency,
#'       date = date_transaction
#'     )
#'   )
#' }
convert_currencies <- function(price_start,
                               from,
                               to,
                               date = lubridate::today(),
                               floor_unit = "day") {
  rates_start <- dplyr::tibble(
    from = from,
    to = to,
    date = date %>%
      as.Date() %>%
      lubridate::floor_date(floor_unit)
  ) %>%
    dplyr::mutate(from_to = pminmax(from, to)) %>%
    dplyr::distinct(from_to, date, .keep_all = TRUE)

  # When passing things to the priceR API it is much faster to send over a range
  # of dates rather than doing individual API calls for each date (even when
  # there are many redundant dates between `from` and `to` values).

  rates_end <- rates_start %>%
    dplyr::group_by(from_to) %>%
    dplyr::summarise(
      date_range = list(range(date)),
      from = from[[1]],
      to = to[[1]],
      rates_lookup = purrr::pmap(.l = list(from, to, date_range),
                          .f = from_to_dates_rates)
    ) %>%
    dplyr::select(-date_range) %>%
    tidyr::unnest(rates_lookup)

  rates_lookup <- rates_end %>%
    dplyr::semi_join(rates_start, "date")

  # this step makes it so could convert either "from" or "to" currency
  rates_lookup <- dplyr::bind_rows(
    rates_lookup,
    rates_lookup %>%
      dplyr::rename(from = to, to = from) %>%
      dplyr::mutate(rate = 1 / rate)
  ) %>%
    dplyr::distinct()

  dplyr::tibble(
    price = price_start,
    from = from,
    to = to,
    date = date
  ) %>%
    dplyr::left_join(rates_lookup, c("from", "to", "date")) %>%
    dplyr::mutate(output = price * rate) %>%
    dplyr::pull(output)
}










