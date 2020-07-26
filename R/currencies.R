
## NOTE: THIS FILE IS IN .Rbuildignore (and hence must be removed before it takes effect)





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



latest_exchange_rates <- function(symbol = "USD") {

  display_api_info()

  dat <- fromJSON(paste0("https://api.exchangerate.host/latest?base=", symbol))

  cat("Daily", symbol, "exchange rate as at end of day", dat$date, "GMT", "\n")

  # "EOD / End of Day historical exchange rates, which become available at
  # 00:05am GMT for the previous day and are time stamped at one second before midnight."

  options(scipen=999)
  options(digits=1)

  col_name <- paste0("one_", tolower(symbol), "_is_equivalent_to")

  dat$rates %>%
    map_dbl(~ .x[1]) %>%
    stack %>%
    .[,c(2,1)] %>%
    `colnames<-`(c("currency", col_name))

}

latest_exchange_rates()
latest_exchange_rates("AUD")






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


# Simple test
start_date = "2010-01-01"
end_date = "2020-06-30"
n_days = 365
make_dates(start_date, end_date, n_days)

# With lots of periods
start_date = "2010-01-01"
end_date = "2020-06-30"
n_days = 20
make_dates(start_date, end_date, n_days)

# Less than one period
start_date = "2020-01-01"
end_date = "2020-06-30"
n_days = 365
make_dates(start_date, end_date, n_days)












historical_exchange_rates <- function(from, to, start_date, end_date) {

  display_api_info()

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

historical_exchange_rates("USD", to = "AUD",
                          start_date = "2020-01-01", end_date = "2020-06-30")


historical_exchange_rates("AUD", to = "USD",
                          start_date = "2010-01-01", end_date = "2020-06-30")


start_date
end_date



fromJSON("https://api.exchangerate.host/timeseries?start_date=2020-01-01&end_date=2020-01-04&base=USD")

fromJSON("https://api.exchangerate.host/timeseries?start_date=2020-01-01&end_date=2020-01-04")


a <- fromJSON("https://api.exchangerate.host/timeseries?start_date=2020-01-01&end_date=2020-01-04&base=USD")












