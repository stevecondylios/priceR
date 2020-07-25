
## NOTE: THIS FILE IS IN .Rbuildignore (and hence must be removed before it takes effect)


currencies <- function() {

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

    if(getOption("forex_warning", TRUE)) {
    # This message appears on first use only of this/similar functions
    message(paste(
            "For full currency exchange rate API documentation visit:\n",
            "https://exchangerate.host/#/#docs\n",
            "(this message will only appear once per session)"))
    options("forex_warning"=FALSE)
  }

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









fromJSON("https://api.exchangerate.host/timeseries?start_date=2020-01-01&end_date=2020-01-04&base=USD")

fromJSON("https://api.exchangerate.host/timeseries?start_date=2020-01-01&end_date=2020-01-04")


a <- fromJSON("https://api.exchangerate.host/timeseries?start_date=2020-01-01&end_date=2020-01-04&base=USD")












