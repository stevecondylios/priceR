% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/currencies.R
\name{retrieve_historical_rates}
\alias{retrieve_historical_rates}
\title{Retrieve historical exchange rates}
\usage{
retrieve_historical_rates(from, to, start_date, end_date)
}
\arguments{
\item{from}{A currency code (see currencies() for supported codes)}

\item{to}{A currency code}

\item{start_date}{A start date (of the form "2010-01-01")}

\item{end_date}{An end date}
}
\value{
A data.frame containing exchange rate data for select currency pair
}
\description{
Retrieves historical exchange rates between a currency pair - retrieves max. 365 days' data
}
\examples{
\dontrun{
# Note date range >365 days', yet only 365 days' returned.
# Use historical_exchange_rates() for > 365 days'.
 priceR:::retrieve_historical_rates("USD", to = "AUD",
                                    start_date = "2018-01-01",
                                    end_date = "2020-06-30")
}


}
