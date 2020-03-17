




# https://stackoverflow.com/questions/12590180/inflation-adjusted-prices-package
set.seed(12)
prices <- rnorm(200, mean=10, sd=3)
years <- round(rnorm(200, mean=2006, sd=5))
df <- data.frame(prices, years)


price <- df$prices
from_date <- df$years

library(jsonlite)


price <- df$prices[1]
from_date <- df$years[1]
to_date <- 2008
country <- "Australia"

adjust_for_inflation(price, from_date, country="Australia", 2008)

adjust_for_inflation(df$prices, df$years, country="US", 2008)

countries_dataframe <- show_countries()
inflation_dataframe <- retrieve_inflation_data(country, countries_dataframe)
adjust_for_inflation(price, from_date, country="AU", 2008,
                     countries_dataframe = countries_dataframe, inflation_dataframe = inflation_dataframe)


inflation_dataframe[[2]][inflation_dataframe[[2]]$date >= df$years[1] & inflation_dataframe[[2]]$date <= 2008, ]

# Right way
inflation_dataframe[[2]][inflation_dataframe[[2]]$date >= df$years[1] & inflation_dataframe[[2]]$date <= 2008, ]$value %>%
  .[1:(length(.) -1)] %>% as.numeric() %>% {. / 100 } %>% {. + 1} %>% prod %>% {. * price}

# Wrong way
inflation_dataframe[[2]][inflation_dataframe[[2]]$date >= df$years[1] & inflation_dataframe[[2]]$date <= 2008, ]$value %>%
  .[2:(length(.))] %>% as.numeric() %>% {. / 100 } %>% {. + 1} %>% prod %>% {. * price}





# Logic: following standard economic convention
# A price will not be adjusted for the inflation that occurred in the period from which the price is from
# E.g. a price in 2008 *will not* have inflation that occurred during 2008 applied to it
# A price *will* have the inflation applied from its end period
# E.g. a price from 2008 converted to 2010 will have 2009 inflation and 2010 inflation applied to it







