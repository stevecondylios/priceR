context("Outputs (or errors/warnings) are generated as expected")
library(jsonlite)
library(dplyr)
library(lubridate)
library(purrr)
# options(digits = 22)

# Assign these variables once
country <- "AU"
inflation_dataframe <- retrieve_inflation_data(country)
countries_dataframe <- show_countries()



#----- URL conversion -----#

test_that("url_all_results() correctly converts URL to JSON", {
  original_url <- "https://api.worldbank.org/v2/country" # Note: no ?format=json on url
  expect_gt(url_all_results(original_url) %>% fromJSON %>% .[[2]] %>% nrow, 50)
})



#----- Retieving all results (not just first page) from WB API -----#

test_that("The number of total results is retreived (and not just 50 on first page)", {
  original_url <- "https://api.worldbank.org/v2/country?format=json"
  total_results <- original_url %>% fromJSON %>% .[[1]] %>% .$total
  expect_gt(total_results, 50)

})

test_that("url_all_results() correctly retreives WB API data", {
  original_url <- "https://api.worldbank.org/v2/country?format=json"
  expect_gt(url_all_results(original_url) %>% fromJSON %>% .[[2]] %>% nrow, 50)
})



#----- Retieving countries from WB API -----#

test_that("show_countries() retrieves countries as expected", {
  expect_gt(show_countries() %>% nrow, 50)
})



#----- Identification of iso2Code/country_name -----#

countries_dataframe <- show_countries()

test_that("show_countries() retrieves countries as expected", {
  expect_equal(country_input_type("AU", countries_dataframe), "iso2Code")
  expect_equal(country_input_type("Australia", countries_dataframe), "country_name")
  expect_equal(country_input_type("AustraTYPOlia", countries_dataframe), "invalid")
})



#----- Identification of iso2Code/country_name -----#

test_that("iso2Code is identified as such and kept as iso2Code", {
  country <- "AU"
  country_input_type_string <- country_input_type(country, countries_dataframe)
  convert_to_iso2Code(country_input_type_string, country) %>% expect_equal("AU")
})

test_that("country_name is identified as such and converted to iso2Code", {
  country <- "Australia"
  country_input_type_string <- country_input_type(country, countries_dataframe)
  convert_to_iso2Code(country_input_type_string, country) %>% expect_equal("AU")
})

test_that("invalid country_name/iso2Code is identified as such", {
  country <- "AustrTESTalia"
  country_input_type_string <- country_input_type(country, countries_dataframe)
  expect_error(convert_to_iso2Code(country_input_type_string, country),
               paste0("'", country, "'", " is not a valid country input - select a valid country from show_countries()"))
})


#----- Testing retrieval of of inflation data using retrieve_inflation_data() -----#


test_that("Inflation data is retrieved as expected for iso2Code input", {
  country <- "AU"
  expect_gt(retrieve_inflation_data(country) %>% .[[2]] %>% nrow, 50)
})


test_that("Inflation data is retrieved as expected for country_name input", {
  country <- "Australia"
  expect_gt(retrieve_inflation_data(country) %>% .[[2]] %>% nrow, 50)
})

test_that("Retrieval of inflation data for an invalid input fails with appropriate error message", {
  country <- "AustraTESTlia"
  expect_error(retrieve_inflation_data(country) %>% .[[2]] %>% nrow,
               paste0("'", country, "'", " is not a valid country input - select a valid country from show_countries()"))
})



###########################################
##### Testing adjust_for_inflation() ######
###########################################

country <- "Australia"
price <- 10
from_date <- today() - (365 * 28)


test_that("One price, one from date, one to date", {
  adjust_for_inflation(price, from_date, country, to_date = 2017,
                       inflation_dataframe = inflation_dataframe,
                       countries_dataframe = countries_dataframe) %>%
    expect_equal(18.98172323759792590181)

})

test_that("One price, one from date, one to date, extrapolating using 3 year average", {
  adjust_for_inflation(price, from_date, country, to_date = 2019,
                       inflation_dataframe = inflation_dataframe, countries_dataframe = countries_dataframe,
                       extrapolate_future_method = "average", future_averaging_period = 3) %>%
    expect_equal(19.65697883048040139897)

})

test_that("One price, one from date, one to date", {
  adjust_for_inflation(price, from_date, country, to_date = 2019,
                       inflation_dataframe = inflation_dataframe, countries_dataframe = countries_dataframe,
                       extrapolate_future_method = "average", future_averaging_period = 3) %>%
    expect_equal(19.65697883048040139897)

})





# NOT IN AUTOMATED TESTING
# Omitting to_date (assumes current)
# adjust_for_inflation(price, from_date, country,
#                      inflation_dataframe = inflation_dataframe, countries_dataframe = countries_dataframe,
#                      extrapolate_future_method = "average", future_averaging_period = 3)


available_inflation_data <- inflation_dataframe %>% .[[2]] %>% na.omit
to_date <- 2030

test_that("adjust_for_inflation() errors with a useful message when to_date is beyond available data", {

  expect_error(
    # Function
    adjust_for_inflation(price, from_date, country, to_date = to_date,
                         inflation_dataframe = inflation_dataframe, countries_dataframe = countries_dataframe)
    ,
    # Error message
    paste0("'to_date' (", to_date, ") is/contains a later date than the latest available data (",
           max(available_inflation_data$date), ").\nTry setting 'extrapolate_future' to TRUE or using an earlier 'to_date'"),

    # Avoid regex: https://github.com/r-lib/testthat/issues/726
    fixed = TRUE
  )
}
)


available_inflation_data <- inflation_dataframe %>% .[[2]] %>% na.omit
to_date <- 2030
extrapolate_future_method = "average"

test_that("adjust_for_inflation() prompts user for number of years to use in extrapolating using average (future)", {

  expect_error(
    # Function
    adjust_for_inflation(price, from_date, country, to_date = to_date,
                         inflation_dataframe = inflation_dataframe, countries_dataframe = countries_dataframe,
                         extrapolate_future_method = "average")
    ,
    # Error message
    paste0("Please specify how many years' average to use when extrapolating forward ('future_averaging_period'
           can take any positive integer or 'all' to use an average of all available years' data"),

    # Avoid regex: https://github.com/r-lib/testthat/issues/726
    fixed = TRUE
  )
}
)



# NOT IN AUTOMATED TESTING
# adjust_for_inflation(price, from_date, country, to_date = 2030,
#                      inflation_dataframe = inflation_dataframe, countries_dataframe = countries_dataframe,
#                      extrapolate_future_method = "average", future_averaging_period = 3)



available_inflation_data <- inflation_dataframe %>% .[[2]] %>% na.omit
to_date <- 2030
extrapolate_future_method = "average"
future_averaging_period <- 3

test_that("Expect a future_rate if rate extrapolation is used (future)", {

  expect_error(
    # Function
    adjust_for_inflation(price, from_date, country, to_date = to_date,
                         inflation_dataframe = inflation_dataframe, countries_dataframe = countries_dataframe,
                         extrapolate_future_method = "rate", future_averaging_period = future_averaging_period)
    ,
    # Error message
    paste0("Please specify the assumed rate of inflation for future periods (i.e. using 'future_rate' parameter)"),

    # Avoid regex: https://github.com/r-lib/testthat/issues/726
    fixed = TRUE
  )
}
)


# NOT IN AUTOMATED TESTING
# adjust_for_inflation(price, from_date, country, to_date = 2030,
#                      inflation_dataframe = inflation_dataframe, countries_dataframe = countries_dataframe,
#                      extrapolate_future_method = "rate", future_rate = 3)
# [1] 27.59076877199724009415



#----- Testing past extrapolation -----#

# Testing the past extrapolations (since past data shouldn't change and responses should be consistent)




test_that("past extrapolation works when based on the last three available periods",
          {adjust_for_inflation(price, from_date, country, to_date = 1930,
                                inflation_dataframe = inflation_dataframe, countries_dataframe = countries_dataframe,
                                extrapolate_past_method = "average", past_averaging_period = 3) %>%
              expect_equal(0.7210519871541605230192)
            }
          )


test_that("past extrapolation works with a rate",
          {adjust_for_inflation(price, from_date = 1990, country, to_date = 1930,
                                inflation_dataframe = inflation_dataframe, countries_dataframe = countries_dataframe,
                                extrapolate_past_method = "rate", past_rate = 2.5) %>%
              expect_equal(0.6273066554899743296758)
          }
)



test_that("returns a useful message when past extrapolation is not required",
          {expect_warning(adjust_for_inflation(price, from_date = "1991-04-14", country, to_date = 1961,
                                inflation_dataframe = inflation_dataframe, countries_dataframe = countries_dataframe,
                                extrapolate_past_method = "rate", past_rate = 2.5),
              "Past extrapolation not required as data available for AU back to 1960")
          }
)




test_that("extrapolation works with longer form date input",
          {adjust_for_inflation(price, from_date = "1991-04-14", country, to_date = 1930,
                                inflation_dataframe = inflation_dataframe, countries_dataframe = countries_dataframe,
                                extrapolate_past_method = "rate", past_rate = 2.5) %>%
              expect_equal(0.6079927011033156025732)
          }
)


test_that("extrapolation works with longer form date input",
          {adjust_for_inflation(price, from_date = "2009-04-09 13:09:39 AEST", country, to_date = 1930,
                                inflation_dataframe = inflation_dataframe, countries_dataframe = countries_dataframe,
                                extrapolate_past_method = "rate", past_rate = 2.5) %>%
              expect_equal(0.3859573478757593045785)
          }
)




##################################
##### Testing vectorisation ######
##################################


country <- "AU"
price <- c(10, 11, 12, 15)
from_date <- Sys.Date()-(365 * 10)


test_that("adjust_for_inflation() can handle a vector of price inputs",
          {adjust_for_inflation(price = price, from_date = from_date, to_date = 2017, country = country,
                                inflation_dataframe = inflation_dataframe, countries_dataframe = countries_dataframe) %>%
              expect_equal(c(11.88555858310627044716, 13.07411444141689749188, 14.26267029972752453659, 17.82833787465940389438))
          }
)





country <- "AU"
price <- c(10, 10, 10, 10)
from_date <- c(Sys.Date()-(365 * 10), Sys.Date()-(365 * 6), Sys.Date()-(365 * 4), Sys.Date()-(365 * 7))

test_that("adjust_for_inflation() can handle a vector of price inputs and a vector of from dates",
          {adjust_for_inflation(price = price, from_date = from_date, to_date = 2017, country = country,
                                inflation_dataframe = inflation_dataframe, countries_dataframe = countries_dataframe) %>%
              expect_equal(c(11.88555858310627044716, 10.79435783221975242441, 10.28046193730850887960, 10.98463863006799279276))
          }
)





country <- "AU"
price <- c(10, 10, 10, 10)
from_date <- c(2009, 2013, 2015, 2012)
to_date <- c(1997, 2008, 2007, 2017)

test_that("adjust_for_inflation() can handle a vector of price inputs, from dates and to dates",
          {adjust_for_inflation(price = price, from_date = from_date, to_date = to_date, country = country,
                                inflation_dataframe = inflation_dataframe, countries_dataframe = countries_dataframe) %>%
              expect_equal(c(7.159303882195446000480,  8.864734299516909388217,  8.165776642674712704206, 10.984638630067992792760))
          }
)


from_date <- c(2009, 2013, 2015, 2012)
to_date <- c(1997, 2008, 2007, 2017)
test_that("vector inputs return same results as element inputs done separately",
          {
            first <- adjust_for_inflation(price = price[1], from_date = from_date[1], to_date = to_date[1], country = country,
                                          inflation_dataframe = inflation_dataframe, countries_dataframe = countries_dataframe)

            second <- adjust_for_inflation(price = price[2], from_date = from_date[2], to_date = to_date[2], country = country,
                                          inflation_dataframe = inflation_dataframe, countries_dataframe = countries_dataframe)

            third <- adjust_for_inflation(price = price[3], from_date = from_date[3], to_date = to_date[3], country = country,
                                          inflation_dataframe = inflation_dataframe, countries_dataframe = countries_dataframe)

            fourth <- adjust_for_inflation(price = price[4], from_date = from_date[4], to_date = to_date[4], country = country,
                                          inflation_dataframe = inflation_dataframe, countries_dataframe = countries_dataframe)


            adjust_for_inflation(price = price, from_date = from_date, to_date = to_date, country = country,
                                inflation_dataframe = inflation_dataframe, countries_dataframe = countries_dataframe) %>%
              expect_equal(c(first, second, third, fourth))
          }
)





