context("Outputs (or errors/warnings) are generated as expected")
library(jsonlite)
library(dplyr)
library(lubridate)





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

test_that("country_name is identified as such and kept as iso2Code", {
  country <- "Australia"
  country_input_type_string <- country_input_type(country, countries_dataframe)
  convert_to_iso2Code(country_input_type_string, country) %>% expect_equal("AU")
})

test_that("iso2Code is identified as such and kept as iso2Code", {
  country <- "AustrTESTalia"
  country_input_type_string <- country_input_type(country, countries_dataframe)
  expect_error(convert_to_iso2Code(country_input_type_string, country), paste0("'", country, "'", " is not a valid country input - select a valid country from show_countries()"))
})























price <- 10
country <- "AU"
from_date <- today() - (365 * 28)










