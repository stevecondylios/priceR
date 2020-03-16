
#' Extract numeric salary from text data
#'
#' Extract numeric salary from text data. `extract_salary` automatically converts weekly and hourly rates to amounts per annum.
#' @name extract_salary
#'
#' @usage extract_salary(salary_text, exclude_below, exclude_above, salary_range_handling,
#' include_periodicity, hours_per_workday, days_per_workweek, working_weeks_per_year)
#'
#' @param salary_text A character string, or vector of character strings.
#' @param exclude_below A lower bound. Anything lower than this number will be replaced with NA.
#' @param exclude_above An upper bound. Anything above this number will be replaced with NA.
#' @param salary_range_handling A method of handling salary ranges. Defaults to returning an average of the range; can also be set to "max" or "min".
#' @param include_periodicity Set to TRUE to return an additional column stating the detected peridicity in the character string.
#' @param hours_per_workday Set assumed number of hours in the workday. Only affects annualisation of rates indentified as Daily. Default is 8 hours.
#' @param days_per_workweek Set assumed number of days per workweek. Only affects annualisation of rates indentified as Daily. Default is 5 days.
#' @param working_weeks_per_year Set assumed number of working weeks in the year. Only affects annualisation of rates indentified as Daily or Weekly. Default is 48 weeks.
#'
#' @import dplyr
#' @import stringr
#' @import gsubfn
#'
#' @export
#'
#' @examples
#'
#' # Provide a salary string and 'extract_salary' and will extract the salary and return it
#' extract_salary("$160,000 per annum")
#' # 160000
#'
#'
#' # If a range is present, the average will be taken by default
#' extract_salary("$160,000 - $180000.00 per annum")
#' # 170000
#'
#'
#' # Take the 'min' or 'max' of a salary range by setting salary_range_handling parameter accordingly
#' extract_salary("$160,000 - $180000.00 per annum", salary_range_handling = "min")
#' # 160000
#'
#'
#' # Extract salaries from character string(s)
#' annual_salaries <- c("$160,000 - $180000.00 per annum",
#'                      "$160000.00 - $180000.00 per annum",
#'                      "$145000 - $155000.00 per annum",
#'                      "$70000.00 - $90000 per annum",
#'                      "$70000.00 - $90000.00 per annum plus 15.4% super",
#'                      "$80000.00 per annum plus 15.4% super",
#'                      "60,000 - 80,000",
#'                      "$78,686 to $89,463 pa, plus 15.4% superannuation",
#'                      "80k - 100k")
#'
#' extract_salary(annual_salaries)
#' # 170000 170000 150000  80000  53338  40008  70000  56055  90000
#' # Note the fifth, sixth, and eighth elements are averages including '15' (undesirable)
#' # Using exclude_below parameter avoids this (see below)
#'
#' # Automatically detect, extract, and annualise daily rates
#' daily_rates <- c("$200 daily", "$400 - $600 per day", "Day rate negotiable dependent on experience")
#' extract_salary(daily_rates)
#' # 48000 120000     NA
#'
#'
#' # Automatically detect, extract, and annualise hourly rates
#' hourly_rates <- c("$80 - $100+ per hour", "APS6/EL1 hourly rate contract")
#' extract_salary(hourly_rates)
#' # 172800   6720
#' # Note 6720 is undesirable. Setting the exclude_below and exclude_above sensibly avoids this
#'
#'
#' salaries <- c(annual_salaries, daily_rates, hourly_rates)
#'
#'
#' # Setting lower and upper bounds provides a catch-all to remove unrealistic results
#' # Out of bounds values will be converted to NA
#' extract_salary(salaries, exclude_below = 20000, exclude_above = 600000)
#' # 170000 170000 150000  80000  80000  80000  70000  84074  90000  48000 120000     NA 172800     NA
#'
#'
#' # extract_salary automatically annualises hourly and daily rates
#' # It does so by making assumptions about the number of working weeks in a year,
#' # days per workweek, and hours per workday
#' # And the assumed number of hours per workday can be changed from the default (8)
#' # The assumed number of workdays per workweek can be changed from the default (5)
#' # The assumed number of working weeks in year can be changed from the default (48)
#' # E.g.
#' extract_salary(salaries, hours_per_workday = 7, days_per_workweek = 4,
#'                working_weeks_per_year = 46, exclude_below = 20000)
#' # 170000 170000 150000  80000  53338  40008  70000  56055  90000  36800  92000     NA 115920     NA
#'
#'
#' # To see which salaries were detected as hourly or weekly, set include_periodicity to TRUE
#' extract_salary(salaries, include_periodicity = TRUE, exclude_below = 20000)
#'
#' # salary periodicity
#' # 1  170000      Annual
#' # 2  170000      Annual
#' # 3  150000      Annual
#' # 4   80000      Annual
#' # 5   80000      Annual
#' # 6   80000      Annual
#' # 7   70000      Annual
#' # 8   84074      Annual
#' # 9   90000      Annual
#' # 10  48000       Daily
#' # 11 120000       Daily
#' # 12     NA       Daily
#' # 13 172800      Hourly
#' # 14     NA      Hourly
#'
#'
library(dplyr)
library(stringr)
library(gsubfn)

extract_salary <- function(salary_text, exclude_below, exclude_above, salary_range_handling, include_periodicity, hours_per_workday, days_per_workweek, working_weeks_per_year) {

  if(missing(exclude_above)) { exclude_above <- 9999999999999 }
  if(missing(exclude_below)) { exclude_below <- 0 }
  if(missing(include_periodicity)) { include_periodicity <- FALSE }

  if(missing(hours_per_workday)) { hours_per_workday <- 8 }
  if(missing(days_per_workweek)) { days_per_workweek <- 5 }
  if(missing(working_weeks_per_year)) { working_weeks_per_year <- 48 }

  if(missing(salary_range_handling)) { salary_range_handling <- "average" }
  if(!salary_range_handling %in% c("average", "max", "min")) { stop("salary_range_handling parameter must be either \"average\", \"min\", or \"max\"") }

  # Clean
  salary <- salary_text %>% gsub(",", "", .) %>% str_replace_all(., "\\d+\\.\\d+", function(x) { as.integer(x) }) %>%
    gsub("(\\d+)k", "\\1000", .)  %>% gsub("(\\d+)K", "\\1000", .) # Sub out commas, round decimals, and convert k to 000


  # Identify hourly, daily or annual
  hourly_daily_or_annual <- function(salary) {

    hourly_terms <- c("ph", "p\\.h", "p\\.h\\.", "p/h", "hourly", "hour")
    daily_terms <- c("pd", "p\\.d", "p\\.d\\.", "p/d", "daily", "day")

    period <- salary %>% { ifelse((grepl(paste(daily_terms, collapse="|"), . , ignore.case=TRUE)), "Daily",
                                  ifelse(grepl(paste(hourly_terms, collapse="|"), . , ignore.case=TRUE), "Hourly",
                                         "Annual")) }
    temp <- data.frame(salary, period, stringsAsFactors = FALSE)
    return(temp)
  }
  temp <- hourly_daily_or_annual(salary)


  # For every number regex'd out, these will convert into annual
  hourly_to_annual <- function(hourly_pay) {
    hourly_pay %>% as.numeric %>% {. * hours_per_workday * days_per_workweek * working_weeks_per_year} %>%
      round %>% { ifelse(. > exclude_above, "", .) } %>% { ifelse(. < exclude_below, "", .) }
  }

  salary[temp$period == "Hourly"] <- salary[temp$period == "Hourly"] %>% gsubfn("(\\d+)", ~ { hourly_to_annual(x) }, . , backref = -1) %>% unlist

  daily_to_annual <- function(daily_pay) {
    daily_pay %>% as.numeric %>% {. * days_per_workweek * working_weeks_per_year} %>%
      round %>% { ifelse(. > exclude_above, "", .) } %>% { ifelse(. < exclude_below, "", .) }
  }

  salary[temp$period == "Daily"] <- salary[temp$period == "Daily"]  %>% gsubfn("(\\d+)", ~ { daily_to_annual(x) }, . , backref = -1) %>% unlist


  # Define 3 functions
  extract_average <- function(salary) {
    salary %>% str_extract_all(. , "\\d+") %>% lapply(., function(x) { x[as.numeric(x) >= exclude_below] } ) %>%
      lapply(., function(x) { x[as.numeric(x) <= exclude_above] } ) %>% lapply(., function(x) { mean(as.numeric(x)) } ) %>% unlist %>% {ifelse(. == -Inf, NA_character_, .) }
  }

  extract_max <- function(salary) {
    salary %>% str_extract_all(. , "\\d+") %>% lapply(., function(x) { x[as.numeric(x) >= exclude_below] } ) %>%
      lapply(., function(x) { x[as.numeric(x) <= exclude_above] } )%>% lapply(., function(x) { max(as.numeric(x)) } ) %>% unlist %>% {ifelse(. == -Inf, NA_character_, .) }
  }

  extract_min <- function(salary) {
    salary %>% str_extract_all(. , "\\d+") %>% lapply(., function(x) { x[as.numeric(x) >= exclude_below] } ) %>%
      lapply(., function(x) { x[as.numeric(x) <= exclude_above] } ) %>% lapply(., function(x) { min(as.numeric(x)) } ) %>% unlist %>% {ifelse(. == -Inf, NA_character_, .) }
  }


  switch(salary_range_handling,
         "average"=suppressWarnings({salary <- salary %>% extract_average %>% as.numeric}),
         "min"=suppressWarnings({salary <- salary %>% extract_min %>% as.numeric}),
         "max"=suppressWarnings({salary <- salary %>% extract_max %>% as.numeric})
  )

  salary <- salary %>% { ifelse(. > exclude_above, "", .) } %>% as.numeric(.) %>% { ifelse(. < exclude_below, "", .) } %>% as.numeric(.) %>% round

  annualised_salaries <- data.frame(salary, stringsAsFactors = FALSE)

  if(include_periodicity) {
    annualised_salaries <- data.frame(annualised_salaries, temp$period, stringsAsFactors = FALSE)
    colnames(annualised_salaries)[2] <- "periodicity"
  }


  return(annualised_salaries)
}


