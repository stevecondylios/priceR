#' priceR: Regular Expressions for Prices and Currencies
#' @docType package
#' @description
#' Applying regular expressions to text data to extract prices and currencies in numeric class, allowing for faster and easier analysis
#'
#' @details
#' It has the goal of providing a quick and practical way of extracting numeric price and currency data from text fields, thus allowing faster and easier analysis of ostensibly numeric data.
#'
#'
#' @author Steve Condylios \email{steve.condylios@gmail.com}
#'
#'
#'
#' @name priceR
#'

# From jennybc's comment here: https://github.com/STAT545-UBC/Discussion/issues/451
# quiets concerns of R CMD check re: the .'s that appear in pipelines
if(getRversion() >= "2.15.1")  utils::globalVariables(c("."))


