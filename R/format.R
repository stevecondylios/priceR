#' Make numeric currency values human readable
#'
#' @name format_currency
#'
#' @usage format_currency(amount, symbol, digits)
#'
#' @param amount Price or vector of prices (character, numeric, or integer)
#' @param symbol Symbol to prepend to amount (e.g. $) see: currency_characters()
#' @param digits The number of decimal places. Set equal to 2 to include cents (defaults to 0 i.e. whole major currency units)
#'
#' @return A character vector
#'
#' @import dplyr
#' @import stringr
#'
#' @export
#'
#' @examples
#'
#' format_currency("2423562534234", "$")
#' # "$2,423,562,534,234"
#'
#' format_currency("2423562534234.876", "$", 0)
#' # "$2,423,562,534,234.88"
#'
#' format_currency("2423562534234.876", "$", 2)
#' # "$2,423,562,534,234.88"
#'
#' format_currency("2423562534234", "¥", 2)
#' # "¥2,423,562,534,234.00"
#'
#' # format_currency() is vectorized and can accept vector arguments
#' format_currency(c("2423562534234", "20"), c("¥", "$"), c(1, 2))
#' # "¥2,423,562,534,234.0" "$20.0"
#'


format_currency <- function(amount, symbol, digits) {

  if(missing(digits)) { digits <- 0 }

  # From: https://stackoverflow.com/questions/14836754/is-there-an-r-function-to-escape-a-string-for-regex-characters
  symbol_regex <- gsub("([.|()\\^{}+$*?]|\\[|\\])", "\\\\\\1", symbol)

  amount %>%
    gsub(",", "", .) %>%
    as.numeric %>%
    round(., digits) %>%
    format(big.mark = ",", digits = digits, scientific = FALSE, nsmall = digits) %>%
    trimws %>%
    paste0(symbol, .)

}








#' Make numeric currency values human readable
#'
#' @name format_dollars
#'
#' @usage format_dollars(amount, digits)
#'
#' @param amount Price or vector of prices (character, numeric, or integer)
#' @param digits The number of decimal places. Set equal to 2 to include cents (defaults to 0 i.e. whole dollars)
#'
#' @return A character vector
#'
#' @import dplyr
#' @export
#'
#' @examples
#'
#' format_dollars("2423562534234")
#' # "$2,423,562,534,234"
#'
#' format_dollars("2423562534234.876", 0)
#' # "$2,423,562,534,234"
#'
#' format_dollars("2423562534234.876", 2)
#' # "$2,423,562,534,234.88"
#'
#' format_dollars("2423562534234", 2)
#' # "$2,423,562,534,234.00"

format_dollars <- function(amount, digits) {

  format_currency(amount, "$", digits)

}











#' Provide currency characters
#'
#' @name currency_characters
#'
#' @usage currency_characters()
#'
#' @return A character vector of currency symbols
#'
#' @import stringi
#' @export
#'
#' @examples
#'
#' currency_characters()
#'


currency_characters <- function() {

  c("\\u0024", "\\u00a4", "\\u00a3", "\\u20ac", "\\u00a2", "\\u00a5", "\\u20a7",
    "\\u0192") %>% stringi::stri_unescape_unicode()

}








#' Convert human readable currencies into numeric data
#'
#' @name currency_to_numeric
#'
#' @usage currency_to_numeric(currency_text)
#'
#' @param currency_text Price or vector of prices
#'
#' @return A numeric vector
#'
#' @import dplyr
#' @export
#'
#' @examples
#' library(dplyr)
#' c("$134,345.05", "£22", "¥30000") %>% currency_to_numeric()
#' # [1] 134345     22  30000


currency_to_numeric <- function(currency_text) {

  currency_symbols <- currency_characters()

  currency_symbols_regex <- currency_symbols %>% gsub("\\$", "\\\\$", .)

  currency_text %>%
    { gsub(paste0(currency_symbols_regex, collapse="|"), "", .) } %>%
    gsub(",", "", .) %>%
    as.numeric

}











