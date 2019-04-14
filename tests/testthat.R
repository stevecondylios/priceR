Sys.setenv("R_TESTS" = "") # Added based on suggestion here: https://github.com/r-lib/testthat/issues/86
library(testthat)
library(priceR)

test_check("priceR")
