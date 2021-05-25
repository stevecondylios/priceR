context("Formatting functions work as expected")

# devtools::test()



#----- format_currency -----#

# test_that("format_currency() correctly inserts commas and symbol to integer value", {
#
#   expect_equal(format_currency("2423562534234", "$"), "$2,423,562,534,234")
#
# })
#
#
#
# test_that("format_currency() correctly inserts commas and symbol to decimal value and
#           rounds to zero decimal places", {
#
#   expect_equal(format_currency("2423562534234.876", "$", 0), "$2,423,562,534,235")
#
# })
#
#
# test_that("format_currency() correctly inserts commas and symbol other than dollar sign", {
#
#   expect_equal(format_currency("2423562534234", "¥", 2), "¥2,423,562,534,234.00")
#
# })
#
#

# test_that("format_currency() correctly handles vector arguments", {
#
#   expect_equal(format_currency(c("2423562534234", "20"), c("¥", "$"), c(1, 2)),
#                c("¥2,423,562,534,234.0", "$20.0"))
#
# })










#----- format_dollars -----#

# test_that("format_dollars() correctly inserts commas and symbol to integer value", {
#
#   expect_equal(format_dollars("2423562534234"), "$2,423,562,534,234")
#
# })
#
#
# test_that("format_dollars() correctly inserts commas and symbol to decimal value and
#           rounds to zero decimal places", {
#
#             expect_equal(format_dollars("2423562534234.876", 0), "$2,423,562,534,235")
#
#           })
#
#
# test_that("format_dollars() correctly inserts commas and symbol and rounds to 2dp", {
#
#   expect_equal(format_dollars("2423562534234.876", 2), "$2,423,562,534,234.88")
#
# })
#
#
#
# test_that("format_dollars() correctly inserts commas and symbol and rounds to 2dp when those
#           2dp are both zeros", {
#
#   expect_equal(format_dollars("2423562534234", 2), "$2,423,562,534,234.00")
#
# })
#











