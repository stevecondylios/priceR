#' Round prices to the nearest specified increment
#'
#' @name round_to_nearest
#'
#' @usage round_to_nearest(amount, to_nearest)
#'
#' @param amount Price to be rounded
#' @param to_nearest Increment to which price is to be rounded to
#'
#'
#' @export
#'
#' @examples
#'
#' # Round to nearest 0.05 (5c)
#' library(dplyr)
#' prices <- c(4.45, 5.22, 0.16, 27.88, 112.19)
#' prices %>% round_to_nearest(0.05)
#'
#'
#' # Round to nearest $10
#' prices <- c(4.45, 5.22, 0.16, 27.88, 112.19)
#' prices %>% round_to_nearest(10)
#'
#'

round_to_nearest <- function(amount, to_nearest) {
  round(amount/to_nearest) * to_nearest
}



#' Round prices up to the nearest specified increment
#'
#' @name round_up_to_nearest
#'
#' @usage round_up_to_nearest(amount, to_nearest)
#'
#' @param amount Price to be rounded
#' @param to_nearest Increment to which price is to be rounded up to
#'
#'
#' @export
#'
#' @examples
#'
#' # Round up to nearest 0.05 (5c)
#' library(dplyr)
#' prices <- c(4.45, 5.22, 0.16, 27.88, 112.19)
#' prices %>% round_up_to_nearest(0.05)
#'
#'
#' # Round up to nearest $10
#' prices <- c(4.45, 5.22, 0.16, 27.88, 112.19)
#' prices %>% round_up_to_nearest(10)


round_up_to_nearest <- function(amount, to_nearest) {
  ceiling(amount/to_nearest) * to_nearest
}




#' Round prices down to the nearest specified increment
#'
#' @name round_down_to_nearest
#'
#' @usage round_down_to_nearest(amount, to_nearest)
#'
#' @param amount Price to be rounded
#' @param to_nearest Increment to which price is to be rounded down to
#'
#'
#' @export
#'
#' @examples
#'
#' # Round down to nearest 0.05 (5c)
#' library(dplyr)
#' prices <- c(4.45, 5.22, 0.16, 27.88, 112.19)
#' prices %>% round_down_to_nearest(0.05)
#'
#'
#' # Round down to nearest $10
#' prices <- c(4.45, 5.22, 0.16, 27.88, 112.19)
#' prices %>% round_down_to_nearest(10)


round_down_to_nearest <- function(amount, to_nearest) {
  floor(amount/to_nearest) * to_nearest
}































