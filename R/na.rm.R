#' @title Remove NA Values
#'
#' @description Remove NA values from an object.
#'
#' @param x R object.
#'
#' @examples
#' na.rm(c(NA, 1, 2, 3, NA, 4))
#'

#' @export na.rm
na.rm <- function(x) return(x[!is.na(x)])



