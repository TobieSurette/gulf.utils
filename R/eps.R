#' Machine precision
#'
#' @description Returns the machine precision of floating point numbers.
#'
#' @return The precision of floating point numbers.
#'
#' @examples
#' eps()
#'
#' @export eps
#'

eps <- function() .Machine$double.eps
