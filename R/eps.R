#' Machine precision
#'
#' @return The precision of floating point numbers.
#'
#' @examples
#' eps()
#'
#' @export eps
#'

eps <- function() .Machine$double.eps
