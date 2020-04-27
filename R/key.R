#' Retrieve Index Key
#'
#' @description Retrieve the variable name(s) which make up and index key for a given object.
#'
#' @param x Object.
#' @param ... Other arguments (not used).
#'
#' @examples
#' x <- data.frame(year = 2010:2014, measurement = rnorm(5))
#' key(x) <- "year" # Assign key.
#' key(x) # Retrieve key.
#'
#' @export key
#' @export key.default
#'
#' @seealso \code{\link{is.key}}
#'
key <- function(x, ...) UseMethod("key")

#' @describeIn key Default key method.
key.default <- function(x, ...) return(attr(x, "key"))
