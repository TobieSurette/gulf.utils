#' Index Key Test
#'
#' @description Checks whether an index key exists or is valid.
#'
#' @param x Object.
#' @param key Character string specifying the fields consituting an index key for an object.
#' @param ... Other arguments (not used).
#'
#' @examples
#' # Build sample data:
#' x <- data.frame(year = 1990:2014, measurement = rpois(25))
#' key(x) <- "year" # Assign key.
#'
#  # Check index keys:
#' is.key(x, "measurement") # Generally FALSE
#' is.key(x, "year")
#' is.key(x)  # No need to specify 'year'.
#'
#' @export is.key
#' @export is.key.default
#' @export is.key.data.frame
#'
#' @seealso \code{\link{key}}
#'
is.key <- function(x, ...) UseMethod("is.key")

#' @describeIn is.key Default \code{is.key} function.
is.key.default <- function(x, key, ...) if ("key" %in% names(attributes)) return(all(key %in% attr(x, "key")))

#' @describeIn is.key Check whether key is a valid index key for a data frame.
is.key.data.frame <- function(x, key, ...){
   if (missing(key)) if ("key" %in% names(attributes)) key <- attr(x, "key")
   if (missing(key)) stop("Index 'key' is unspecified.")
   key <- as.character(key)
   if (!all(key %in% names(x))) stop("Some 'key' variables are not in 'x'.")
   return (!any(duplicated(x[key])))
}
