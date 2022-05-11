#' @title Match Data Frame Rows
#'
#' @description Returns the row indices of a target data frame which match those of a source data frame.
#'
#' @param x Source data frame whose elements will be searched for in the target data frame \code{y}.
#' @param y Target data frame.
#' @param by Column name(s) which will be used for matching elements.
#'           The default is the common column names for \code{x} and \code{y}.
#' @param key Character vector defining index key.
#' @param ... Further arguments passed onto \code{\link[base]{match}}.
#'
#' @examples
#' # Create a simple data frame:
#' x <- data.frame(
#'         u = c(2, 2, 4),
#'         v = c(1, 2, 3))
#'
#' y <- data.frame(
#'         u = c(1, 1, 1, 1, 2, 2, 2, 3),
#'         v = c(1, 2, 3, 4, 1, 2, 3, 3))
#'
#' # Get the list of indices matching the rows of 'x' to 'y':
#' index <- match(x, y)
#'

#' @export match
match <- function(x, ...) UseMethod("match")

#' @describeIn match Default match method, see \code{\link[base]{match}}.
#' @export
match.default <- function(x, ...) return(base::match(x, ...))

#' @describeIn match Match rows between two data frames.
#' @export
match.data.frame <- function(x, y, by, key, ...){
   # Check 'by' argument:
   if (missing(by) & !missing(key)) by <- key
   if (missing(by)){
      if (!is.null(attr(y, "key"))) by <- attr(y, "key")
      if (missing(by)) by <- base::intersect(names(x), names(y))
   }
   if (missing(by)) stop("Index key variable(s) not defined.")
   if (!is.character(by)) stop("Index key must be a character vector.")
   by <- by[(by %in% names(x)) & (by %in% names(y))]
   if (length(by) == 0) stop("There must be at least one common variable for the match to be performed.")

   # Convert fields to character vectors:
   fun <- function(x){
      x <- as.character(x)
      x[is.na(x)] <- " "
      return(x)
   }
   x[by] <- base::sapply(x[by], fun)
   y[by] <- base::sapply(y[by], fun)

   # Check that the index key is unique in target object 'y':
   if (any(base::duplicated(y[by]))) stop ("Target object 'y' index key is not unique.")

   # Find indices:
   xx <- apply(x[ ,by , drop = FALSE], 1, function(x) paste0(x, collapse = "-t-"))
   yy <- apply(y[ ,by , drop = FALSE], 1, function(x) paste0(x, collapse = "-t-"))

   return(base::match(xx, yy, ...))
}
