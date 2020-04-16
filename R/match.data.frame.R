#' Match Data Frame Rows
#'
#' @description Returns the row indices of a target data frame which match those of a source data frame.
#'
#' @param x Source data frame whose elements will be searched for in the target data frame \code{y}.
#' @param y Target data frame.
#' @param by Column name(s) which will be used for matching elements. The default is the common column names for \code{x} and \code{y}.
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
#' # Get the list of indices matching the rows "u" of 'x' to 'y':
#' index <- match(x, y, by = "u")
#'
#' @export match
#' @export match.default
#' @export match.data.frame
match <- function(x, ...) UseMethod("match")

#' @describeIn match Default match method, see \code{\link[base]{match}}.
match.default <- function(x, ...) return(base::match(x, ...))

#' @describeIn match Match rows between two data frames.
match.data.frame <- function(x, y, by = base::intersect(names(x), names(y)), ...){
   # Check 'by' argument:
   if (length(by) == 0) stop("There must be at least one common variable for the match to be performed.")
   if (!all(by %in% names(x))) stop("Some 'by' variables are not in source object 'x'.")
   if (!all(by %in% names(y))) stop("Some 'by' variables are not in target object 'y'.")

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

   # Find the indices:
   if (length(by) == 1){
      # Use usual vector-based 'match' function:
      index <- base::match(x[, by], y[, by])
   }else{
      # Merge index column from 'y' into 'x':
      index <- base::match(apply(x[by], 1, function(x) paste0(x, collapse = "-t-")),
                           apply(y[by], 1, function(x) paste0(x, collapse = "-t-")))
   }

   return(index)
}

