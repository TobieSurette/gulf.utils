#' @title Sort Data Frame
#'
#' @name sort
#'
#' @description Sorts a data frame by one or more columns.
#'
#' @param x Data frame.
#' @param by Field variable names or column indices by which to sort a data frame.
#' @param decreasing Logical value specifying whether to sort by decreasing order.
#' @param ... Not used.
#'
#' @examples
#' # Create a simple data frame:
#' x <- data.frame(
#'         u = c(1, 1, 1, 1, 2, 2, 2, 3),
#'         v = c(1, 2, 3, 4, 1, 2, 3, 3))
#'
#' sort(x, by = "u")
#' sort(x, by = "v")
#' sort(x, by = c("u", "v"))  # Sort by both variables.
#'
#' @seealso \code{\link[base]{order}}
#'

#' @rdname sort
#' @export
sort.data.frame <- function(x, decreasing = FALSE, by, ...){
   # Define 'by':
   if (missing(by)) if (length(attr(x, "key")) > 0) by <- attr(x, "key") else by <- names(x)

   # Check if all variables are in the target object.
   if (!all(by %in% names(x))) stop("Some column names are not in target object.")

   # Build and evaluate 'order' expression:
   str <- "base::order("
   for (i in 1:length(by)){
     if (i > 1) str <- paste(str, ",")
     str <- paste(str, "x[, '", by[i], "']", sep = "")
   }
   str <- paste(str, ")", sep = "")
   ix <- eval(parse(text = str))

   # Invert index if '!increasing' is TRUE
   if (decreasing) ix <- rev(ix)
   temp <- attributes(x) # Save attributes.
   temp$row.names <- temp$row.names[ix] # Sort row names.
   x <- x[ix, , drop = FALSE] # Sort data frame.
   attributes(x) <- temp # Restore attributes.

   return(x)
}
