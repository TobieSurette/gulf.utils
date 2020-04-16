#' Sort Data Frame
#'
#' @description Sorts a dta frame by one or more columns.
#'
#' @param x Data frame.
#' @param by Data frame variable names or column indices by which to sort the data frame.
#' @param increasing Logical value specifying whether to sort by increasing order.
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
#' @export sort.data.frame
sort.data.frame <- function(x, by = NULL, increasing = TRUE){
   # SORT.DATA.FRAME - Sorts a data frame object.

   if (is.null(by)){
      by <- names(x)
   }

   # Check if all variables are in the target object.
   if (!all(by %in% names(x))) stop("Some column names are not in target object.")

   # Build and evaluate 'order' expression:
   str <- "order("
   for (i in 1:length(by)){
     if (i > 1) str <- paste(str, ",")
     str <- paste(str, "x[, '", by[i], "']", sep = "")
   }
   str <- paste(str, ")", sep = "")
   index <- eval(parse(text = str))

   # Invert index if '!increasing' is TRUE
   if (!increasing) index <- rev(index)
   temp <- attributes(x) # Save attributes.
   temp$row.names <- temp$row.names[index] # Sort row names.
   x <- x[index, , drop = FALSE] # Sort data frame.
   attributes(x) <- temp # Restore attributes.
   return(x)
}
