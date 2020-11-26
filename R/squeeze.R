#' Remove Redundant Elements
#' 
#' @description Function to remove redundant elements from a data object.
#' 
#' @param x Data object.
#' 

#' @export
squeeze <- function(x, ...) UseMethod("squeeze")

#' @describeIn squeeze Remove empty data columns.
#' @export
squeeze.data.frame <- function(x, ...){
   ix <- unlist(lapply(x, function(x) return(all(is.na(x) | (x == "")))))
   x <- x[, !ix]
   return(x)
}


