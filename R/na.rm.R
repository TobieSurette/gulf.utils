#' @title Remove NA Values
#' 
#' @description Remove NA values from an object.
#' 
#' @param x R object.
#' 
#' 
#' 

#' @export na.rm
na.rm <- function(x) return(x[!is.na(x)])



