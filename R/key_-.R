#' Assign Index Key
#'
#' @description Assign the variable name(s) which make up and index key for a given object.
#'
#' @param x Object.
#' @param ... Other arguments (not used).
#' @param value Index key to be assigned.
#'
#' @examples
#' x <- data.frame(year = 2010:2014, measurement = rnorm(5))
#' key(x) <- "year" # Assign key.
#' key(x) # Retrieve key.
#'
#' @export key<-
#' @export key<-.default
#'
"key<-" <- function(x, ...) UseMethod("key<-")

#' @describeIn key<- Default key assignment method.
"key<-.default" <- function(x, value){
   if (!is.null(value)){
      if (!is.character(value)) stop("Key must contain variable name(s).")
      if (!all(value %in% names(x))) stop("Variable name(s) not in target object.")
   }

   # Assign key:
   attr(x, "key") <- value

   return(x)
}
