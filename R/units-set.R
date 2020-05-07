#' Assign Observational Units
#'
#' @description Assigns units to measured or observed data in an object.
#'
#' @param x Target object.
#' @param y Character string(s) specifying the name(s) of the variables or attributes to be assigned measurement units.
#' @param ... Other arguments (not used).
#' @param value Character string(s) specifying the units to be assigned.
#'
#' @details The \code{units} attribute may be erased by assigning a \code{NULL} value to it.
#'
#' @return An object with a \code{units} attribute attached to it.
#'
#' @examples
#' x <- data.frame(year = 2010:2014, measurement = rnorm(5))
#' units(x, "measurement") <- "centimeters"
#'
#' # Show attributes:
#' attributes(x)
#'
#' # Erase 'units' attribute:
#' units(x) <- NULL
#'
#' @export "units<-"
#' @export "units<-.default"
#' @export "units"
#' @export "units.default"
#'
#' @seealso \code{\link{metadata}}, \code{\link{key}}, \code{\link{description}}, \code{\link{fmt}}
"units<-" <- function(x, ...) UseMethod("units<-")

#' @describeIn units-set Default 'units' assignment method.
"units<-.default" <- function(x, y, value, ...){
   if (missing(y)){
      # Extract attributes fields from 'value':
      if (!is.null(names(value))){
         units(x, names(value)) <- value
         return(x)
      }
      # Assign attribute:
      attr(x, "units") <- value
   }else{
      if (!is.character(y)) stop("Named argument must be a character string(s).")
      if (!all(y %in% names(x))) stop("Named argument must be in target object.")
      tmp <- attr(x, "units")
      if (is.null(value)){
         tmp <- tmp[setdiff(names(tmp), y)]
      }else{
         if ((length(value) == 1) & (length(y) > 1)) value <- rep(value, length(y))
         if (length(y) != length(value)) stop("Named arguments must be the same length as assigned values.")
         tmp[y] <- as.vector(value)
      }
      attr(x, "units") <- tmp
   }

   return(x)
}

#' @describeIn units-set Generic 'units' extraction method.
units <- function(x, ...) UseMethod("units")

#' @describeIn units-set Default 'units' extraction method.
units.default <- function(x, ...) return(attr(x, "units"))
