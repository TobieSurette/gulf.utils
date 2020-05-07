#' Assign Data Format
#'
#' @description Assigns a data format(s) to an object.
#'
#' @param x Target object.
#' @param y Character string(s) specifying the name(s) of the variables or attributes to be assigned data formats.
#' @param ... Other arguments (not used).
#' @param value Character string(s) specifying the data format to be assigned.
#'
#' @details The \code{fmt} attribute may be erased by assigning a \code{NULL} value to it.
#'
#' @return An object with a \code{fmt} attribute attached to it.
#'
#' @examples
#' x <- data.frame(year = 2010:2014, month = 6, day = 1:5, measurement = rnorm(5))
#' fmt(x, "year")  <- "YYYY"
#' fmt(x, "month") <- "MM"
#' fmt(x, "day")   <- "DD"
#'
#' # Show attributes:
#' attributes(x)
#'
#' # Erase 'fmt' attribute:
#' fmt(x) <- NULL
#'
#' @export "fmt<-"
#' @export "fmt<-.default"
#' @export "fmt"
#' @export "fmt.default"
#'
#' @seealso \code{\link{metadata}}, \code{\link{key}}, \code{\link{description}}, \code{\link{units}}
"fmt<-" <- function(x, ...) UseMethod("fmt<-")

#' @describeIn fmt-set Default 'fmt' assignment method.
"fmt<-.default" <- function(x, y, value, ...){
   if (missing(y)){
      # Extract attributes fields from 'value':
      if (!is.null(names(value))){
         fmt(x, names(value)) <- value
         return(x)
      }
      # Assign attribute:
      attr(x, "fmt") <- value
   }else{
      if (!is.character(y)) stop("Named argument must be a character string(s).")
      if (!all(y %in% names(x))) stop("Named argument must be in target object.")
      tmp <- attr(x, "fmt")
      if (is.null(value)){
         tmp <- tmp[setdiff(names(tmp), y)]
      }else{
         if ((length(value) == 1) & (length(y) > 1)) value <- rep(value, length(y))
         if (length(y) != length(value)) stop("Named arguments must be the same length as assigned values.")
         tmp[y] <- as.vector(value)
      }
      attr(x, "fmt") <- tmp
   }

   return(x)
}

#' @describeIn fmt-set Generic 'fmt' extraction method.
fmt <- function(x, ...) UseMethod("fmt")

#' @describeIn fmt-set Default 'fmt' extraction method.
fmt.default <- function(x, ...) return(attr(x, "fmt"))
