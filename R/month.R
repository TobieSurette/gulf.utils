#' Month
#'
#' @description Extract month from a data object.
#'
#'

#' @export
month <- function(x, ...) UseMethod("month")

#' @export
month.default <- function(x, ...){
   if ("month" %in% tolower(names(x))) return(x[["month"]])
}

#' @export
month.data.frame <- function(x, ...){
   names(x) <- tolower(names(x))
   if ("month" %in% tolower(names(x))) return(x$month)
   if ("date" %in% tolower(names(x))) date <- x$date else date <- date(x)
   ux <- unique(date)
   month <- as.numeric(substr(ux, 6, 7))
   return(month[match(date, ux)])
}

