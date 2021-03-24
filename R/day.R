#' Day
#'
#' @description Extract day from a data object.
#'
#'

#' @export
day <- function(x, ...) UseMethod("day")

#' @export
day.default <- function(x, ...){
   if ("day" %in% tolower(names(x))) return(x[["day"]])
}

#' @export
day.data.frame <- function(x, ...){
   names(x) <- tolower(names(x))
   if ("day" %in% tolower(names(x))) return(x$day)
   if ("date" %in% tolower(names(x))) date <- x$date else date <- date(x)
   ux <- unique(date)
   day <- as.numeric(substr(ux, 9, 10))
   return(day[match(date, ux)])
}
