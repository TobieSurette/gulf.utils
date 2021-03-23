#' Year
#'
#' @description Extract year from a data object.
#'
#'

#' @export
year <- function(x, ...) UseMethod("year")

#' @export
year.default <- function(x, ...){
   if ("year" %in% tolower(names(x))) return(x[["year"]])
}

#' @export
year.data.frame <- function(x, ...){
   names(x) <- tolower(names(x))
   if ("year" %in% tolower(names(x))) return(x$year)
   if ("date" %in% tolower(names(x))) date <- x$date else date <- date(x)
   ux <- unique(date)
   year <- as.numeric(substr(ux, 1, 4))
   return(year[match(date, ux)])
}
