#' Extract Year
#'
#' @description Extract year from a data object.

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

   return(as.numeric(substr(date(x), 1, 4)))
}
