#' Relative Time Conversion
#'
#' @description Calculate time difference relative to a specified reference time.
#'
#' @param x Time variable.
#' @param reference Reference date, time or day of the week.
#' @param units Time units: \code{secs}, \code{mins}, \code{hours} or \code{days}.
#'
#' @export time2unit
#' @export time2sec
#' @export time2min
#' @export time2hour
#' @export time2day
#' @export julian
#' @export week
#'
#' @seealso \code{\link[base]{difftime}}
time2unit <- function(x, reference, units, ...){
   units <- match.arg(tolower(units), c("secs", "mins", "hours", "days"))
   if (!missing(reference)) if (is.numeric(reference)) reference <- as.POSIXct(0, origin = "1970-01-01", tz = "GMT")
   if (!all(class(x) %in% c("POSIXct", "POSIXlt", "POSIXt"))) stop("'x' argument must be a valid R time type.")

   # Define reference time as the earliest time:
   if (missing(reference)) reference <- min(x, na.rm = TRUE)

   # Calculate time difference:
   return(as.numeric(difftime(x, reference, units = units)))
}

#' @describeIn time2unit Calculate time difference in \bold{seconds}.
time2sec <- function(x, ...) return(time2unit(x, units = "secs", ...))

#' @describeIn time2unit Calculate time difference in \bold{minutes}.
time2min <- function(x, ...) return(time2unit(x, units = "secs", ...))

#' @describeIn time2unit Calculate time difference in \bold{hours}.
time2hour <- function(x, ...) return(time2unit(x, units = "hours", ...))

#' @describeIn time2unit Calculate time difference in \bold{days}.
time2day <- function(x, ...) return(time2unit(x, units = "days", ...))

#' @describeIn time2unit Calculate Julian day, i.e. the day of the year relative to January 1st.
julian <- function(x) return(time2day(x, paste0(as.numeric(substr(as.character(x), 1, 4)), "-01-01")) + 1)

#' @describeIn time2unit Calculate to in which \bold{week} of the year a date fall into.
week <- function(x, reference = "Sunday"){
   # WEEK - Convert R time format to relative days for some reference time.

   # Parse reference argument:
   days <- c("sunday", "monday", "tuesday", "wednesday", "thursday", "friday")
   if (is.character(reference)) reference <- match.arg(tolower(reference), days)

   # Calculate week:
   if (is.character(reference)){
      offset <- match(reference, days) - 1
      v <- floor((julian(x) - offset - 1) / 7) + 1
   }
   if (any(class(reference) %in% c("POSIXct", "POSIXlt", "POSIXt"))){
      v <- floor(time2day(x, reference) / 7) + 1
   }

   return(v)
}

