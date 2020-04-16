#' Time Extraction and Conversion
#'
#' @description These are convenience functions to extract a time field from an object, such as a character
#' string or a data frame.
#'
#' @param x Object.
#' @param year Date year.
#' @param month Date month.
#' @param day Date day.
#'
#' @return Generally a \code{POSIXct} or \code{POSIXt} object.
#'
#' @examples
#' time() # Base method.
#'
#' # Single day:
#' date(year = 2000, month = 9, day = 19)
#'
#' # Entire month:
#' date(year = 2000, month = 9, day = 1:30)
#'
#' @export time
time <- function(x, ...) UseMethod("time")

#' @describeIn time Default time method.
time.default <- function(x, date, time, year, month, day, hour = 0, minute = 0, second = 0, ...){
   if (missing(x) & missing(hour) & missing(time)) return(Sys.time())
   if (!missing(x)) return(stats::time(x, ...))
   if (missing(date)) date <- paste0(year, "-", month, "-", day) else date <- as.character(date)
   if (missing(time)) time <- paste0(hour, ":", minute, ":", second)
   v <- as.POSIXlt(paste0(date ," ", time), tz = "")

   return(v)
}

#' @describeIn time Convert time from character string.
time.character <- function(x){
   # Initialize variables:
   min <- hour <- rep(NA, length(x))
   sec <- rep(0, length(x))

   # Format = "hh"
   index <- nchar(x) <= 2
   hour[index] <- as.numeric(x[index])

   # Format = "hhmm"
   index <- nchar(x) == 4
   hour[index] <- as.numeric(substr(x[index], 1, 2))
   min[index]  <- as.numeric(substr(x[index], 3, 4))

   # Format = "hhmmss"
   index <- nchar(x) == 6
   hour[index] <- as.numeric(substr(x[index], 1, 2))
   min[index]  <- as.numeric(substr(x[index], 3, 4))
   sec[index]  <- as.numeric(substr(x[index], 5, 6))

   # Format = "hh:mm:ss"
   index <- grep(":", x)
   y <- strsplit(x[index], ":")
   hour[index] <- as.numeric(unlist(lapply(y, function(x) x[1])))
   min[index] <- as.numeric(unlist(lapply(y, function(x) x[2])))
   sec[index] <- as.numeric(unlist(lapply(y, function(x) x[2])))

   index <- nchar(x) > 8
   v[index] <- as.POSIXct(x[index])

   return(v)
}

#' @describeIn time Extract time from data frame.
time.data.frame <- function(x, ...){
   if (nrow(x) == 0) return(NULL)
   v <- NULL  # Init result variable.
   names(x) <- tolower(names(x))
   if (all(c("year", "month", "day", "hour", "minute", "second") %in% names(x))){
      v <- time.default(year = x$year, month = x$month, day = x$day, hour = x$hour, minute = x$minute, second = x$second)
   }
   if (all(c("year", "month", "day", "time") %in% names(x))){
      hour <- as.numeric(substr(x$time, 1, 2))
      minute <- as.numeric(substr(x$time, 4, 5))
      second <- as.numeric(substr(x$time, 7, 8))
      v <- time.default(year = x$year, month = x$month, day = x$day, hour = hour, minute = minute, second = second)
   }
   if (all(c("date", "time") %in% names(x))) v <- date(paste0(x$date, " ", x$time, "AST"))

   if (is.null(v)) stop("Unable to convert time fields.")
   return(v)
}
