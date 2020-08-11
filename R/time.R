#' Time Extraction and Conversion
#'
#' @description These are convenience functions to extract a time field from an object, such as a character
#' string or a data frame.
#'
#' @param x Object.
#' @param date Date object or formatted character string representing a date.
#' @param time Time object or formatted character string representing a time.
#' @param year Date year.
#' @param month Date month.
#' @param day Date day.
#' @param hour Time hour.
#' @param minute Time minute.
#' @param second Time second.
#' @param ... Further arguments passed onto
#'
#' @return Generally a \code{POSIXct} or \code{POSIXt} object.
#'
#' @examples
#' time() # Current time (base method).
#'
#' # Equivalent nine o'clock specification:
#' time(9)
#' time("9")
#' time("9h")
#' time("9h00")
#' time("9:00")
#' time("9:00:00")
#'
#' # All equivalent time strings:
#' time("194501")
#' time("19:45:01")
#' time("19h45m01")
#' time("19h45m01")
#'
#' @export time
#'
time <- function(x, ...) UseMethod("time")

#' @describeIn time Default time method.
time.default <- function(x, date, time, year, month, day, hour = 0, minute = 0, second = 0, ...){
   if (missing(x) & missing(hour) & missing(time)) return(Sys.time())
   if (!missing(x)) return(stats::time(x, ...))
   if (missing(date)) date <- paste0(year, "-", month, "-", day) else date <- date(as.character(date))
   if (missing(time)) time <- paste0(hour, ":", minute, ":", second)
   v <- as.POSIXlt(paste0(date ," ", time), tz = "")

   return(v)
}

#' @describeIn time Convert time from logical value.
time.logical <- function(x) return(time(as.numeric(x)))

#' @describeIn time Convert time from numeric value.
time.numeric <- function(x){
   hour <- floor(x)
   hour[(hour < 0) & (hour > 24)] <- NA
   min <- 60 * (x %% 1)
   sec <- floor(60 * (min %% 1))
   min <- floor(min)

   v <- rep("", length(x))
   index <- which((x >= 0) & (x <= 24))
   if (length(index) > 0) v[index] <- paste0(hour[index], ":", min[index], ":", sec[index])

   return(time(v))
}

#' @describeIn time Convert time from character string.
time.character <- function(x){
   # Initialize variables:
   hour <- rep(NA, length(x))
   min <- rep(NA, length(x))
   sec <- rep(NA, length(x))

   # "hhmmss"-type formats:
   index <- setdiff(1:length(x), grep("[:a-z]", x))
   if (length(index) > 0){
      # Format = "hh"
      ii <- nchar(x[index]) <= 2
      hour[index[ii]] <- as.numeric(x[index[ii]])

      # Format = "hhmm"
      ii <- nchar(x[index]) == 4
      hour[index[ii]] <- as.numeric(substr(x[index[ii]], 1, 2))
      min[index[ii]]  <- as.numeric(substr(x[index[ii]], 3, 4))

      # Format = "hhmmss"
      ii <- nchar(x[index]) == 6
      hour[index[ii]] <- as.numeric(substr(x[index[ii]], 1, 2))
      min[index[ii]]  <- as.numeric(substr(x[index[ii]], 3, 4))
      sec[index[ii]]  <- as.numeric(substr(x[index[ii]], 5, 6))
   }

   # "hh:mm:ss"-type formats:
   index <- grep(":", x)
   if (length(index) > 0){
      hour[index] <- as.numeric(unlist(lapply(strsplit(x[index], ":"), function(x) x[1])))
      min[index] <- as.numeric(unlist(lapply(strsplit(x[index], ":"), function(x) x[2])))
      sec[index] <- as.numeric(unlist(lapply(strsplit(x[index], ":"), function(x) x[3])))
   }

    # Format = "9h30m59s"
   index <- grep("[0-9][h]", x)
   if (length(index) > 0){
      hour[index] <- as.numeric(unlist(lapply(strsplit(tolower(x[index]), "[hms]"), function(x) x[1])))
      min[index] <- as.numeric(unlist(lapply(strsplit(tolower(x[index]), "[hms]"), function(x) x[2])))
      sec[index] <- as.numeric(unlist(lapply(strsplit(tolower(x[index]), "[hms]"), function(x) x[3])))
   }

   # Set error values to NA:
   index <- which((hour > 99) | (hour < 0) | (min > 59) | (min < 0) | (sec > 59) | (sec < 0))
   hour[index] <- NA
   min[index] <- NA
   sec[index] <- NA

   # Minutes and seconds to zero when hour is known:
   min[!is.na(hour) & is.na(min)] <- 0
   sec[!is.na(hour) & is.na(sec)] <- 0

   index <- !is.na(hour) & !is.na(min) & !is.na(sec)
   v <- rep("", length(x))
   v[index] <- paste0(format(hour[index], width = 2), ":",
                      format(min[index], width = 2), ":",
                      format(sec[index], width = 2))
   v[index] <- gsub(" ", "0", v[index])

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
   if (all(c("date", "time") %in% names(x))) v <- as.POSIXct(paste(x$date, x$time))

   if (is.null(v)) stop("Unable to convert time fields.")
   return(v)
}
