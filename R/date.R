#' Date Extraction and Conversion
#'
#' @description These are wrapper functions to extract a date field from an object, such as a character
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
#' date() # Base method.
#'
#' # Single day:
#' date(year = 2000, month = 9, day = 19)
#'
#' # Entire month:
#' date(year = 2000, month = 9, day = 1:30)
#'
#' # Sorts out YYYYMMDD and DDMMYYYY:
#' date(c("2000-01-19", "19-01-2000", "2000/01/19"))
#'
#' # Apply to data frame:
#' x <- data.frame(Year = 2000, Month = 01, Day = 1:30)
#' date(x)
#'
#' @export date
#' @export date.default
#' @export date.character
#' @export date.data.frame
#'
date <- function(x, ...) UseMethod("date")

#' @describeIn date Default date method.
date.default <- function(x, year, month, day, ...){
   if (!missing(x)) if (length(x) > 0) return(date(as.character(x)))
   if (missing(year) | missing(month) | missing(day)) return(base::date())

   # Parse 'year', 'month' and 'day' arguments:
   n <- c(length(year), length(month), length(day))
   if (length(year) == 1)  year  <- rep(year, max(n))
   if (length(month) == 1) month <- rep(month, max(n))
   if (length(day) == 1)   day   <- rep(day, max(n))
   if ((length(year) != length(month)) | (length(month) != length(day)))
      stop("'year', 'month' or 'day' have inconsistent lengths.")
   v <- paste0(year, "-", month, "-", day)
   return(date(paste0(year, "-", month, "-", day)))
}

#' @describeIn date Convert character string to date.
date.character <- function(x, ...){
   x <- gsub("^ ", "", x)
   x <- gsub(" $", "", x)
   x <- gsub("[/ ]", "-", x)
   fun <- function(x){
      i <- which(nchar(x) == 4)
      if (i == 1) return(paste(x[1:3], collapse = "-"))
      if (i == 3) return(paste(x[3:1], collapse = "-"))
      return("          ")
   }
   v <- rep(ISOdate(NA, NA, NA, tz = ""), length(x))
   x <- strsplit(x, "-")

   index <- which((unlist(lapply(x, length)) == 3) &
                  (unlist(lapply(x, function(x) if (length(x) == 0) return(0) else max(nchar(x)))) == 4))
   if (length(index) > 0) v[index] <- unlist(lapply(x, fun))

   return(v)
}

#' @describeIn date Extract date from a data frame.
date.data.frame <- function(x, year, month, day, ...){
   names(x) <- tolower(names(x))

   # Look for date variable:
   datevar <- names(x)[grep("^date", names(x))][1]
   if (length(datevar == 0)) datevar <- names(x)[grep("date$", names(x))][1]
   datevar <- datevar[!is.na(datevar)]
   # Process date variable:
   if (length(datevar) > 0){
      if (length(grep("POSIX", class(x[, datevar]))) > 0){
         return(x[, datevar])
      }else{
         return(date(as.character(x[, datevar])))
      }
   }

   # Look for year, month and day fields:
   datevars <- c(year  = grep("year", names(x))[1],
                 month = grep("month", names(x))[1],
                 day   = grep("day", names(x))[1])
   if (length(datevars) != 3) stop("Unable to identity date fields.")

   # Collate and convert date fields:
   year  <- x[, datevars["year"]]
   month <- x[, datevars["month"]]
   day   <- x[, datevars["day"]]
   v <- rep(ISOdate(NA, NA, NA, tz = ""), nrow(x))
   index <- which(!is.na(year) & !is.na(month) & !is.na(day))

   if (length(index) > 0) v[index] <- as.POSIXct(paste0(formatC(year[index]), "-", month[index], "-", day[index]), tz = "")

   return(v)
}
