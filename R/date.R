#' Date Extraction and Conversion
#'
#' @name date
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
#' @seealso \code{\link[gulf.utils]{time}}
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
#' @export
date <- function(x, ...) UseMethod("date")

#' @describeIn date Default date method.
#' @export
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
#' @export
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
#' @export
date.data.frame <- function(x, ...){
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
   if (length(datevars) != 3) stop("Unable to locate date fields.")

   # Collate and convert date fields:
   year  <- x[, datevars["year"]]
   month <- x[, datevars["month"]]
   day   <- x[, datevars["day"]]
   v <- rep(ISOdate(NA, NA, NA, tz = ""), nrow(x))
   index <- which(!is.na(year) & !is.na(month) & !is.na(day))

   if (length(index) > 0) v[index] <- as.POSIXct(paste0(formatC(year[index]), "-", month[index], "-", day[index]), tz = "")

   return(v)
}

#' @export
day <- function(x, ...) UseMethod("day")

#' @describeIn date Default method for extracting month from a date.
#' @export
day.default <- function(x, ...){
   ix <- grep("day", tolower(names(x)))
   if (length(ix) > 0) return(x[[ix[1]]])

   ix <- grep("date", tolower(names(x)))
   if (length(ix) > 0) x <- x[[ix]]

   if (length(grep("POSIX", class(x))) == 0) x <- gulf.utils::date(x)
   if (length(grep("POSIX", class(x))) > 0){
      ux <- unique(x)
      v <- as.numeric(substr(as.character(ux), 9, 10))
      return(v[match(x, ux)])
   }

   return(NULL)
}

#' @export
month <- function(x, ...) UseMethod("month")

#' @describeIn date Default method for extracting month from a date.
#' @export
month.default <- function(x, ...){
   ix <- grep("month", tolower(names(x)))
   if (length(ix) > 0) return(x[[ix[1]]])

   ix <- grep("date", tolower(names(x)))
   if (length(ix) > 0) x <- x[[ix]]

   if (length(grep("POSIX", class(x))) == 0) x <- gulf.utils::date(x)
   if (length(grep("POSIX", class(x))) > 0){
      ux <- unique(x)
      v <- as.numeric(substr(as.character(ux), 6, 7))
      return(v[match(x, ux)])
   }

   return(NULL)
}

#' @export
year <- function(x, ...) UseMethod("year")

#' @describeIn date Default method for extracting year field from a date.
#' @export
year.default <- function(x, ...){
   ix <- grep("year", tolower(names(x)))
   if (length(ix) > 0) return(x[[ix[1]]])

   ix <- grep("date", tolower(names(x)))
   if (length(ix) > 0) x <- x[[ix]]

   if (length(grep("POSIX", class(x))) == 0) x <- gulf.utils::date(x)
   if (length(grep("POSIX", class(x))) > 0){
      ux <- unique(x)
      v <- as.numeric(substr(as.character(ux), 1, 4))
      return(v[match(x, ux)])
   }

   return(NULL)
}
