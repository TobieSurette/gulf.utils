#' @title Date Extraction and Conversion
#'
#' @description These are wrapper functions to extract a date field from an object, such as a character
#' string or a data frame.
#'
#' @param x Object.
#' @param year Date year.
#' @param month Date month.
#' @param day Date day.
#' @param ... Further arguments.
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
#' date(c("2000-01-19", "19-01-2000", "2000/01/19", "00-1-19"))
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

   # Convert from character:
   v <- date(paste0(year, "-", month, "-", day))

   return(v)
}

#' @describeIn date Convert character string to date.
#' @export
date.character <- function(x, ...){
   # Standardize non-digit characters:
   x <- gsub("^ ", "", x)
   x <- gsub(" $", "", x)
   x <- gsub("[/ ]", "-", x)

   fun <- function(x){
      y <- strsplit(x, "-")[[1]]
      i <- which(nchar(y) == 4)
      if (length(i) == 1) if (i == 3) y <- y[3:1]
      if (nchar(y[1]) == 2) y[1] <- paste0("20", y[1])
      if ((gsub("[0-9]", "", y[1]) == "") & (gsub("[0-9]", "", y[2]) == "") & (gsub("[0-9]", "", y[3]) == "")){
         x <- paste(y, collapse = "-")
      }else{
         x <- NA
      }

      return(x)
   }

   # Convert to date format:
   ux <- unique(x[!is.na(x)])
   uv <- rep(as.POSIXct(NA), length(ux))
   for (i in 1:length(ux)) uv[i] <- as.POSIXct(fun(ux[i]))
   v <- uv[match(x, ux)]

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
   v <- rep(as.POSIXct(NA), nrow(x))
   ix <- which(!is.na(year) & !is.na(month) & !is.na(day))

   if (length(ix) > 0) v[ix] <- as.POSIXct(paste0(formatC(year[ix]), "-", month[ix], "-", day[ix]), tz = "")

   return(v)
}

# @describeIn date Generic method for extracting day from a date.
#' @export
day <- function(x, ...) UseMethod("day")

#' @describeIn date Default method for extracting day from a date.
#' @export
day.default <- function(x, ...){
   ix <- grep("day", tolower(names(x)))
   if (length(ix) > 0) return(x[[ix[1]]])

   ix <- grep("date", tolower(names(x)))
   if (length(ix) > 0) x <- x[[ix]]

   # Convert dates to POSIX object:
   if (length(grep("POSIX", class(x))) == 0){
      ix <- which(x != "")
      xx <- gulf.utils::date(x[ix])
      x[(x == "") | is.na(x)] <- NA
      x <- rep(as.POSIXct(NA), length(x))
      x[ix] <- xx
   }

   # Extract day from date:
   if (length(grep("POSIX", class(x))) > 0){
      ux <- unique(x[!is.na(x)])
      uv <- as.numeric(substr(as.character(ux), 9, 10))
      ix <- which(!is.na(x))
      v <- rep(NA, length(x))
      v[ix] <- uv[match(x[ix], ux)]
      return(v)
   }

   return(NULL)
}

# @describeIn date Generic method for extracting month from a date.
#' @export
month <- function(x, ...) UseMethod("month")

#' @describeIn date Default method for extracting month from a date.
#' @export
month.default <- function(x, ...){
   ix <- grep("month", tolower(names(x)))
   if (length(ix) > 0) return(x[[ix[1]]])

   ix <- grep("date", tolower(names(x)))
   if (length(ix) > 0) x <- x[[ix]]

   # Convert dates to POSIX object:
   if (length(grep("POSIX", class(x))) == 0){
      ix <- which(x != "")
      xx <- gulf.utils::date(x[ix])
      x[(x == "") | is.na(x)] <- NA
      x <- rep(as.POSIXct(NA), length(x))
      x[ix] <- xx
   }

   # Extract month from date:
   if (length(grep("POSIX", class(x))) > 0){
      ux <- unique(x[!is.na(x)])
      uv <- as.numeric(substr(as.character(ux), 6, 7))
      ix <- which(!is.na(x))
      v <- rep(NA, length(x))
      v[ix] <- uv[match(x[ix], ux)]
      return(v)
   }

   return(NULL)
}

# @describeIn date Generic method for extracting year field from a date.
#' @export
year <- function(x, ...) UseMethod("year")

#' @describeIn date Default method for extracting year field from a date.
#' @export
year.default <- function(x, ...){
   ix <- grep("year", tolower(names(x)))
   if (length(ix) > 0) return(x[[ix[1]]])

   ix <- grep("date", tolower(names(x)))
   if (length(ix) > 0) x <- x[[ix]]

   # Convert dates to POSIX object:
   if (length(grep("POSIX", class(x))) == 0){
      ix <- which(x != "")
      xx <- gulf.utils::date(x[ix])
      x[(x == "") | is.na(x)] <- NA
      x <- rep(as.POSIXct(NA), length(x))
      x[ix] <- xx
   }

   # Extract year from date:
   if (length(grep("POSIX", class(x))) > 0){
      ux <- unique(x[!is.na(x)])
      uv <- as.numeric(substr(as.character(ux), 1, 4))
      ix <- which(!is.na(x))
      v <- rep(NA, length(x))
      v[ix] <- uv[match(x[ix], ux)]
      return(v)
   }

   return(NULL)
}
