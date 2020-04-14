
date <- function(x, ...) UseMethod("date")

date.default <- function(x, year, month, day, format = "yyyymmdd", ...){

   # Call base method:
   if (missing(x)){
      if (!missing(year) & !missing(month) & !missing(day)){
         x <- data.frame(year = year, month = month, day = day)
      }else{
         return(base::date(...))
      }
   }

   if (is.character(x)){
      v <- rep(NA, length(x))
      x <- gsub("[^0-9.-]", "", x)
      index <- which(nchar(x) == nchar(format))
      if (length(index)){
         year <- v[index]
      }
   }

   if (is.data.frame(x)){
      names(x) <- tolower(names(x))
      if (all(c("year", "month", "day") %in% names(x))){
         index <- !is.na(x$year) & !is.na(x$month) & !is.na(x$day)
         str <- paste(formatC(x$year[index]), "-", x$month[index], "-", x$day[index], sep = "")
         v  <-  as.POSIXct(str, tz = "")
         temp <- rep(ISOdate(NA, NA, NA, tz = ""), nrow(x))
         temp[index] <- v
         v <- temp
      }else{
         stop("'year', 'month' and 'day' fields are required to contruct the date.")
      }
      return(v)
   }

   return(base::date(x, ...))
}
