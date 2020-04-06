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