time2hour <- function(x, reference){
   # TIME2HOUR - Convert R time format to relative hours for some reference time.

   # Convert numeric seconds to date format:
   if (!missing(reference)) if (is.numeric(reference)) reference <- as.POSIXct(0, origin = "1970-01-01", tz = "GMT")

   # Check 'x' argument:
   if (!all(class(x) %in% c("POSIXct", "POSIXlt", "POSIXt")))
      stop("'x' argument must be a valid R time type.")
   
   # Define reference time as the earliest time:
   if (missing(reference)) reference <- min(x, na.rm = TRUE)
   
   # Calculate time difference:
   v <- as.numeric(difftime(x, reference, units = "hours"))
   
   return(v)
}
