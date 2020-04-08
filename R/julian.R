julian <- function(x){
   # JULIAN - Returns Julian day for a given time.

   reference <- paste0(as.numeric(substr(as.character(x), 1, 4)), "-01-01")
   
   v <- time2day(x, reference) + 1
   
   return(v)
}
