match.default <- function(x, ...){
   # MATCH.DEFAULT - Default 'match' method.
   result <- base::match(x, ...)
   return(result)
}