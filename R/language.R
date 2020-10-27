#' Language Options
#'
#' @description Function to standardize language options.
#'
#' @param x Character strings.
#'
#' @examples
#' language()
#'
#' language("f")     # Returns 'french'.
#' language("franc") # Returns 'french'.
#' language("ang")   # Returns 'english'.
#' language("lat")   # Returns 'latin'.

#' @export
language <- function(x){
   options <- c("french", "francais", "français", "english", "anglais", "latin")
   if (missing(x)) return(options)
   x <- options[grep(paste0("^", tolower(x)), options)[1]]
   x[x %in% c("francais", "français")] <- "french"
   x[x %in% c("anglais")] <- "english"
   return(x)
}
