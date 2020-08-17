#' Remove Leading and Trailing Spaces
#'
#' @description Removes leading and trailing spaces from a character string(s).
#'
#' @param x Character string(s).
#'
#' @examples
#' x <- "  Why hello there. "
#' print(deblank(x))
#'
#' @export deblank
deblank <- function(x){
   x <- gsub("^[ \t\r\n]+", "", x, perl = TRUE)
   x <- gsub("[ \t\r\n]+$", "", x, perl = TRUE)
   return(x)
}
