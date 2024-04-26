#' Fix Encoding
#'
#' @description Fix encoding issues.
#'
#' @param x R object containing character data.
#' @param ... Not used.
#'
#' @return R object with standardized character encoding.
#'

#' @export
recode <- function(x, ...) UseMethod("recode")

#' @export
recode.default <- function(x,  ...){
   if (is.list(x)){
      for (i in 1:length(x)) x[[i]] <- recode(x[[i]])
   }

   if (is.character(x)) x <- recode(x)

   return(x)
}

#' @export
recode.character <- function(x,  ...) return(iconv(x, from = "ISO-8859-1", to = "UTF-8"))
