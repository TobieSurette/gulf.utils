#' Clear
#'
#' @name clear
#'
#' @description Functions to clear or erase items.
#'
#' @param x Graphic handle(s)
#'
#' @examples
#' # Clear graphics:
#' clg()
#' clg(grDevices::dev.list())
#'
#' # Clear variables:
#' clm()
#'
#' # Clear R console:
#' clc()
#'
#' @section Functions:
#' \describe{
#'    \item{\code{clg}}{Clear graphics windows.}
#'    \item{\code{clm}}{Clear all variables from memory.}
#'    \item{\code{clc}}{Clear the R console.}
#' }

#' @rdname clear
#' @export clg
clg <- function(x){
   if (missing(x))
      grDevices::graphics.off()
   else{
      if (is.null(x) | length(x) == 0){
         grDevices::graphics.off()
      }else{
         x <- round(as.numeric(x))
         if (all(x < 0)) x <- grDevices::dev.list()[!(grDevices::dev.list() %in% abs(x))]
         x <- unique(x)
         x <- x[x %in% grDevices::dev.list()]
         for (i in 1:length(x)) grDevices::dev.off(x[i])
      }
   }
}

#' @rdname clear
#' @export clm
clm <- function() rm(list = ls(envir = globalenv()), envir = globalenv())

#' @rdname clear
#' @export clc
clc <- function() cat("\014")
