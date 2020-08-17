#' Clear
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
#' @export clg
#' @export clm
#' @export clc
#'

#' @describeIn clear Clear graphics windows.
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

#' @describeIn clear Clear all variables from memory.
clm <- function() rm(list = ls(envir = globalenv()), envir = globalenv())

#' @describeIn clear Clear the R console.
clc <- function() cat("\014")
