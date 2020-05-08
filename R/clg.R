#' Clear
#'
#' @description Clears all graphics windows.
#'
#' @param x Graphic handle(s)
#'
#' @examples
#' # Clear graphics:
#' clg()
#' clg(dev.list())
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

clg <- function(x){
   if (missing(x))
      grDevices::graphics.off()
   else{
      if (is.null(x) | length(x) == 0){
         grDevices::graphics.off()
      }else{
         x <- round(as.numeric(x))
         if (all(x < 0)) x <- grDevices::dev.list()[!(dev.list() %in% abs(x))]
         x <- unique(x)
         x <- x[x %in% grDevices::dev.list()]
         for (i in 1:length(x)) grDevices::dev.off(x[i])
      }
   }
}

#' @describeIn clg Clear all variables from memory.
clm <- function() rm(list = ls(envir = globalenv()), envir = globalenv())

#' @describeIn clg Clear the R console.
clc <- function() cat("\014")
