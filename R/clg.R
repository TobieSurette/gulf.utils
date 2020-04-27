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
      graphics.off()
   else{
      x <- round(as.numeric(x))
      print(x)
      if (all(x < 0)) x <- dev.list()[!(dev.list() %in% abs(x))]
      x <- unique(x)
      print(x)
      x <- x[x %in% dev.list()]
      print(x)
      for (i in 1:length(x)) dev.off(x[i])
   }
}

#' @describeIn clg Clear all variables from memory.
clm <- function() rm(list = ls())

#' @describeIn clg Clear the R console.
clc <- function() cat("\014")
