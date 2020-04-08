#' Clear graphics
#'
#' @param x Graphic handle(s)
#'
#' @examples
#' clg()
#' clg(dev.list())
#'
#' @export clg
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
