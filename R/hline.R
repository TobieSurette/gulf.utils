#' Draw horizontal or vertical lines
#'
#' These functions are used to draw hortizontal or vertical lines on an existing plot.
#'
#' @aliases hline vline
#'
#' @param x A numeric vector.
#' @param lower A vector which defines the line(s)' lower plot range.
#' @param upper A vector which defines the line(s)' upper plot range.
#' @param ... Arguments passed onto 'graphics::lines'
#'
#' @examples
#'   plot(c(0, 10), c(0, 10), type = "n")
#'   hline(9, col = "red", lty = "dashed")
#'   hline(1:5, col = "black", lty = "solid", lwd = 2)
#'
#' @export hline
#' @export vline

#' @describeIn hline Draw horizontal lines.
hline <- function(x, lower, upper, ...){
   # Define default line bounds:
   if (missing(lower)) lower <- graphics::par("usr")[1]
   if (missing(upper)) upper <- graphics::par("usr")[2]

   # Remove lines which are not in the plot region:
   x <- x[(x >= graphics::par("usr")[3]) & (x <= graphics::par("usr")[4])]

   # Check argument sizes and draw lines:
   if (length(x) > 0){
      if (length(lower) == 1) lower <- rep(lower, length(x))
      if (length(upper) == 1) upper <- rep(upper, length(x))
      if (length(lower) != length(upper)) stop("'lower' and 'upper' have inconsistent dimensions.")
      if (any(lower > upper)) stop("Lower bounds cannot be greater than upper bounds.")
      if (length(lower) != length(x)) stop("Number of lines and bounds are inconsistent.")
      for (i in 1:length(x)) graphics::lines(c(lower[i], upper[i]), rep(x[i], 2), ...)
   }
}

#' @describeIn vline Draw vertical lines.
vline <- function(x, lower, upper, ...){
   # Define default line bounds:
   if (missing(lower)) lower <- graphics::par("usr")[3]
   if (missing(upper)) upper <- graphics::par("usr")[4]

   # Remove lines which are not in the plot region:
   x <- x[(x >= graphics::par("usr")[1]) & (x <= graphics::par("usr")[2])]

   # Check argument sizes and draw lines:
   if (length(x) > 0){
      if (length(lower) == 1) lower <- rep(lower, length(x))
      if (length(upper) == 1) upper <- rep(upper, length(x))
      if (length(lower) != length(upper)) stop("'lower' and 'upper' have inconsistent dimensions.")
      if (any(lower > upper)) stop("Lower bounds cannot be greater than upper bounds.")
      if (length(lower) != length(x)) stop("Number of lines and bounds are inconsistent.")
      for (i in 1:length(x)) graphics::lines(rep(x[i], 2), c(lower[i], upper[i]), ...)
   }
}
