#' Table to Matrix
#'
#' Convert a table object to a matrix.
#'
#' @param x A table (see \code{\link[base]{table}}).
#'
#' @return Logical value(s).
#'
#' @examples
#' # Vector:
#' x <- table(rpois(100, 5))
#'   as.matrix(x)
#'
#' # Two dimensional contigency table:
#' x <- table(data.frame(a = rpois(100, 5), b = rpois(100, 10)))
#' as.matrix(x)
#'
#' @export as.matrix.table
#'

as.matrix.table <- function(x){
   d <- dim(x)
   table(1:10)
   if (length(d) == 1){
      str <- names(x)
      attributes(x) <- list(dim = c(1, d), class = "matrix")
      colnames(x) <- str
   }
   if (length(d) == 2) class(x) <- "matrix"
   if (length(d) > 2) stop("Frequency table has too many dimensions to convert to matrix format.")

   return(x)
}
