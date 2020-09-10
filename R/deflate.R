#' Deflate or Inflate
#'
#' @name deflate
#'
#' @description Inflate and deflate objects.
#'
#' @param x Object, usually some type of data frame.
#'
#' @examples
#'
#' x <- data.frame(year = 2018, count = rpois(10, 5), survey = "scs", group = rep(1:2, each = 5))
#' print(x)
#' x <- deflate(x)
#' print(x)
#' attributes(x)
#' x <- inflate(x)
#' print(x)
#'
#' @section Functions:
#' \describe{
#'    \item{\code{deflate}}{Generic \code{deflate} method.}
#'    \item{\code{inflate}}{Generic \code{inflate} method.}
#'    \item{\code{deflate.data.frame}}{Collapse columns with only single unique entries and store as object attributes.}
#'    \item{\code{inflate.data.frame}}{Expand object attributes and store as data columns.}
#' }
#'

#' @export deflate
deflate <- function(x, ...) UseMethod("deflate")

#' @export inflate
inflate <- function(x, ...) UseMethod("inflate")

#' @export
deflate.data.frame <- function(x, ...){
   tmp <- unlist(lapply(x, function(x) if (length(unique(x)) == 1) return(unique(x)) else return(NULL)))
   tmp <- tmp[!is.na(tmp)]
   tmp <- c(attributes(x), tmp)

   attributes(x) <- tmp
   return(x)
}

#' @export
inflate.data.frame <- function(x, ...){
   tmp <- attributes(x)

   return(x)
}

