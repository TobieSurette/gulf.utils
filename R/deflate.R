#' Deflate, Inflate and Expand
#'
#' @name deflate
#'
#' @description Inflate and deflate data objects.
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
#' attr(x, "status") <- "critical"
#' expand(x)
#' attr(x, "units") <- c(length = "millimeters", weight = "grams")
#'
#' @section Functions:
#' \describe{
#'    \item{\code{deflate}}{Generic \code{deflate} method.}
#'    \item{\code{inflate}}{Generic \code{inflate} method.}
#'    \item{\code{expand}}{Generic \code{expand} method.}
#'    \item{\code{deflate.data.frame}}{Collapse columns with only single unique entries and store as object attributes.}
#'    \item{\code{inflate.data.frame}}{Expand object attributes and store as data columns.}
#'    \item{\code{expand.data.frame}}{Expand object attributes and store as data columns.}
#' }
#'

#' @export deflate
deflate <- function(x, ...) UseMethod("deflate")

#' @export inflate
inflate <- function(x, ...) UseMethod("inflate")

#' @export expand
expand <- function(x, ...) UseMethod("expand")

#' @export
deflate.data.frame <- function(x, ...){
   tmp <- unlist(lapply(x, function(x) if (length(unique(x)) == 1) return(unique(x)) else return(NULL)))
   tmp <- tmp[!is.na(tmp)]

   if (length(tmp) > 0){
      # Store which variables were deflated and in which order:
      deflated <- rep(FALSE, ncol(x))
      names(deflated) <- names(x)
      deflated[names(tmp)] <- TRUE

      # Remove variables to be stored as attributes:
      x <- x[, setdiff(names(x), names(tmp))]
      attributes(x) <- c(attributes(x), tmp)

      attr(x, "deflated") <- deflated
   }

   return(x)
}

#' @export
inflate.data.frame <- function(x, ...){
   a <- attributes(x)
   if ("deflated" %in% names(a)){
      deflated <- a[["deflated"]]
      for (i in 1:sum(deflated)){
         var <- names(deflated)[which(deflated)[i]]
         x[, var] <- a[var][[1]]
      }
      vars <- names(x)
      vars[vars %in% names(deflated)] <- names(deflated)
      x <- x[vars]
   }

   return(x)
}

#' @export
expand.data.frame <- function(x, keep, ...){
   # Loop over attributes:
   a <- attributes(x)
   a <- a[setdiff(names(a), c("names", "class", "row.names"))]
   if (!missing(keep)) a <- a[keep]

   if (length(a) > 0){
      for (i in 1:length(a)){
         if (length(a[[i]]) == 1){
            vars <- names(x)
            x[, ncol(x)+1] <- a[[i]]
            names(x) <- c(vars, names(a[i]))
         }
         if ((length(a[[i]]) > 1) & (length(names(a[[i]])) > 0)){
            for (j in 1:length(a[[i]])){
               vars <- names(x)
               x[, ncol(x)+1] <- a[[i]][j]
               str <- names(a[[i]][j])
               if (length(names(a[i])) == 1) str <- paste0(names(a[i]), ".", str)
               names(x) <- c(vars, str)
            }
         }
      }
   }

   return(x)
}

#' Collapse singluar fields
#' Expand data frame so its fields contain singluar attributes.
#' - Including header information?
#' - Do not necessarily remove attributes.
#'
#' @rawNamespace S3method(collapse,data.frame)
collapse.data.frame <- function(){


}



