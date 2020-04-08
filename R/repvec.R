#' Repeat vectors
#'
#' Create matrices by repeating vectors row-wise or column-wise a specified number of times.
#'
#' @param x Scalar or vector.
#' @param n Integer specifying the number of times \code{x} is to be repeated.
#' @param dim Integer specifying along which dimension \code{x} is to be repeated.
#' @param nrow Integer specifying the number of row-wise repetitions of \code{x}.
#' @param ncol Integer specifying the number of column-wise repetitions of \code{x}.
#'
#' @examples
#'   repvec(1, 12)              # Column vector of ones.
#'   repvec(1, ncol = 12)       # Row vector of ones.
#'   repvec(1:10, 12, dim = 2)  # 10x12 matrix with 1:10 in each column.
#'   repvec(1:10, 12, dim = 1)  # 12x10 matrix with 1:10 in each row.
#'   repvec(1:10, ncol = 12)    # 10x12 matrix with 1:10 in each column.
#'
#' @export repvec
#'

repvec <- function(x, n = 1, dim = NULL, nrow = NULL, ncol = NULL){
   if ((is.null(dim)) & (is.null(nrow)) & (is.null(ncol))){
      # Only n is specified:
      ncol <- n
      nrow <- length(x)
      transpose <- TRUE
   }else{
      # "nrow" or "ncol" are used:
      if (!is.null(nrow)){
         n <- nrow
         ncol <- nrow
         nrow <- length(x)
         transpose <- TRUE
      }else{
         if (!is.null(ncol)){
            n <- ncol
            nrow <- length(x)
            transpose <- FALSE
         }
      }

      # "dim" is used:
      if (!is.null(dim)){
         nrow <- length(x)
         ncol <- n
         if (dim == 1) transpose <- TRUE
         if (dim == 2) transpose <- FALSE
      }
   }

   # Create matrix:
   M <- matrix(rep(x, n), nrow = nrow, ncol = ncol)

   # Transpose if necessary:
   if (transpose) M <- t(M)

   return(M)
}
