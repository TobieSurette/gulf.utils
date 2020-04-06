repvec <- function(x, n = 1, dim = NULL, nrow = NULL, ncol = NULL){
   # REPVEC - Repeats a vector along matrix dimensions.

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
   X <- matrix(rep(x, n), nrow = nrow, ncol = ncol)

   # Transpose if necessary:
   if (transpose) X <- t(X)

   return(X)
}
