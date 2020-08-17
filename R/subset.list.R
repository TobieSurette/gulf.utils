#' List Subset Function
#'
#' @description Returns a subset of a list object corresponding to a set of specified
#'              membership criteria.
#'
#' @param x A list vector.
#' @param index Regular list index.
#' @param ... Named arguments which refer to list cell contents.
#'
#' @return A list object is returned.
#'
#' @seealso \code{\link[base]{subset}}
#'
#' @export
subset.list <- function(x, index, ...){
   # Subset using 'index' argument:
   if (!missing(index)) x <- x[index]

   # Extract arguments:
   args <- list(...)

   n <- length(x) # Number of elements in the list.
   index <- rep(TRUE, n)
   if (length(args) > 0){
      for (i in 1:n){ # Loop over list elements.
         print(args)
         for (j in 1:length(args)){ # Loop arguments for each list element.
            if (!is.null(args[[j]])){
               if (any(names(args[j]) %in% names(x[[i]]))){
                  index[i] <- index[i] & any(x[[i]][[names(args[j])]] %in% args[[j]])
               }else{
                  index[i] <- FALSE
               }
            }
         }
      }
   }

   return(x[index])
}
