match.data.frame <- function(x, y, by = intersect(names(x), names(y))){
   # MATCH.DATA.FRAME - Matches the rows from one data frame to another.
   
   # Check 'by' argument:
   if (length(by) == 0) stop("There must be at least one common variable for the macth to be performed.")
   if (!all(by %in% names(x))) stop("Some 'by' variables are not in source object 'x'.")
   if (!all(by %in% names(y))) stop("Some 'by' variables are not in target object 'y'.")
   
   # Convert fields to character vectors:
   fun <- function(x){ x <- as.character(x); x[is.na(x)] <- " "; return(x)}
   x[by] <- sapply(x[by], fun)
   y[by] <- sapply(y[by], fun)
      
   # Check that the index key is unique in target object 'y':
   if (any(duplicated(y[by]))) stop ("Target object 'y' index key is not unique.")
   
   # Find the indices:
   if (length(by) == 1){
      # Use usual vector-based 'match' function:
      index <- base::match(x[, by], y[, by])
   }else{      
      # Merge index column from 'y' into 'x':
      index <- base::match(apply(x[by], 1, function(x) paste0(x, collapse = "-t-")), 
                           apply(y[by], 1, function(x) paste0(x, collapse = "-t-")))
   }

   return(index)
}
