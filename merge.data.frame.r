merge.data.frame <- function(x, y, by = intersect(names(x), names(y)), by.x = by, by.y = by,
                             all = FALSE, all.x = all, all.y = all, sort = TRUE, suffixes = c(".x",".y"),
                             incomparables = NULL, overwrite = FALSE, warnings = FALSE, one.to.one = FALSE,
                             names = NULL, ...){     
   # MERGE.DATA.FRAME - An overloaded version of the standard merge for data frames.
 
   # Save attributes of x:
   atts <- attributes(x)
   
   # If variable names are specified, subset 'y' variable:
   if (!is.null(names) & !is.null(y)) y <- y[, union(by, names)]

   # Check that source index key contains no NA valuse:
   if (warnings){
      if (any(is.na(y[, by.y]))) warning("Index key of source object 'y' contains NA values.")
   }
   
   # Check that the index key of the source object is unique:
   if (dim(y)[1] != dim(unique(y[by.y]))[1]) stop("Index key of the source object 'y' is not unique.")
  
   # If matches are to be one to one, check that target's key is unique:
   if (one.to.one){
      # Create index of rows with no NAs in their in index key:
      index <- apply(as.matrix(is.na(x[by.x])), 1, sum) > 0
      if (dim(x[!index,])[1] != dim(unique(x[!index,by.x]))[1]) stop("Index key of the target object 'x' is not unique.")
   }
   
   # Check that all rows of 'y' have at least one match in 'x':
   if (warnings & all.y){
      if (any(is.na(match(y, x, by = by.y)))) warning("Some rows of 'y' had no matches in 'x'.")
   }
   
   # Check if one-to-one merge is required:
   if (one.to.one){
      index1 <-  match.data.frame(x, y, by = by)
      index2 <-  match.data.frame(y, x, by = by)
      index1 <- index1[!is.na(index1)]
      index2 <- index2[!is.na(index2)]
      if ((length(index1) > length(unique(index1))) | (length(index2) > length(unique(index2)))){
         error("Match between data frames is not one-to-one.")
      }
   }
   
   # Merge variables that are present in 'y' but not in 'x':
   if (overwrite){
      # Define variable set to be appended:
      addvars <- setdiff(setdiff(names(y), by.y), names(x))
      # Define variable set to be replaced:
      repvars <- setdiff(setdiff(names(y), by.y), addvars)
      
      # Merge variables which are to be appended:
      if (length(addvars) > 0){
         x <- base::merge.data.frame(
                 x, y[, c(by.y, addvars)], by = by, by.x = by.x, by.y = by.y, 
                 all = all, all.x = all.x, all.y = all.y, sort = sort, suffixes = suffixes, 
                 incomparables = incomparables)
      }
      
      # Replace values:
      if (length(repvars) > 0){
         index <- match.data.frame(x, y, by = by)
         x[!is.na(index), repvars] <- y[index[!is.na(index)], repvars]
      }
   }else{
      # Perform typical merge:
      x <- base::merge.data.frame(x, y, by = by, by.x = by.x, by.y = by.y, 
              all = all, all.x = all.x, all.y = all.y, sort = sort, 
              suffixes = suffixes, incomparables = incomparables, ...)     
   }
   
   # Restore attributes of 'x':
   atts <- atts[setdiff(names(atts), c("names", "row.names"))]
   for (i in 1:length(atts)){
      attr(x, names(atts)[i]) <- atts[[i]]
   }
   
   return(x)
}
