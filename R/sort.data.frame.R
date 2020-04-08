sort.data.frame <- function(x, by = NULL, increasing = TRUE){
   # SORT.DATA.FRAME - Sorts a data frame object.

   if (is.null(by)) by <- names(x)

   # Check if all variables are in the target object.
   if (!all(by %in% names(x))) stop("Some column names are not in target object.")

   # Build and evaluate 'order' expression:
   str <- "order("
   for (i in 1:length(by)){
     if (i > 1) str <- paste(str, ",")
     str <- paste(str, "x[, '", by[i], "']", sep = "")
   }
   str <- paste(str, ")", sep = "")
   index <- eval(parse(text = str))

   # Invert index if '!increasing' is TRUE
   if (!increasing) index <- index[length(index):1]
   temp <- attributes(x) # Save attributes.
   temp$row.names <- temp$row.names[index] # Sort row names.
   x <- x[index, , drop = FALSE] # Sort data frame.
   attributes(x) <- temp # Restore attributes.
   return(x)
}
