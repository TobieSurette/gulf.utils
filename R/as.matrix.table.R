as.matrix.table <- function(x){
   # AS.MATRIX.TABLE - Convert frequency table object to a matrix.
   
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
