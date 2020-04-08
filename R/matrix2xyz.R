matrix2xyz <- function(m, na.rm = FALSE){
   # MATRIX2XYZ - Converts a matrix to xyz format.
   
   # Column labels:
   xlab <- dimnames(m)[[1]]
   ylab <- dimnames(m)[[2]]
   
   # Build coordinate matrices:
   x <- repvec(1:dim(m)[2], nrow = dim(m)[1])
   y <- repvec(1:dim(m)[1], ncol = dim(m)[2])
   
   # Set row and column labels to numeric values:
   if (is.null(xlab)) xlab <- 1:dim(m)[1]
   if (is.null(ylab)) ylab <- 1:dim(m)[2]
   
   # Catenate vectors:
   if (is.character(xlab) | is.character(ylab)){
      v <- data.frame(x = as.vector(x), y = as.vector(y), z = as.vector(m))
   }else{
      v <- cbind(as.vector(x), as.vector(y), as.vector(m))
      dimnames(v) <- list(NULL, c("x", "y", "z"))
   }
   
   # Remove 'z' NA values:
   if (na.rm){
      v <- v[!is.na(v[, "z"]), ]
   }
   
   return(v)
}
