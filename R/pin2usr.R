pin2usr <- function(x = NULL, y = NULL){
   # PIN2USR - Converts from plot inches ('pin') to user ('usr') coordinates.

   # Flag whether to return value as a data frame.
   flag <- TRUE

   # Convert 'x' data frame to vector:
   if (is.data.frame(x)){
      y <- x[, 2]
      x <- x[, 1]
   }

   # Extract 'usr' graphical coordinates:
   if (is.null(x)){
      x <- c(0, par("pin")[1])
      y <- c(0, par("pin")[2])
      flag <- FALSE
   }

   # Parse single 'x' argument:
   if (is.null(y)){
      if (length(x) != 4) stop("Unable to parse input argument 'x'.")
      y <- x[3:4]
      x <- x[1:2]
      flag <- FALSE
   }

   # Check input arguments:
   if (length(x) != length(y)) stop("Input arguments have inconsistent lengths.")

   # Convert plot inches to user coordinates:
   ux <- (x / par("pin")[1]) * diff(par("usr")[1:2]) + par("usr")[1]
   uy <- (y / par("pin")[2]) * diff(par("usr")[3:4]) + par("usr")[3]

   # Return results:
   if (flag){
      return(data.frame(x = ux, y = uy))
   }else{
      return(c(ux, uy))
   }
}
