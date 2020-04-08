fin2plt <- function(x = NULL, y = NULL){
   # FIN2PLT - Converts from figure inches ('fin') to relative figure ('plt') coordinates.

   # Flag whether to return value as a data frame.
   flag <- TRUE

   # Convert 'x' data frame to vector:
   if (is.data.frame(x)){
      y <- x[, 2]
      x <- x[, 1]
   }

   # Extract 'fin' graphical coordinates:
   if (is.null(x)){
      x <- c(0, par("fin")[1])
      y <- c(0, par("fin")[2])
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

   # Convert plot inches to figure inches:
   fx <- x
   fy <- y

   # Convert figure inches to relative coordinates:
   lx <- fx / par("fin")[1]
   ly <- fy / par("fin")[2]

   # Return results:
   if (flag){
      return(data.frame(x = lx, y = ly))
   }else{
      return(c(lx, ly))
   }
}
