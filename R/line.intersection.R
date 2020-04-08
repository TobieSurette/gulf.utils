line.intersection <- function(x0, y0, x, y, collapse = TRUE, tolerance = eps()){
   # Find the intersect point of two lines.

   # Check input arguments:
   if (missing(x0) | missing(y0) | missing(x) | missing(y))
      stop("Line vertices must be specified.")
   if ((length(x0) != length(y0)) | (length(x) != length(y)))
      stop("Vectors of line vertices must be the same length.")
   
   if ((length(x0) == 2) & (length(x) == 2)){
      # Calculate deltas:
      dx0 <- x0[1] - x0[2]
      dx  <- x[1] - x[2]
      dy0 <- y0[1] - y0[2]
      dy  <- y[1] - y[2]

      # Check if lines are parallel:
      a <- dx0 * dy - dx * dy0
      if (abs(a) < tolerance) return (list(x = NA, y = NA))

      # Calculate intersection point:
      d1 <- x0[1]*y0[2] - x0[2]*y0[1]
      d2 <- x[1] * y[2] - x[2] * y[1]
      X <- (d1 * dx - dx0 * d2) / a
      Y <- (d1 * dy - dy0 * d2) / a

      # Check if point lies within the line segments:
      pt.empty <- list(x = NA, y = NA)
      if (X < (min(x0) - tolerance) | X > (max(x0) + tolerance)) return(pt.empty)
      if (Y < (min(y0) - tolerance) | Y > (max(y0) + tolerance)) return(pt.empty)
      if (X < (min(x)  - tolerance) | X > (max(x)  + tolerance)) return(pt.empty)
      if (Y < (min(y)  - tolerance) | Y > (max(y)  + tolerance)) return(pt.empty)

      return(list(x = X, y = Y))
   }else{
      xi <- rep(NA, (length(x0)-1) * (length(x)-1))
      yi <- rep(NA, (length(y0)-1) * (length(y)-1))
      for (i in 1:(length(x0)-1)){
         for (j in 1:(length(x)-1)){
            temp <- line.intersection(x0[i:(i+1)], y0[i:(i+1)], x[j:(j+1)], y[j:(j+1)])
            xi[(j-1)*(length(x0)-1) + i] <- temp$x
            yi[(j-1)*(length(x0)-1) + i] <- temp$y
         }
      }
      if (collapse){
         index <- !is.na(xi) & !is.na(yi)
         xi <- xi[index]
         yi <- yi[index]
      }
   }
   
   return(list(x = xi, y = yi))
}
