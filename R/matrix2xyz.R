#' Matrix Format Conversion
#'
#' @description These are functions to convert between different matrix formats. Generally, we may convert a two-dimensional
#' array of values to a linear xyz format which contains the coordinates or indices of the rows and columns
#' corresponding to each matrix element \code{z}.
#'
#' @aliases xyz2matrix
#'
#' @param m Matrix.
#' @param x Vector containing matrix row indices. Alternatively, \code{x} may be a
#'          three-column matrix or data frame containing columns specifying
#'          \code{x}, \code{y} and \code{z}.
#' @param y Vector containing the matrix column indices. Must be the same length as \code{x}.
#' @param z Vector containing the matrix elements which are mapped by \code{x} and \code{y}.
#'          Must be the same length as \code{x} and \code{y}.
#' @param na.rm Logical value specifying whether to remove \code{NA} values in \code{z} the output.
#'              The default is \code{FALSE}.
#' @param fun Function (e.g. \code{\link[base]{mean}}) to be used on \code{z} when the indices \code{x}
#'            and \code{y} have duplicate entries.
#'
#' @return Matrix
#'
#' @examples
#' # Define  random matrix:
#' m <- round(matrix(runif(40), nrow = 8), 2)
#'
#' # Convert to XYZ format:
#' v <- matrix2xyz(m)
#'
#' # Convert back to matrix format:
#' xyz2matrix(x = v[,"x"], y = v[,"y"], z = v[,"z"])
#'
#' # Equivalently:
#' xyz2matrix(v)
#'
#' # Convert XYZ elements with duplicate entries to matrix:
#' x <- c(1, 1, 3, 2, 3)
#' y <- c(2, 2, 1, 2, 3)
#' z <- 1:5
#' m <- xyz2matrix(x, y, z, mean)
#'
#' # Convert back to XYZ format:
#' v <- matrix2xyz(m, na.rm = TRUE)
#'
#' @export matrix2xyz
#' @export xyz2matrix

#' @describeIn matrix2xyz Convert from matrix to xyz format.
matrix2xyz <- function(m, na.rm = FALSE){
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

#' @describeIn matrix2xyz Convert from xyz to matrix format.
xyz2matrix <- function(x, y, z, fun){
   # Parse 'x' argument:
   if (!is.null(dim(x))){
      if (dim(x)[2] == 3){
         z <- x[, 3]
         y <- x[, 2]
         x <- x[, 1]
      }
   }

   # Check that vectors have the appropriate lengths:
   if (any(c(length(y), length(z)) != length(x)))
      stop("'x', 'y' and 'z' vectors must all be the same length.")

   # Extract unique values:
   ux <- sort(unique(x))
   uy <- sort(unique(y))

   # Create matrix coordinate matrices:
   xx <- repvec(ux, nrow = length(uy))
   yy <- repvec(uy, ncol = length(ux))

   # Create index vectors:
   i <- paste(x, y)

   # Apply function for duplicate indices:
   if ((length(unique(i)) < length(i)) & missing(fun))
      stop("There are duplicate index values and 'fun' is undefined.")
   if (!missing(fun)){
      if (length(unique(i)) < length(i)){
         temp <- aggregate(z, by = list(i), fun)
         i <- temp[, 1]
         z <- temp[, 2]
      }
   }

   ii <- paste(as.vector(xx), as.vector(yy))

   # Initialize result variable:
   zz <- rep(NA, prod(dim(xx)))

   # Dump values:
   zz <- z[match(ii, i)]

   # Set dimensions:
   dim(zz) <- dim(xx)

   return(zz)
}



