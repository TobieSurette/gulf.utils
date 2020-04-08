#' Doug's Bar Plot
#'
#' This is Doug's alternative version of the \code{\link[graphics]{barplot}}
#' function. Numeric \code{x} values may be passed directly for proper positioning along the
#' X axis, allowing for easy overlaying over existing plots. If both positive and negative 'y'
#' values are present, then the resulting stacked plot has bars separated above and below the X axis.
#'
#' @param y A numeric vector, table, matrix or data frame object containing numeric values which are
#'          to be displayed as a bar plot. Each row of the matrix will be plotted as a separate bar.
#'
#' @param x A vector specifying the numeric values or labels to be used for separating each bar along
#'          the horizontal axis.
#'
#' @param labels A character vector specifying labels to be used for labelling each bar along the horizontal
#'               axis. The number of labels must equal or be consistent with those of \code{x} or \code{y}.
#'
#' @param width A numeric value from 0 to 1 specifying the relative width of each bar. A value of zero corresponds
#'              to a bar with no width while a value of one implies that adjacent bars are touching (i.e. there
#'              is no space between the bars). The default value is \code{0.8}.
#'
#' @param col Vector specifying the colour(s) of the bars to be drawn. Each column of 'y' is drawn in a separate
#'            colour. The default colour for a single variable is "grey". If there is more than one variable
#'            and \code{col} is unspecified, then colours are interpolated from a grayscale palette.
#'
#' @param border Border colour(s) of the bars.
#'
#' @param add A logical value specifying if bars should be added to an existing plot. The default is \code{FALSE}.
#'
#' @param yaxs A character value specifying how to set the limits of the Y axis, see \code{\link[graphics]{par}}.
#'
#' @param legend A logical value specifying whether a legend is to be drawn. The default is \code{TRUE}.
#'             The legend entries are taken from the columns names of \code{y}.
#'
#' @param \dots Further arguments to be passed onto the \code{\link[graphics]{plot}} function.
#'
#' @examples
#'
#' # Generate random vector:
#' y <- 10*rnorm(5)+5
#'
#' # Simple barplot:
#' dbarplot(y)
#'
#' # Same barplot with explicit 'x':
#' dbarplot(y, x = c(1, 2, 4, 5, 7))
#'
#' # Add explicit labels:
#' dbarplot(y, x = c(1, 2, 4, 5, 7), labels = letters[1:5])
#'
#' # Display bar plot:
#' y <- data.frame("Variable 1" = c(1, -2, NA, -4, 5), "Variable 2" = 1:5)
#' dbarplot(y, xlab = "Order", ylab = "y",
#'          labels = c("First", "Second", "Third", "Fourth", "Fifth"))
#'
#' # Bar plot of lobster length-frequencies by sex:
#' x <- read.gulf(year = 2000, species = 2550, card = "len")
#' f <- t(freq(x, by = "sex"))
#' f <- as.data.frame(f[2:dim(f)[1], 1:2])
#' f[, 1] <- -f[, 1]  # Reverse sign of male frequencies.
#' names(f) <- sex.str(1:2) # Assign sex labels.
#' dbarplot(f)
#'
#' # Display a frequency table overlayed by the true distribution:
#' y <- table(round(rnorm(250, sd = 5)))
#' dbarplot(y, col = "grey")
#' x <- seq(-15, 15, len = 100)
#' lines(x, 250 * dnorm(x, sd = 5), col = "red", lwd = 2)
#'
#' @export dbarplot
#'

dbarplot <- function(y, x = NULL, labels = NULL, width = 0.8,
                     col = NULL, border = par("fg"), add = FALSE,
                     yaxs = ifelse(all(y[!is.na(y)] <= 0)|all(y[!is.na(y)] >= 0), "i", "r"),
                     legend = TRUE, ...){

   # Parse 'y' argument:
   if (is.table(y)){
      y <- as.matrix(y)
      class(y) <- "matrix"
      x <- as.numeric(rownames(y))
      if (any(is.na(x))) x <- NULL
   }
   if (!is.data.frame(y)){
      y <- as.data.frame(y)
   }
   if (dim(y)[1] == 1) y <- t(y)

   # Define 'col' argument:
   if ((length(col) != dim(y)[1]) & (length(col) != dim(y)[2])){
      if (dim(y)[2] == 1){
         col = "grey"
      }else{
         col = grey(seq(0, 1, len = dim(y)[2]))
      }
   }

   # Parse 'border' argument:
   if (length(border) == 1) border <- rep(border, length(col))

   # Define 'x' as an integer sequence if undefined:
   if (is.null(x) | (length(x) == 0)){
      x <- as.numeric(rownames(y))
      if (any(is.na(x))) x <- 1:dim(y)[1]
   }

   # Define 'labels' as an integer sequence if undefined:
   if (is.null(labels)){
      if (!is.null(x)) labels <- as.character(x) else labels <- rownames(y)
   }

   # If 'x' is a character vector, define 'labels' as 'x':
   if (is.character(x)){
      labels <- x
      x <- 1:length(labels)
   }

   # Check if 'x' is a vector:
   if (!is.vector(x)) stop("'x' must be a vector.")

   # Check that 'x' and 'labels' are the same length:
   if (length(x) != length(labels)) stop("'x' and 'labels' must be the same length.")

   # Order data by values of 'x':
   index <- order(x)
   x <- x[index]
   y <- y[index, , drop = FALSE]
   labels <- labels[index]

   # Define y axis limits:
   ylim <- c(NA, NA)
   temp <- y
   temp[y > 0] <- 0
   ylim[1] <- min(apply(temp, 1, sum, na.rm = TRUE))
   temp <- y
   temp[y < 0] <- 0
   ylim[2] <- max(apply(temp, 1, sum, na.rm = TRUE))
   if (yaxs == "i"){
      if (all(y[!is.na(y)] < 0)) ylim[1] <- 1.04 * ylim[1]
      if (all(y[!is.na(y)] > 0)) ylim[2] <- 1.04 * ylim[2]
   }

   # Create blank axes:
   if (!add){
      plot(c(min(x) - width/2, max(x) + width/2), ylim, type = "n", ann = FALSE, xaxt = "n", yaxs = yaxs, ...)
      if (all(as.character(x) == labels)) axis(1, ...)
      else axis(1, at = x, labels = labels, ...)
   }

   # Plot figure title:
   title(...)

   # Loop over each bar:
   for (i in 1:length(x)){
      y.lower <- 0
      y.upper <- 0

      # Draw negative bars:
      index <- which(!(is.na(y[i, ])) & (y[i, ] < 0))
      if (length(index) > 0){
         for (j in 1:length(index)){
            xx <- c(x[i] - width / 2, x[i] - width / 2, x[i] + width / 2, x[i] + width / 2, x[i] - width / 2)
            yy <- c(y.lower, y.lower + y[i, index[j]], y.lower + y[i, index[j]], y.lower, y.lower)
            polygon(xx, yy, col = col[index[j]], border = border[index[j]], ...)
            y.lower <- y.lower + y[i, index[j]]
         }
      }

      # Draw positive bars:
      index <- which(!(is.na(y[i, ])) & (y[i, ] >= 0))
      if (length(index) > 0){
         for (j in 1:length(index)){
            xx <- c(x[i] - width / 2, x[i] - width / 2, x[i] + width / 2, x[i] + width / 2, x[i] - width / 2)
            yy <- c(y.upper, y.upper + y[i, index[j]], y.upper + y[i, index[j]], y.upper, y.upper)
            polygon(xx, yy, col = col[index[j]], border = border[index[j]], ...)
            y.upper <- y.upper + y[i, index[j]]
         }
      }
   }

   # Draw legend:
   if (legend & (dim(y)[2] > 1)){
      legend("topleft", fill = col, legend = names(y))
   }

   invisible(col)
}
