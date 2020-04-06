dbarplot <- function(y, x = NULL, labels = NULL, width = 0.8, width.delta = min(diff(x)),
                     col = NULL, border = par("fg"), add = FALSE,
                     yaxs = ifelse(all(y[!is.na(y)] <= 0)|all(y[!is.na(y)] >= 0), "i", "r"),
                     legend = TRUE, ...){
   # DBARPLOT - Doug's bar plot function which separates negative from positive values.

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

   # Define bar width:
   width <- width * width.delta
   
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
