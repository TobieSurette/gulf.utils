#' @title Compress or Expand Data Frames
#'
#' @description Function to remove or restore redundant or empty elements from a data object.
#'
#' @param x Data object.
#' @param keep Data object.
#' @param na.rm Logical value specifying whether to remove columns containing only NA values. columns containing only NA values.
#' @param remove Character string(s) specifying other column contents to be treated as empty.
#' @param unique Logical value specifying whether to remove columns that contain only a single unique value.
#' @param ... Not used.
#'
#' @examples
#' x <- data.frame(year = 2018, count = rpois(10, 5), survey = "scs", group = rep(1:2, each = 5))
#' x <- compress(x)
#' print(x)
#' attributes(x)
#' attr(x, "status") <- "critical"
#' expand(x)
#' attr(x, "units") <- c(length = "millimeters", weight = "grams")
#' expand(x)

#' @export
compress <- function(x, ...) UseMethod("compress")

#' @describeIn compress Remove empty data columns.
#' @export
compress.data.frame <- function(x, keep = "", na.rm = TRUE, remove = "", unique = FALSE, ...){
   cols  <- 1:ncol(x)
   a <- list()
   for (i in 1:ncol(x)){
      ix <- rep(FALSE, nrow(x))
      if (na.rm) ix <- ix | is.na(x[,i])
      ix <- ix | (x[,i] %in% remove)
      if (all(ix == TRUE)) cols <- setdiff(cols, i)
      if (unique){
         if (length(unique(x[, i])) == 1){
            cols <- setdiff(cols, i)
            if (!(names(x)[i] %in% keep)) a[[length(a)+1]] <- unique(x[, i])  # Store as attribute.
         }
      }
      if (names(x)[i] %in% keep) cols <- sort(unique(cols, i))
   }

   # Remove columns:
   names <- names(x)
   tmp <- attributes(x)
   tmp <- tmp[setdiff(names(tmp), c("names", "row.names"))]
   x <- x[, cols]
   attributes(x) <- c(attributes(x)[c("names", "row.names")], tmp)  # Restore attributes:

   # Attach unique fields as attributes:
   if (length(a) > 0){
      names(a) <- names[-cols]
      for (i in 1:length(a)) attr(x, names(a)[i]) <- a[[i]]
   }

   return(x)
}

#' @rdname compress
#' @export expand
expand <- function(x, ...) UseMethod("expand")

#' @rdname compress
#' @export
expand.data.frame <- function(x, keep, ...){
   # Loop over attributes:
   a <- attributes(x)
   a <- a[setdiff(names(a), c("names", "class", "row.names"))]
   if (!missing(keep)) a <- a[keep]

   if (length(a) > 0){
      for (i in 1:length(a)){
         if (length(a[[i]]) == 1){
            vars <- names(x)
            x[, ncol(x)+1] <- a[[i]]
            names(x) <- c(vars, names(a[i]))
         }
         if ((length(a[[i]]) > 1) & (length(names(a[[i]])) > 0)){
            for (j in 1:length(a[[i]])){
               vars <- names(x)
               x[, ncol(x)+1] <- a[[i]][j]
               str <- names(a[[i]][j])
               if (length(names(a[i])) == 1) str <- paste0(names(a[i]), ".", str)
               names(x) <- c(vars, str)
            }
         }
      }
   }

   return(x)
}
