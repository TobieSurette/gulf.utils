#' Describe Data Frame
#'
#' This function provides a description of the unique and missing elements of a data frame on the R console.
#' Unique elements are sorted so that the minimum and maximum values are visible.
#' The total number of missing and NA values are also shown when the occur.
#'
#' @param x Data frame.
#' @param index Number of indent spaces (default = 3).
#' @param ... Other arguments (not used).
#'
#' @examples
#' # Create data frame
#' x <- data.frame(categories = LETTERS[1:5],
#'                 n = rpois(5),
#'                 values = rnorm(5)))
#'
#' describe(x)
#'
#' # Bigger example:
#'x <- data.frame(categories = LETTERS[1:25],
#'                 n = rpois(25, lambda = 10),
#'                 values = sample(c(rnorm(20), rep(NA, 5))),
#'                 result = runif(25) > 0.3,
#'                 missing = NA)
#'
#' describe(x)
#' @export describe
#' @export describe.data.frame
#'

describe <- function(x, ...) UseMethod("describe")

sumstr <- function(x, digits = 3, max = 10){
   # Summary string function:
   u <- sort(unique(x[!is.na(x)]))
   na.number <- sum(is.na(x))
   if (is.factor(x) | is.character(x)) sep = "', '" else sep = ", "

   # String indicating data type:
   class <- "   "
   if (typeof(x) == "logical")   class <- "log"
   if (typeof(x) == "integer")   class <- "int"
   if (typeof(x) == "double")    class <- "num"
   if (typeof(x) == "character") class <- "chr"
   if (is.factor(x))             class <- "fac"

   if (length(u) == 0){
      v <- ""
   }else{
      if (class == "num") u <- format(u, digits = digits)
      if (length(u) > max){
         v <- paste(c(u[1:2], "...", tail(u, 2)), collapse = sep)
      }else{
         v <- paste(u, collapse = sep)
      }
      if (class %in% c("chr", "fac")) brackets <- c("{'", "'}") else brackets <- c("{", "}")
      v <- paste0(brackets[1], v, brackets[2])
   }

   # Append data type:
   v <- paste0(class, " : ", v)

   # Append number of empty values:
   if (class == "chr") na.number <- na.number + sum(gsub(" ", "", x[!is.na(x)]) == "") # Empty strings.
   if ((na.number > 0) & (na.number < length(x))) v <- paste0(v, " (", na.number, " empty)")
   if (na.number == length(x)) v <- paste0(v, "(empty)")

   return(v)
}

#' @describeIn describe Describe method for data frame objects.
describe.data.frame <- function(x, indent = 3, ...){
   cat("\n")
   cat(paste(paste(dim(x), collapse = " x "), "data frame:\n"))
   fields <- names(x)
   for (i in 1:ncol(x)){
      spaces <- paste(rep(" ", indent + max(nchar(fields)) - nchar(fields[i])), collapse = "")
      cat(paste0(spaces, "'", names(x)[i], "' : ", sumstr(x[,i]), "\n"))
   }
   cat("\n")
}

