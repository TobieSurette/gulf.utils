#' Describe Contents
#'
#' @description These functions provide a summary description of the contents of different types of R objects.
#' Unique elements are shown and sorted so that the minimum and maximum values are visible. For
#' integer values, ranges are shown. For data frames, the total number of NA or missing
#' values are also shown, along with the contents of each variable field.
#'
#' @param x Data frame.
#' @param digits Number of significant digits to display for real numbers (default = 3).
#' @param max Maximum number of different elements to display (default = 10).
#' @param sep Character string specifying the separator to be used in concatenating unique values.
#' @param indent Number of spaces to indent field variables when displaying a data frame (default = 3).
#' @param drop Logical value specifying whether to ignore empty variables in the output.
#' @param ... Other arguments (not used).

#' @examples
#' # Integer examples:
#' describe(1:10)
#' describe(seq(2, 10, by = 2))
#' describe(seq(2, 40, by = 2))
#'
#' # Decimal numeric examples:
#' describe(rnorm(10))
#' describe(rnorm(20))
#'
#' # Character string examples:
#' describe(LETTERS[1:5])
#' describe(LETTERS)
#'
#' # Create data frame
#' x <- data.frame(categories = LETTERS[1:5],
#'                 n = rpois(5, lambda=1),
#'                 values = rnorm(5))
#' describe(x)
#'
#' # Bigger example:
#'x <- data.frame(categories = factor(sample(LETTERS[1:5], 25, replace = TRUE),
#'                levels = LETTERS[1:5]),
#'                n = rpois(25, lambda = 10),
#'                values = sample(c(rnorm(20), rep(NA, 5))),
#'                result = runif(25) > 0.3,
#'                missing = NA,
#'                stringsAsFactors = FALSE)
#' describe(x)
#'
#' @export describe
#'
describe <- function(x, ...) UseMethod("describe")

#' @describeIn describe Summary contents of a logical vector.
describe.logical <- function(x, ...){
   ux <- sort(unique(x[!is.na(x)]))
   return(paste0("{", paste0(ux, collapse = ", "), "}"))
}

#' @describeIn describe Summary contents of a numeric vector.
describe.numeric <- function(x, digits = 3, max = 10, sep = "-", ...){
   ux <- sort(unique(x[!is.na(x)]))
   if (length(ux) == 0) return("{}")
   if (any(ux < 0)) sep = " to "
   if (length(ux) == 1) return(paste0("{", ux,"}"))
   if (length(ux) == 2) return(paste0("{", ux[1], ", ", ux[2], "}"))
   if (all((ux %% 1) == 0)){
      # Integers:
      du <- diff(ux)
      index <- which(du > 1)
      if (length(index) == 0) return(paste0("{", min(ux), sep, max(ux), "}"))
      if (length(index) > max) return(paste0("{", min(ux), ",...,", max(ux), "}"))
      if (ux[1] == ux[index[1]]) v <- ux[1] else v <- paste0(ux[1], sep, ux[index[1]])
      if (length(index) > 1){
         for (i in 1:(length(index)-1)) if (ux[index[i]+1] == ux[index[i+1]]) v[i+1] <- ux[index[i]+1] else v[i+1] <- paste0(ux[index[i]+1], sep, ux[index[i+1]])
      }
      if (ux[index[length(index)]+1] == ux[length(ux)]) v[length(index)+1] <-  ux[length(ux)] else v[length(index)+1] <- paste0(ux[index[length(index)]+1], sep, ux[length(ux)])
      v <- paste(v, collapse = ", ")
   }else{
      # Real numbers:
      ux <- format(ux, digits = digits)
      ux <- gsub(" ", "", ux)
      if (length(ux) > max){
         v <- paste0(c(ux[1:2], "...", utils::tail(ux, 2)), collapse = ", ")
      }else{
         v <- paste0(ux, collapse = ", ")
      }
   }

   # Add brackets:
   v <- paste0("{", v, "}")

   return(v)
}

#' @describeIn describe Summary contents of a character vector.
describe.character <- function(x, max = 10, sep = "', '", ...){
   ux <- sort(unique(x[!is.na(x) & (x != "")]))
   if (length(ux) > max){
      v <- paste(c(ux[1:2], "...", utils::tail(ux, 2)), collapse = sep)
   }else{
      v <- paste(ux, collapse = sep)
   }

   # Add brackets:
   v <- paste0("{'", v, "'}")

   return(v)
}

#' @describeIn describe Summary contents of a factor.
describe.factor <- function(x, max = 10, sep = ", ", ...){
   ux <- sort(unique(x[!is.na(x)]))
   if (length(ux) > max){
      v <- paste(c(ux[1:2], "...", utils::tail(ux, 2)), collapse = sep)
   }else{
      v <- paste(ux, collapse = sep)
   }

   # Add brackets:
   v <- paste0("{", v, "}")

   return(v)
}

#' @describeIn describe Summary contents of a data frame.
describe.data.frame <- function(x, indent = 3, drop = FALSE, ...){
   cat("\n")
   cat(paste(paste(dim(x), collapse = " x "), "data frame:\n"))
   fields <- names(x)
   for (i in 1:ncol(x)){
      spaces <- paste(rep(" ", indent + max(nchar(fields)) - nchar(fields[i])), collapse = "")

      # Summary string function:
      na.number <- sum(is.na(x[, i]))

      # String indicating data type:
      class <- "   "
      if (typeof(x[, i]) == "logical")   class <- "log"
      if (typeof(x[, i]) == "integer")   class <- "int"
      if (typeof(x[, i]) == "double")    class <- "num"
      if (typeof(x[, i]) == "character") class <- "chr"
      if (is.factor(x[, i]))             class <- "fac"

      # Summary of contents:
      v <- describe(x[, i], ...)

      # Append data type:
      v <- paste0(class, " : ", v)

      # Append number of empty values:
      if (class == "chr") na.number <- sum(is.na(x[, i]) | gsub(" ", "", x[, i]) == "") # Empty strings.
      if ((na.number > 0) & (na.number < length(x[,i]))) v <- paste0(v, " (", na.number, " empty)")
      if (na.number == nrow(x)) v <- paste0(v, " (empty)")

      if (!drop | (na.number != nrow(x))) cat(paste0(spaces, "'", names(x)[i], "' : ", v, "\n"))
   }
   cat("\n")
}

