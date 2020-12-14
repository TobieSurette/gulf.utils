#' Import Data
#' 
#' @description Import from one object into another.
#' 
#' @param x Target object.
#' @param value Object containing catch data to be assigned.
#' @param by Character string(s) specifying which variables use as an index key.
#' @param variables Character string(s) specifying the fields to import from the source object.
#' @param fill.na Value to be used to fill in target field entries with no matching source entries.
#' @param ... Other parameters (not used).
#' 
#' @seealso \code{\link[gulf.utils]{match}}
#' 
#' @examples 
#' x <- read.scsset(year = 2020, valid = 1)
#' b <- read.scsbio(2020)
#' import(x, fill = 0) <- catch(b, category = c("M", "COM", "MF"))
#' print(x$M)

#' @export
"import<-" <- function(x, ...) UseMethod("import<-")

#' @describeIn import-set Import assignment method for data frames.
#' @export
"import<-.data.frame" <- function(x, value, variables, fill.na, ...){
   if (missing(variables)) variables <- names(value)
   
   # Remove indexing variables:
   variables <- setdiff(variables, list(...)$by)
   variables <- setdiff(variables, list(...)$key)
   variables <- setdiff(variables, key(value))
   variables <- setdiff(variables, key(x))
   variables <- variables[variables %in% names(value)]
   if (length(variables) == 0) stop("No matching variables to import from source object.") 

   # Append results:
   if (length(variables) > 0){
      ix <- gulf.utils::match(x, value, ...)
      x[variables] <- NA
      x[variables] <- value[ix, variables]
   }

   # Fill NA values:
   if (!missing(fill.na)){
      tmp <- x[variables]
      tmp[is.na(tmp)] <- fill.na
      x[variables] <- tmp
   }
   
   return(x)
}
