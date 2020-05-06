#' Extract Metadata
#'
#' @description Function to extract metadata from an object.
#'
#' @param x Object.
#' @param ... Other arguments (not used).
#'
#' @seealso \code{\link{key}}, \code{\link{description}}, \code{\link{units}}, \code{\link{fmt}}
#'
metadata <- function(x, ...) UseMethod("metadata")

#' @describeIn metadata Extract metadata for a data frame.
metadata.data.frame <- function(x, ...){

   res <- data.frame(AttributeName = names(x),
                     attributeDefinition = "",
                     formatString = NA,
                     definition = NA,
                     stringsAsFactors = FALSE)



   return(res)
}
