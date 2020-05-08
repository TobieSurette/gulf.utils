#' Retrieve Metadata
#'
#' @description Function to retrieve metadata from an object.
#'
#' @param x Object.
#' @param ... Other arguments (not used).
#'
#' @examples
#' # Define sample data set:
#' x <- data.frame(year = 2010:2014,
#'  site = letters[1:5],
#'  weight = exp(rnorm(5)),
#'  number = rpois(5,5))
#'
#' # Define metadata attributes:
#' units(x, "weight") <- "kilogram"
#' description(x) <- c(year = "Year of sampling",
#'  site = "Sampling site",
#'  weight = "Weight of specimen",
#'  number = "Number observed")
#' fmt(x, "year") <- "YYYY"
#' key(x) <- "year"
#'
#' # Retrieve metadata:
#' metadata(x)
#'
#' @seealso \code{\link{key}}, \code{\link{description}}, \code{\link{units}}, \code{\link{fmt}}
#'
#' @export metadata
#' @export metadata.data.frame
#'
metadata <- function(x, ...) UseMethod("metadata")

#' @describeIn metadata Extract metadata for a data frame.
#' @export metadata.data.frame
metadata.data.frame <- function(x, ...){
   # Initialize output:
   res <- data.frame(attributeName = names(x),
                     attributeDefinition = "",
                     formatString = NA,
                     definition = "",
                     unit = NA,
                     numberType = NA,
                     stringsAsFactors = FALSE)

   # Extract attributes:
   res$attributeDefinition[match(names(attr(x, "description")), res$attributeName)] <- as.vector(attr(x, "description"))
   res$unit[match(names(attr(x, "units")), res$attributeName)] <- as.vector(attr(x, "units"))
   res$formatString[match(names(attr(x, "fmt")), res$attributeName)] <- as.vector(attr(x, "fmt"))

   return(res)
}
