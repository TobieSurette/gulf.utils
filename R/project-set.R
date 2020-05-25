#' Assign a Project Identifier
#'
#' @description Assigns a project identifier to an object.
#'
#' @param x Target object.
#' @param ... Other arguments (not used).
#' @param value Character string(s) specifying the project identifier to be assigned.
#'
#' @details The \code{project} attribute may be erased by assigning a \code{NULL} value to it.
#'
#' @return An object with a \code{project} attribute attached to it.
#'
#' @examples
#' x <- data.frame(year = 2010:2014, measurement = rnorm(5))
#' project(x) <- "test.survey"
#'
#' # Show attributes:
#' attributes(x)
#'
#' # Erase 'project' attribute:
#' project(x) <- NULL          # Remove all projects.
#'
#' @export "project<-"
#' @export "project<-.default"
#' @export "project"
#' @export "project.default"
#'
#' @seealso \code{\link{metadata}}, \code{\link{key}}, \code{\link{description}}, \code{\link{units}}, \code{\link{fmt}}
#'
"project<-" <- function(x, ...) UseMethod("project<-")

#' @describeIn project-set Default 'project' assignment method.
"project<-.default" <- function(x, value, ...){
   if (missing(y)){
      # Extract attributes fields from 'value':
      if (!is.null(names(value))){
         project(x, names(value)) <- value
         return(x)
      }
      # Assign attribute:
      attr(x, "project") <- value
   }

   return(x)
}

#' @describeIn project-set Generic 'project' extraction method.
project <- function(x, ...) UseMethod("project")

#' @describeIn project-set Default 'project' extraction method.
project.default <- function(x, ...) return(attr(x, "project"))
