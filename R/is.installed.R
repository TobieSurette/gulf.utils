#' Check Package Installation
#'
#' This function checks whether an R package(s) is installed.
#'
#' @param package.name Name(s) of the packages to be checked.
#' @return Logical value(s).
#'
#' @examples
#'
#' # Check whether there is a 'base' and 'splines' package:
#' is.installed(c("base", "splines"))
#'
#' @export is.installed
#'
is.installed <- function(package.name) is.element(package.name, installed.packages()[,1])
