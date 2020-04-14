#' Check Package Installation
#'
#' This function checks whether R package are installed.
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
is.installed <- function(package.name) is.element(package, installed.packages()[,1])
