#' Number of Words
#'
#' @description Return the number of words contained in a text.
#'
#' @param x Character vector or an object coercible to a text objexct.
#' @param ... Further arguments (not used).
#'
#' @examples
#' nword("The punctuation, in this phrase; is ... really: 'terrible'.")

#' @export nword
nword <- function(x, ...) return(length(words(as.character(x), ...)))
