#' Extract Lexicon
#'
#' @description Returns the different words used in a an object within text.
#'
#' @param x Character vector or an object coercible to a text objexct.
#' @param file Character vector specifying a file to be read.
#' @param ... Further arguments (not used).
#'
#' @examples
#' lexicon("The punctuation, in this phrase; is ... really: 'terrible'.")

#' @export lexicon
lexicon <- function(x, ...) UseMethod("lexicon")

#' @describeIn lexicon Returns a character vector of component words.
#' @export
lexicon.default <- function(x, file, ...) if (!missing(file)) return(lexicon(readLines(file)))

#' @describeIn lexicon Returns a character vector of component words.
#' @export
lexicon.character <- function(x, ...){
   v <- unique(unlist(strsplit(x, "[ ,-.:'();]")))
   v <- v[nchar(v) > 0]
   return(v)
}
