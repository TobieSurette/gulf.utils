#' Build Lexicon
#'
#' @description Returns the different words used in a an object with text.
#'
#' @param x Character vector or an object coercible to a text objexct.
#' @param file Character vector specifying a file to be read.
#'
#' @export lexicon
#' @export lexicon.default
#' @export lexicon.character
#'
lexicon <- function(x, ...) UseMethod("lexicon")

#' @describeIn lexicon Returns a character vector of component words.
lexicon.default <- function(x, file){
   if (!missing(file)) return(lexicon(readLines(file)))
}

#' @describeIn lexicon Returns a character vector of component words.
lexicon.character <- function(x) return(unique(unlist(strsplit(x, " "))))
