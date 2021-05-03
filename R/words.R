#' Extract Words
#'
#' @description Return a list of words contained in a text.
#'
#' @param x Character vector or an object coercible to a text objexct.
#' @param file Character vector specifying a file to be read.
#' @param ... Further arguments (not used).
#'
#' @examples
#' words("The punctuation, in this phrase; is ... really: 'terrible'.")

#' @export words
words <- function(x, ...) UseMethod("lexicon")

#' @describeIn words Returns a character vector of component words.
#' @export
words.default <- function(x, file, ...) if (!missing(file)) return(lexicon(readLines(file)))

#' @describeIn words Returns a character vector of component words.
#' @export
words.character <- function(x, ...){
   v <- unique(unlist(strsplit(x, "[ ,-.:'();]")))
   v <- v[nchar(v) > 0]
   return(v)
}

#' @export words
lexicon <- words
