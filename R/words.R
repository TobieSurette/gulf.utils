#' @title Extract or Count Words
#'
#' @name words
#'
#' @description Return a list of words contained in a text.
#'
#' @param x Character vector or an object coercible to a text objexct.
#' @param file Character vector specifying a file to be read.
#' @param ... Further arguments (not used).
#'
#' @examples
#' words("The punctuation, in this phrase; is ... really: 'terrible'.")
#' nword("The punctuation, in this phrase; is ... really: 'terrible'.")

#' @export words
words <- function(x, ...) UseMethod("words")

#' @rdname words
#' @export
words.default <- function(x, file, ...) if (!missing(file)) return(lexicon(readLines(file)))

#' @rdname words
#' @export
words.character <- function(x, ...){
   v <- unique(unlist(strsplit(x, "[ ,-.:'();]")))
   v <- v[nchar(v) > 0]
   return(v)
}

#' @rdname words
#' @export nword
nword <- function(x, ...) return(length(words(as.character(x), ...)))

#' @rdname words
#' @export lexicon
lexicon <- words
