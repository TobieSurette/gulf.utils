#' @title Translate Words
#'
#' @name translate
#'
#' @description Functions for translating common fishery and biological words and terms. These
#'              functions are intended for translating words and common short phrases only.
#'
#' @param x Character string(s).
#' @param plural Logical value specifying whether the plural form of a word is to be imposed.
#' @param feminine Logical value specifying whether the feminine form of a word is to be imposed.
#'
#' @examples
#' en2fr("male")
#' en2fr("crab")
#' en2fr("black")
#' en2fr("missing legs")
#' en2fr("fishing grids")
#' en2fr("survey")

#' @export
en2fr <- function(x, plural = FALSE, feminine = FALSE){
   # Load dictionary:
   tab <- utils::read.csv(locate(package = "gulf.utils", file = "dictionnary.csv"), header = TRUE, stringsAsFactors = FALSE)

   # Perform substitutions:
   for (i in 1:nrow(tab)) x <- gsub(tab$english[i], tab$french[i], x)

   return(x)
}

#' @rdname translate
#' @export
fr2en <- function(x, plural = FALSE, feminine = FALSE){
   # Load dictionary:
   tab <- utils::read.csv(locate(package = "gulf.utils", file = "dictionnary.csv"), header = TRUE, stringsAsFactors = FALSE)

   # Perform substitutions:
   for (i in 1:nrow(tab)) x <- gsub(tab$french[i], tab$english[i], x)

   return(x)
}
