#' @title Remove Accented Characters
#'
#' @description Replace french characters with unaccented versions.
#'
#' @param x Character string(s) where accented characters will be replaced by their unaccented versions.

#' @export
unaccent <- function(x){
   x <- gsub("\xbc", "0.25", x)
   x <- gsub("\xfb", "u", x)
   x <- gsub("\xe0", "a", x)
   x <- gsub("\xea", "e", x)
   x <- gsub("\xe2", "a", x)
   x <- gsub("\x82", "e", x)
   x <- gsub("<82>", "e", x)
   x <- gsub("\x8a", "e", x)
   x <- gsub("\x8e", "e", x)
   x <- gsub("\x8f", "e", x)
   x <- gsub("\xe9", "e", x)
   x <- gsub("\xc9", "e", x)
   x <- gsub("\x9c", "oe", x)
   x <- gsub("\xe8", "e", x)
   x <- gsub("_œ", "oe", x)
   x <- gsub("œ", "oe", x)
   x <- gsub("\xcf", "oe", x)
   x <- gsub("[éè]", "e", x)
   return(x)
}

