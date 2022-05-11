#' Correct Spelling
#'
#' @description Functions for correcting common fishery and biological spelling errors.
#'
#' @param x Character string(s).
#'
#' @examples
#' spelling("aligatore")
#' spelling("yelow  tail")

#' @export
spelling <- function(x){
   # Load spelling table:
   tab <- utils::read.table(locate(package = "gulf.utils", file = "spelling.txt"), header = TRUE, sep = "\t", stringsAsFactors = FALSE)

   # Perform corrections:
   for (i in 1:nrow(tab)) x <- gsub(tab$expression[i], tab$correction[i], x)

   return(x)
}
