#' Language Options
#'
#' @description Function to standardize language options.
#'
#' @param x Character strings.
#'
#' @examples
#' language()
#'
#' language("f")     # Returns 'french'.
#' language("franc") # Returns 'french'.
#' language("ang")   # Returns 'english'.
#' language("lat")   # Returns 'latin'.
#'
#' # Messy inputs:
#' language(c("en", "ENgli", "fr", "francais", "fr", "lat"))

#' @export
language <- function(x){
   options <- c("french", "francais", "français", "english", "anglais", "latin", "bilingual", "bilingue")
   if (missing(x)) return(options)
   ux <- unique(x)
   index <- match(x, ux)
   for (i in 1:length(ux)) ux[i] <- options[grep(paste0("^", tolower(ux[i])), options)[1]]
   ux[ux %in% c("francais", "français")] <- "french"
   ux[ux %in% c("bilingue")] <- "bilingual"
   ux[ux %in% c("anglais")] <- "english"
   x <- ux[index]
   return(x)
}
