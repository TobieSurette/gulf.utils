#' Find Data Files
#'
#' @description Searches for the path(s) and name of package data file(s).
#'
#' @param pattern Character string(s) specifying the patterns, including regular expressions, to search for in the file name.
#' @param path Character string specifying the file path to search.
#' @param package Character string specifying the package name.
#' @param full.names Logical value specifying whether to return the path names along with the file names.
#'
#' @examples
#' locate(package = "gulf.data", pattern = "species.csv")
#' locate(package = "gulf.data", pattern = "*.csv")
#'
#' @return Character vector.
#'
#' @seealso \code{\link[base]{grep}}
#'
#' @export locate
#'
#'
locate <- function(x, ...) UseMethod("locate")

#' @describeIn locate Default function to locate data files.
locate.default <- function(x, pattern, path, package, keywords, full.names = TRUE, ...){
   # Define search paths:
   if (missing(path) & missing(package))  path <- getwd()
   if (missing(path) & !missing(package)) path <- find.package(package = package)
   if (missing(path) & missing(pattern)) return(NULL)
   paths <- dir(path = path, recursive = TRUE, full.names = TRUE, all.files = TRUE)

   # Only look in package data paths:
   if (!missing(package)) paths <- paths[unique(c(grep("/data", paths), grep("/extdata", paths)))]

   # Match pattern:
   if ((length(paths) > 0) & !missing(keywords)){
      keywords <- tolower(keywords)
      if (length(keywords) > 0) for (i in 1:length(keywords)) paths <- paths[grep(keywords[i], tolower(paths)) ]
   }

   # Path check:
   if (length(paths) == 0) return(NULL)

   # Extract file list:
   files <- unlist(lapply(strsplit(paths, "/"), function(x) x[length(x)]))

   # Create data frame:
   f <- data.frame(path = paths, file = files, stringsAsFactors = FALSE)

   # Match pattern:
   if (!missing(pattern)){
      pattern <- tolower(pattern)
      if (length(pattern) > 0) for (i in 1:length(pattern)) f <- f[grep(pattern[i], tolower(f$file)), ]
   }

   if (full.names) return(f$path) else return(f$file)
}
