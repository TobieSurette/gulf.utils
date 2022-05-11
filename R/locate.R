#' Find Data Files
#'
#' @description Searches for the path(s) and name of package data file(s).
#'
#' @param x Not used.
#' @param file Character string(s) specifying the files, including regular expressions, to search for in the file name.
#' @param path Character string specifying the file path to search.
#' @param package Character string specifying the package name.
#' @param keywords Character string specifying keywords that the path and file names must contain.
#' @param full.names Logical value specifying whether to return the path names along with the file names.
#' @param ... Not used.
#'
#' @examples
#' \dontrun{
#' locate(package = "gulf.data", file = "species.csv")
#' locate(package = "gulf.data", file = "*.csv")
#' }
#'
#' @return Character vector.
#'
#' @seealso \code{\link[base]{grep}}

#' @export locate
locate <- function(x, ...) UseMethod("locate")

#' @describeIn locate Default function to locate data files.
#' @export
locate.default <- function(x, file, path, package, keywords, full.names = TRUE, ...){
   # Define search paths:
   if (missing(path) & missing(package))  path <- getwd()
   if (missing(path) & !missing(package)) path <- find.package(package = package)
   if (missing(path) & missing(file)) return(NULL)
   paths <- dir(path = path, recursive = TRUE, full.names = TRUE, all.files = TRUE)

   # Only look in package data paths:
   if (!missing(package)) paths <- paths[unique(c(grep("/data", paths), grep("/extdata", paths)))]

   # Match search for keywords:
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

   # Match file:
   if (!missing(file)){
      file <- tolower(file)
      if (length(file) > 0) for (i in 1:length(file)) f <- f[grep(file[i], tolower(f$file)), ]
   }

   if (full.names) return(f$path) else return(f$file)
}





