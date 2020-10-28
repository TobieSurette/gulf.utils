#' Export data frame to Excel
#'
#' @description This function dumps a data frame into a temporary tab-delimited table and opens an Excel session for the file.
#'
#' @aliases excel excel.default excel.data.frame
#'
#' @param x Data frame.
#' @param header Logical value specifying whether data frame column names should be exported.
#' @param row.names Logical value specifying whether data frame row names should be exported.
#' @param ... Further arguments passed onto the \code{\link[utils]{write.table}} function.
#'
#' @examples
#' # Send a vector to Excel:
#' excel(1:10)
#'
#' # Send a 25 x 4 matrix of random numbers to Excel, with no headers:
#' x <- matrix(runif(100), ncol = 4)
#' excel(x, header = FALSE)

#' @export
excel <- function(x, ...) UseMethod("excel")

#' @describeIn excel Default 'excel' method.
#' @export
excel.default <- function(x, ...) return(excel(as.data.frame(x), ...))

#' @describeIn excel Export data frame to MS Excel.
#' @export
excel.data.frame <- function(x, header = TRUE, row.names = FALSE, ...){
   # Define temporary file name:
   file <- paste(tempfile(tmpdir = tempdir()), ".txt", sep = "")

   # Write data frame 'x':
   utils::write.table(x, sep = "\t", file = file, col.names = header, row.names = row.names, ...)

   # Call Excel:
   if (.Platform$OS.type == "unix") command <- paste0("open ", file, ' -a "Microsoft Excel"')
   if (.Platform$OS.type != "unix") command <- paste0("start excel '", file, '" /e')

   # Excel call:
   b <- shell(paste0('start excel "', file, '" /e'), wait = TRUE)

   # Removes file from temporary directory:
   #on.exit(unlink(file))
}
