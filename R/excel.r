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
#'
#' # Send a vector to Excel:
#' excel(1:10)
#'
#' # Send a 25 x 4 matrix of random numbers to Excel, with no headers:
#' x <- matrix(runif(100), ncol = 4)
#' excel(x, header = FALSE)
#'

#' @export excel
excel <- function(x, ...) UseMethod("excel")

#' @describeIn excel Default key method.
#' @export
excel.default <- function(x, ...) return(excel(as.data.frame(x), ...))

#' @describeIn excel Export data frame to MS Excel.
#' @export
excel.data.frame <- function(x, header = TRUE, row.names = FALSE, ...){
   # EXCEL.DATA.FRAME - Send data frame to an Excel spreadsheet.

   # Define temporary file name:
   file <- paste(base::tempfile(tmpdir = base::tempdir()), ".txt", sep = "")

   # Write data frame 'x':
   utils::write.table(x, sep = "\t", file = file, col.names = header, row.names = row.names, ...)

   # Call Excel:
   if (.Platform$OS.type == "unix") command <- paste0("open ", file, ' -a "Microsoft Excel"')
   if (.Platform$OS.type != "unix") command <- paste0("start excel ", file, '" /e')

   b <- base::system(command, intern = TRUE, wait = TRUE)

   # Remove CSV file from temporary directory:
   #if (file.exists(file)) file.remove(file)
}
