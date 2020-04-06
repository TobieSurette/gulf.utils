excel.default <- function(x, ...){
   # EXCEL.DEFAULT - Send data to an Excel spreadsheet.

   y <- as.data.frame(x)
   excel(y, ...)
}
