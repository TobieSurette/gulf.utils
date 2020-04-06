excel.data.frame <- function(x, header = TRUE, row.names = FALSE, ...){
   # EXCEL.DATA.FRAME - Send data frame to an Excel spreadsheet.

   # Define temporary CSV file name:
   file <- paste(tempfile(tmpdir = tempdir()), ".txt", sep = "")

   # Write data frame 'x' to CSV file:
   write.table(x, sep = "\t", file = file, col.names = header, row.names = row.names, ...)

   # Call Excel to load tab delimited file:
   shell(paste0('start excel "', file, '" /e'), wait = TRUE)
   
   # Remove CSV file from temporary directory:
   #if (file.exists(file)) file.remove(file)
}
