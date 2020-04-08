gulf.fprint <- function (text = NULL, file = NULL, path = NULL, overwrite = FALSE, echo  = FALSE, user.info = TRUE){

   # GULF.FPRINT - Log changes to file.

   if(user.info) user.info.text = paste("(", Sys.time() ," - ",Sys.getenv('USERNAME'),") : ", sep="")
   else  user.info.text = ""
   
   # File and path are required 
   if(is.null(file)) stop("'file' must have a value.")
   if(is.null(path)) stop("'path' must have a value.")
   
   # Define full file name to write to 
   f <- paste(path, file, sep = "")

   # Check 'text' argument
   if(is.null(text))
      stop("'text' must have a value.")
   else{
      # Append to existing file
      if (file.exists(f) & overwrite == FALSE){
         write.table(paste(user.info.text, text, sep=""),
                     file = f, append = TRUE, col.names = FALSE, row.names = FALSE, quote = FALSE)
         if(echo)
            cat(paste("Entry added to: '", f , "'\n", sep = ""))
      }else{
         
         if (file.create(file=f, showWarnings = TRUE))
            gulf.fprint(text = text, file = file, path= path, echo = echo, user.info = user.info, overwrite = FALSE)
      }
   }
}
