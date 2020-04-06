tic <- function(gcFirst = TRUE, type = c("elapsed", "user.self", "sys.self")){
   type <- match.arg(type)
   assign(".type", type, envir=baseenv())
   if(gcFirst) gc(FALSE)
   tic <- proc.time()[type]
   assign(".tic", tic, envir=baseenv())
   invisible(tic)
}

