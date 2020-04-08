#' Calculate elapsed time
#'
#' @aliases tic toc
#' @param gcFirst Logical value.
#'
#' @examples
#' tic()
#'    for (i in 1:1000000) x <- i
#' toc()
#'
#' @export tic
#' @export toc
#'

tic <- function(gcFirst = TRUE){
   assign(".type", "elapsed", envir = baseenv())
   if (gcFirst) gc(FALSE)
   tic <- proc.time()["elapsed"]
   assign(".tic", tic, envir = baseenv())
   invisible(tic)
}

toc <- function(){
   type <- get(".type", envir = baseenv())
   toc <- proc.time()[type]
   tic <- get(".tic", envir = baseenv())
   print(toc - tic)
   invisible(toc)
}
