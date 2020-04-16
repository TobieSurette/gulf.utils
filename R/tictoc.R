#' Calculate elapsed time
#'
#' @description Calculates elapsed time between calls to \code{tic} and \code{toc}.
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

#' @describeIn tic Start timer.
tic <- function(gcFirst = TRUE){
   assign(".type", "elapsed", envir = baseenv())
   if (gcFirst) gc(FALSE)
   tic <- proc.time()["elapsed"]
   assign(".tic", tic, envir = baseenv())
   invisible(tic)
}

#' @describeIn tic Stop timer and report elapsed time.
toc <- function(){
   type <- get(".type", envir = baseenv())
   toc <- proc.time()[type]
   tic <- get(".tic", envir = baseenv())
   print(toc - tic)
   invisible(toc)
}
