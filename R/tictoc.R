#' Calculate elapsed time
#'
#' @description Calculates elapsed time between calls to \code{tic} and \code{toc}.
#'
#' @aliases tic
#' @aliases toc
#'
#' @param gcFirst Logical value.
#'
#' @examples
#' tic()
#'    for (i in 1:1000000) x <- i
#' toc()
#'
#' @export toc

#' @describeIn tictoc Start timer.
#' @export tic
tic <- function(gcFirst = TRUE){
   assign(".type", "elapsed", envir = baseenv())
   if (gcFirst) gc(FALSE)
   tic <- proc.time()["elapsed"]
   assign(".tic", tic, envir = baseenv())
   invisible(tic)
}

#' @describeIn tictoc Stop timer and report elapsed time.
#' @export toc
toc <- function(){
   type <- get(".type", envir = baseenv())
   toc <- proc.time()[type]
   tic <- get(".tic", envir = baseenv())
   print(toc - tic)
   invisible(toc)
}
