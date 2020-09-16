#' Calculate elapsed time
#'
#' @name tictoc
#'
#' @description Calculates elapsed time between calls to \code{tic} and \code{toc}.
#'
#' @param gcFirst Logical value.
#'
#' @examples
#' tic()
#'    for (i in 1:1000000) x <- i
#' toc()
#'
#' @section Functions:
#' \describe{
#'    \item{\code{tic}}{Start timer.}
#'    \item{\code{toc}}{Stop timer and report elapsed time.}
#' }

#' @rdname tictoc
#' @export tic
tic <- function(gcFirst = TRUE){
   assign(".type", "elapsed", envir = baseenv())
   if (gcFirst) gc(FALSE)
   tic <- proc.time()["elapsed"]
   assign(".tic", tic, envir = baseenv())
   invisible(tic)
}

#' @rdname tictoc
#' @export toc
toc <- function(){
   type <- get(".type", envir = baseenv())
   toc <- proc.time()[type]
   tic <- get(".tic", envir = baseenv())
   print(toc - tic)
   invisible(toc)
}
