#' Pretty sequence of logarithmic values
#'
#' @description Returns a sequence of values on the logarithmic scale suitable for labelling on a logarithmic plot, i.e.
#' the values are equally spaced on the log-scale.
#'
#' @param x A numeric vector.
#' @param n An integer giving the target number of intervals. Non-integer values are rounded down.
#' @param ... Further arguments passed on to \code{\link[base]{pretty}}.
#'
#' @examples
#' # Exponentail breakpoints across values from 0 to 10:
#' pretty.log(0:10)
#'
#' # Generate exponential breakpoints for random numbers:
#' pretty.log(runif(100)*100)
#'
#' @export "pretty.log"
#'
pretty.log <- function(x, base, n = 5, ...){
   # Expand 'x' if it is too short:
   if (length(x) < 10) x <- seq(min(x), max(x), len = 10)

   # Remove negative values:
   x <- x[x>=0]

   # Expand 'x' if it is too short:
   if (length(x) < 10) x <- seq(min(x), max(x), len = 10)

   # Remove zero values:
   x <- x[x!=0]

   # Candidate logarithmic bases:
   if (missing(base)) base <- c(2:10, 12, 15, 20, 25, 30, 40, 50, 60)

   # Initialize candidate scale:
   scale <- list()

   # Generate set of candidate scales for each logarithmic base:
   for (i in 1:length(base)){
      y <- log(x, base = base[i])
      s <- base[i]^unique(round(pretty(y, n = n))) #, ...)))

      # Remove values lying beyond the data:
      if (length(s) > 1){
         d <- diff(log(c(s[(length(s)-1)], max(x)), base = base[k])) /
              diff(log(s[(length(s)-1):length(s)], base = base[k]))
         if (d < 0.5) s <- s[-length(s)]
      }

      scale[[i]] <- s
   }

   # Mean number of characters by scale:
   nc <- unlist(lapply(lapply(scale, formatC), function(x) mean(nchar(x))))

   # Length of each candidate scale:
   nl <- unlist(lapply(lapply(scale, formatC), length))

   # Select appropriate scale:
   k <- NULL
   for (i in 0:max(abs(nl-n))){
       index <- which(abs(nl-n) == i)
       if (length(index) > 0){
          p <- (1:length(base))[index][which(nc[index] == min(nc[index]))[1]]
          if (is.null(k)){
             k <- p
          }else{
             if ((nc[p] + i) < nc[k]) k <- p
          }
       }
   }

   return(scale[[k]])
}
