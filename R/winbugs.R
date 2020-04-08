winbugs <- function(..., digits, print = TRUE, sep = ","){
   # WINBUGS - Converts object(s) to WinBUGS data format.

   # Extract argument names:  
   args <- as.list(substitute(list(...)))
   args <- args[-1]
   index <- (unlist(lapply(args, class)) == "name")
   argnames <- rep("", length(args))
   for (i in 1:length(args)){
      if (index[i]){
         argnames[i] <- as.character(args[[i]])
      }
   }
   
   # Extract argument values:
   args <- list(...) 
   if ((length(args) == 1) & is.list(args[[1]])) args <- args[[1]]
   temp <- names(args)
   argnames[temp != ""] <- temp[temp != ""]
   
   S <- rep("", length(args))
   for (i in 1:length(args)){
      X <- args[[i]]

      if (!is.numeric(X)) stop("Arguments must be numeric.")
      
      # Round data to appropriate digits:
      if (!missing(digits)) X <- round(X, digits)

      # Convert vector to WinBUGS format:      
      if (is.vector(X) & (length(X) == 0)) S[i] <- ""
      if (is.vector(X) & (length(X) == 1)){
         S[i] <- paste(X)
         if (argnames[i] != "") S[i] <- paste0(argnames[i], " = ", S[i])
         cat(S[i], "\n")
      }
      if (is.vector(X) & (length(X) > 1)){
         S[i] <- "c("
         if (is.character(X)) quote <- "'" else quote <- ""
         for (j in 1:(length(X)-1)){
            if (is.na(X[j])){
               S[i] <- paste0(S[i], "NA", ",")
            }else{
               S[i] <- paste0(S[i], quote, X[j], quote, ",")
            }
         }
         if (is.na(X[j+1])){
            S[i] <- paste(S[i], "NA", ")", sep = "")
         }else{
            S[i] <- paste(S[i], quote, X[j+1], quote, ")", sep = "")
         }
         if (argnames[i] != "") S[i] <- paste0(argnames[i], " = ", S[i])
         if (print){
            cat(paste0(S[i], ifelse(i < length(args), sep, ""), "\n"))
         }
      }
      
      # Convert data frame object to matrix:
      if (is.data.frame(X)) X <- as.matrix(X)
 
      # Convert matrix to S format:
      if (is.matrix(X)){
         S[i] <- "structure(\n    .Data = c("
         if (argnames[i] != "") S[i] <- paste0(argnames[i], " = ", S[i])
         if (print) cat(S[i])
         dim.str <- winbugs(dim(X), print = FALSE)
         width <- min(6, max(nchar(as.character(as.vector(X)))))
         flag <- all((X %% 1) == 0)
         for (j in 1:nrow(X)){
            if (j > 1) if (print) cat("              ")
            for (k in 1:ncol(X)){
               if ((!is.na(X[j, k])) & ((X[j, k] %% 1) == 0) & (missing(digits) | flag)){
                  if ((j == nrow(X)) & (k == ncol(X))) str <- "), " else str <- ", " 
                  if (print) cat(paste0(as.character(X[j, k]), str))
               }else{
                  if ((j == nrow(X)) & (k == ncol(X))) str <- "), " else str <- ", "
                  if (print) cat(formatC(X[j, k], format = "f", width = width, digits = digits), str, sep = "")
               }
            }
            if (print) cat("\n")
         }
         S[i] <- paste(S[i], winbugs(as.vector(t(X)), print = FALSE), ", .Dim = ", dim.str, ")", sep = "")
         if (print) cat(paste("    .Dim = ", dim.str, ")", ifelse(i < length(args), sep, ""),"\n", sep = ""))
      }
   }
   
   invisible(S)
}
