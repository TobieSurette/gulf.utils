#' Install 'gulf' Packages
#'
#' @description Install the suite of gulf packages.
#'
#' @examples
#' install.gulf()

#' @export
install.gulf <- function(){
   # Define packages:
   required <- c("devtools", "sp", "rgdal", "akima")
   gulf <- c("gulf.metadata", "gulf.data", "gulf.graphics", "gulf.spatial", "gulf.stats")
   installed <- rownames(installed.packages())

   # Install gulf package dependencies:
   for (i in 1:length(required)){
      if (!(required[i] %in% installed)) install.packages(required[i], dependencies = TRUE)
   }

   library(devtools)

   for (i in 1:length(gulf)) install_github(paste0("TobieSurette/", gulf[i]))
}
