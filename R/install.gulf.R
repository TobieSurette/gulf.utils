#' @title Install 'gulf' Packages
#'
#' @description Install the suite of gulf packages.
#'
#' @examples
#' \dontrun{
#' install.gulf()
#' }

#' @export install.gulf
install.gulf <- function(){
   # Define packages:
   required <- c("devtools", "sp", "rgdal", "akima")
   gulf <- c("gulf.metadata", "gulf.data", "gulf.graphics", "gulf.spatial", "gulf.stats")
   installed <- rownames(utils::installed.packages())

   # Install gulf package dependencies:
   for (i in 1:length(required)){
      if (!(required[i] %in% installed)) utils::install.packages(required[i], dependencies = TRUE)
   }

   for (i in 1:length(gulf)){
      if (gulf[i] == "gulf.spatial") library(gulf.data)
      devtools::install_github(paste0("TobieSurette/", gulf[i]))
   }
}
