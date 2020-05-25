#' List Git Repository Files
#'
#' @description This function lists available files and directories on a GitHub repository.
#'
#' @param username GitHub user name.
#' @param repository GitHub repository name.
#'
#' @examples
#' dir.github("TobieSurette", "gulf.utils")
#'
#' @return Character vector of files and directories.
#'
#' @export dir.github
#'
dir.github <- function(username, repository){
   http <- paste0("https://api.github.com/repos/", username, "/", repository, "/git/trees/master?recursive=1")
   req <- httr::GET(http)
   httr::stop_for_status(req)
   files <- unlist(lapply(httr::content(req)$tree, "[", "path"), use.names = F)
   return(files)
}
