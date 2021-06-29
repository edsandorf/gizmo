#' Get the list of files in the Github repository
#'
#' @param user A character string giving the user 
#' @param repo A character string giving the repository
#'
#' @return A vector with the list of files contained in the repository
#' 
#' @export
github_repo_file_list <- function(user, repo) {
  # Get the Github default branch
  default_branch <- github_default_branch(user, repo)
  
  # Define the API url
  url_repo <- paste(
    "https://api.github.com/repos",
    user,
    repo,
    "git/trees",
    default_branch,
    sep = "/"
  )
  
  url_repo <- paste0(url_repo, "?recursive=1")
  
  request <- httr::GET(url_repo)
  httr::stop_for_status(request)
  file_list <- unlist(lapply(httr::content(request)$tree, "[", "path"),
                      use.names = FALSE)
  
  return(file_list)
}

#' Get the default branch of a repository
#' 
#' @inheritParams github_repo_file_list
#' 
#' @return A character string with the default branch
#' 
#' @export
github_default_branch <- function(user, repo) {
  api_info <- github_api_info(user, repo)
  string <- api_info[[grep("default_branch", api_info)]]
  default_branch <- unlist(
    stringr::str_extract_all(string, stringr::boundary("word"))
  )[2L]
  
  return(default_branch)
}

#' Get Github API information
#'
#' The function gets the API information for a given user and Github repository.
#' 
#' @inheritParams github_repo_file_list
#' 
#' @return A vector with the list of API information
#' 
#' @export
github_api_info <- function(user, repo) {
  url_repo <- paste("https://api.github.com/repos",
                    user,
                    repo,
                    sep = "/")
  
  command <- paste0("curl -s '",
                    url_repo,
                    "'")
  
  api_info <- shell(command, intern = TRUE)
  return(api_info)
}
