#' Downloads a folder and its files from Github
#' 
#' The function downloads all the files from the Github repository into a
#' folder with the same name as the subfolder in the online repository
#' 
#' @param directory A character string giving the full name of the Github
#' folder with the theme to be downloaded.
#' @param user A character string giving the user 
#' @param repo A character string giving the repository
#' @param branch A character string giving the branch in the repository
#' @param file_extensions A character vector with file extensions (excl. '.',
#' e.g. c("R", "Rmd"))
#' 
#' @return NULL
#' 
#' @export
download_from_github <- function(directory,
                                 user, 
                                 repo,
                                 branch,
                                 file_extensions = "") {
  # Check if the folder already exists
  if (dir.exists(file.path(getwd(), directory))) {
    stop(paste0(directory, " has already been downloaded"))
  }
  
  # Get the complete list of files from the Github repository
  file_list_repo <- github_repo_file_list(user, repo, branch)
  
  # Create the regular expression for the file extensions to keep
  file_extenstions_regex <- make_file_extensions_regex(file_extensions)
  
  # Subset to only include the list of files associated with the current theme
  file_list <- grep(paste0(directory, file_extenstions_regex),
                    file_list_repo,
                    value = TRUE,
                    fixed = FALSE)
  
  # Create the directory structure
  create_dir_structure(file_list)
  
  # Download the files
  invisible(github_download_files(file_list, user, repo, branch))
}

#' Get updated files from a Github folder
#'
#' Gets the latest files from the specified directory in a user's repository
#' for a specific branch. It will download the specified folder, replace all
#' files in the current working directory corresponding to the files in the
#' repository without warning and then clean up.
#' 
#' The intention behind the function is to have an easy way to update in 
#' particular templates and model examples in existing projects to the newest 
#' version on Github.
#' 
#' This function is exported for convenience, but it is recommended to use the
#' wrapper functions update_*.
#' 
#' WARNING: If you have made local changes to the files in your project, then
#' these will be lost when you run the function. This is also true for the 
#' update_* functions.
#' 
#' @inheritParams download_from_github
#' 
#' @return NULL
#' 
#' @export
update_from_github <- function(directory,
                               user,
                               repo,
                               branch,
                               file_extensions = "") {
  # List all files in the current working directory and sub-directories
  all_files <- list.files(recursive = TRUE)
  
  # Download files (after to avoid also counting these)
  invisible(download_from_github(directory,
                                 user,
                                 repo,
                                 branch,
                                 file_extensions))

  # Get the list of files in the beamer folder
  file_list <- list.files(directory, recursive = TRUE)
  for (i in seq_along(file_list)) {
    string <- file_list[[i]]
    to_replace <- grep(string, all_files, value = TRUE)
    file_path <- file.path(directory, string)
    file.copy(file_path, to_replace, overwrite = TRUE)
  }
  
  # Clean up
  unlink(file.path(getwd(), directory), recursive = TRUE, force = FALSE)
}

#' Get the list of files in the Github repository
#'
#' @inheritParams download_from_github
#' 
#' @return A vector with the list of files contained in the repository
#' 
#' @export
github_repo_file_list <- function(user, 
                                  repo,
                                  branch) {
  # Define the API url
  url_repo <- paste(
    "https://api.github.com/repos",
    user,
    repo,
    "git/trees",
    branch,
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
#' @inheritParams download_from_github
#' 
#' @return A character string with the default branch
#' 
#' @export
github_default_branch <- function(user,
                                  repo) {
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
#' @inheritParams download_from_github
#' 
#' @return A vector with the list of API information
#' 
#' @export
github_api_info <- function(user, 
                            repo) {
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

#' Download Github Director
#'
#' @param file_list A vector of files 
#' @inheritParams download_from_github
#' 
#' @return NULL
github_download_files <- function(file_list,
                                  user,
                                  repo,
                                  branch) {
  # Create the source paths
  source_path <- paste(
    "https://github.com",
    user,
    repo,
    "raw",
    branch,
    file_list,
    sep = "/"
  )
  
  # Create the destination paths - NOTE: No checks yet.
  destination_path <- file.path(getwd(), file_list)
  
  # Loop over elements in the source path
  for (i in seq_along(source_path)) {
    utils::download.file(source_path[[i]], destination_path[[i]])
  }
  
  # No return from the function
  return(NULL)
}
