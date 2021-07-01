#' Setup models folder
#' 
#' A wrapper around \code{\link{download_from_github}} to make it easy to 
#' download and setup a 'models' folder within the current project using
#' examples from Github. The function downloads the specified examples and
#' renames it to 'models'.
#' 
#' @inheritParams download_from_github
#' 
#' @return NULL
#' 
#' @export
setup_models <- function(directory = "examples",
                         user = "edsandorf",
                         repo = "cmdlr") {
  # Check if already exists
  if (dir.exists(file.path(getwd(), "models"))) {
    stop("models has already been set up for this project. To update with the 
         newest examples, use: update_models()")
  }
  
  # Get the list of all files in the repository
  default_branch <- github_default_branch(user, repo)
  
  # Define the file extensions
  file_extensions <- c("R")
  
  # Download the Beamer template 
  invisible(download_from_github(directory,
                                 user,
                                 repo,
                                 default_branch,
                                 file_extensions))
  
  rename_dir(directory, "models")
  
  cli::cli_alert_success("'models' set up")
  
  return(NULL)
}

#' Update the examples in the models folder
#' 
#' A wrapper around \code{\link{update_from_github}} to make it easy to update
#' model examples in the current project with new and updated examples from
#' Github.
#'
#' @inheritParams download_from_github
#' 
#' @return NULL
#' 
#' @export
update_models <- function(directory = "examples",
                          user = "edsandorf",
                          repo = "cmdlr") {
  
  # Get the list of all files in the repository
  default_branch <- github_default_branch(user, repo)
  
  # Define the file extensions
  file_extensions <- c("R")
  
  # Update the files
  tryCatch({
    update_from_github(directory,
                       user,
                       repo,
                       default_branch,
                       file_extensions) 
  },
  error = function(e) {
    cli::cli_alert_danger("Failed to update templates. Possibly because they do
                          not exist. Check that you have run 'setup_models' at 
                          least once.")
    unlink(file.path(getwd(), directory), recursive = TRUE, force = FALSE)
  })
  
  cli::cli_alert_success("Model templates updated")
  
  return(NULL)
}
