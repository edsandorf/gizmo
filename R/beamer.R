#' Setup the presentation folder with a Beamer template
#'
#' A wrapper around \code{\link{download_from_github}} to make it easy to
#' download and setup a Beamer presentation within the current project using
#' templates from Github. The function downloads the specified template and
#' renames it to 'presentation'.
#'
#' @inheritParams download_from_github
#'
#' @return NULL
#'
#' @export
setup_beamer <- function(directory = "beamer-nmbu",
                         user = "edsandorf",
                         repo = "latex-templates") {
  # Check if already exists
  if (dir.exists(file.path(getwd(), "presentations"))) {
    stop("Beamer has already been set up for this project. To update the theme
         to the newest version, use: update_beamer()")
  }

  # Get the list of all files in the repository
  default_branch <- github_default_branch(user, repo)
  
  # Define the file extensions
  file_extensions <- c("sty", "eps", "Rmd", "jpg")

  # Download the Beamer template 
  invisible(download_from_github(directory,
                                 user,
                                 repo,
                                 default_branch,
                                 file_extensions))
  
  # Rename folder
  rename_dir(directory, "presentations")
  
  cli::cli_alert_success("Beamer set up")
}

#' Update the Beamer templates in the current project
#'
#' A wrapper around \code{\link{update_from_github}} to make it easy to update
#' Beamer templates in the current project with new and updated templates from
#' Github.
#'
#' @inheritParams download_from_github
#' 
#' @return NULL
#' 
#' @export
update_beamer <- function(directory = "beamer-nmbu",
                          user = "edsandorf",
                          repo = "latex-templates") {
  
  # Get the list of all files in the repository
  default_branch <- github_default_branch(user, repo)
  
  # Define the file extensions
  file_extensions <- c("sty", "eps", "Rmd", "jpg")
  
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
                          not exist. Check that you have run 'setup_beamer' at 
                          least once.")
    unlink(file.path(getwd(), directory), recursive = TRUE, force = FALSE)
  })
  
  cli::cli_alert_success("Beamer templates updated")
}
