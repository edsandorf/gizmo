#' Setup Beamer 
#' 
#' Downloads the correct Beamer template folder from my Github repository and
#' renames it to 'presentation'
#'
#' @param theme A character string giving the name of the Beamer theme
#' @inheritParams github_repo_file_list
#' 
#' @export
setup_beamer <- function(theme = "nmbu",
                         user = "edsandorf",
                         repo = "latex-templates") {
  # Check if already exists
  if (dir.exists(file.path(getwd(), "presentations"))) {
    stop("Beamer has already been set up for this project. To update the theme
         to the newest version, use: update_beamer()")
  }
  
  # Define useful variables
  beamer_theme <- paste0("beamer-", theme)
  
  # Download the Beamer template 
  invisible(download_beamer(beamer_theme, user, repo))
  
  # Rename folder
  rename_dir(beamer_theme, "presentations")
}

#' Updates the Beamer template
#'
#' The function updates the Beamer templates in the current working directory
#' and all sub-directories recursively.
#' 
#' @inheritParams setup_beamer
#' @inheritParams github_repo_file_list
#' 
#' @export
update_beamer <- function(theme = "nmbu",
                          user = "edsandorf",
                          repo = "latex-templates") {
  # Define useful variables
  beamer_theme <- paste0("beamer-", theme)

  # List all files in the current working directory and sub-directories
  all_files <- list.files(recursive = TRUE)
  
  # Download files (after to avoid also counting these)
  invisible(download_beamer(beamer_theme, user, repo))
  
  # Get the list of files in the beamer folder
  theme_files <- list.files(beamer_theme, recursive = TRUE)
  for (i in seq_along(theme_files)) {
    string <- theme_files[[i]]
    to_replace <- grep(string, all_files, value = TRUE)
    file_path <- file.path(beamer_theme, string)
    file.copy(file_path, to_replace, overwrite = TRUE)
  }
  
  # Clean up
  unlink(file.path(getwd(), beamer_theme), recursive = TRUE, force = FALSE)
}

#' Download the Beamer template from Github
#' 
#' The function downloads all the files from the Github repository into a
#' folder with the same name as the subfolder in the online repository
#' 
#' @param beamer_theme A character string giving the full name of the Github
#' folder with the theme to be downloaded.
#' @inheritParams github_repo_file_list
#' 
#' @return NULL
download_beamer <- function(beamer_theme,
                            user, 
                            repo) {
  # Check if the folder already exists
  if (dir.exists(file.path(getwd(), beamer_theme))) {
    stop("The theme has already been downloaded")
  }
  
  # Get the list of all files in the repository
  default_branch <- github_default_branch(user, repo)
  repo_file_list <- github_repo_file_list(user, repo, default_branch)
  
  # Subset to only include the list of files associated with the current theme
  theme_file_list <- grep(paste0(beamer_theme, "/.*\\.(sty|eps|Rmd|jpg)$"),
                          repo_file_list,
                          value = TRUE,
                          fixed = FALSE)
  
  # Create the directory structure
  create_dir(beamer_theme)
  create_dir(paste0(beamer_theme, "/gfx"))
  
  # Download the files
  invisible(github_download_files(theme_file_list, user, repo, default_branch))
  
  # No return from the function
  return(NULL)
}
