#' Setup Beamer 
#' 
#' Downloads the correct Beamer template folder from my Github repository and
#' renames it to 'presentation'
#'
#' @param theme A character string giving the name of the Beamer theme
#'
#' @export
setup_beamer <- function(theme = "nmbu") {
  # Check if already exists
  if (dir.exists(file.path(getwd(), "presentations"))) {
    stop("Beamer has already been set up for this project. To update the theme
         to the newest version, use: update_beamer()")
  }
  
  # Define useful variables
  beamer_theme <- paste0("beamer-", theme)
  
  # Download the Beamer template 
  invisible(download_beamer(beamer_theme))
  
  # Rename folder
  rename_dir(beamer_theme, "presentations")
}

#' Updates the Beamer template
#'
#' The function updates the Beamer templates in the current working directory
#' and all sub-directories recursively.
#' 
#' @inheritParams setup_beamer
#' 
#' @export
update_beamer <- function(theme = "nmbu") {
  # Define useful variables
  beamer_theme <- paste0("beamer-", theme)

  # List all files in the current working directory and sub-directories
  all_files <- list.files(recursive = TRUE)
  
  # Download files (after to avoid also counting these)
  invisible(download_beamer(beamer_theme))
  
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
download_beamer <- function(beamer_theme) {
  # Check if the folder already exists
  if (dir.exists(file.path(getwd(), beamer_theme))) {
    stop("The theme has already been downloaded")
  }
  
  # Get the list of all files in the repository
  url_latex_templates <- "https://api.github.com/repos/edsandorf/latex-templates/git/trees/main?recursive=1"
  url_request <- httr::GET(url_latex_templates)
  httr::stop_for_status(url_request)
  repo_file_list <- unlist(lapply(httr::content(url_request)$tree, "[", "path"),
                           use.names = FALSE)
 
  # Subset to only include the list of files associated with the current theme
  theme_file_list <- grep(paste0(beamer_theme, "/.*\\.(sty|eps|Rmd|jpg)$"),
                          repo_file_list,
                          value = TRUE,
                          fixed = FALSE)
  
  # Create the directory structure
  create_dir(beamer_theme)
  create_dir(paste0(beamer_theme, "/gfx"))
  
  # Download the files
  source_url <- paste0("https://github.com/edsandorf/latex-templates/raw/main/",
                       theme_file_list)
  dest_path <- file.path(getwd(), theme_file_list)
  for (i in seq_along(source_url)) {
    utils::download.file(source_url[[i]], dest_path[[i]])
  }
  
  # No return from the function
  return(NULL)
}
