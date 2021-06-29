#' Initialize a new project
#'
#' @param theme A character string giving the name of the Beamer theme
#' @param overwrite A boolean equal to TRUE if existing folders and files
#' should be overwritten. All warnings are supressed. Default is FALSE.
#'
#' @export
initialize_new_project <- function(theme = "nmbu", overwrite = FALSE) {
  # Prompt the user to confirm that they wish to continue
  cli::cli_alert_info("This will overwrite existing folders and files without warning. Make sure that your working directory is empty and that you have initialized a new project.")
  proceed <- readline(prompt = "Do you wish to continue (y/n)?")
  
  # Check response and stop 
  if (!(tolower(proceed) %in% c("yes", "y"))) {
    return(cli::cli_alert_danger("Initialization aborted"))
  }
  
  # Create directories
  dirs <- c("data", "models", "outputs")
  lapply(dirs, create_dir, overwrite = TRUE)
  
  # Beamer presentations
  beamer <- readline(prompt = "Do you wish to set up the Beamer templates (y/n)?")
  if (tolower(beamer) %in% c("yes", "y")) {
    # Create the presentation directory
    setup_beamer(theme)
  }
  
  # Print success message
  cli::cli_alert_success("Project sucessfully initialized")
}

#' Create a directory
#' 
#' A wrapper function for creating new directories. Each directory is created
#' relative to the current working directory.
#' 
#' @param string A character string giving the name of the new folder.
#' @inheritParams initialize_new_project
#' 
#' @export
create_dir <- function(string, overwrite = FALSE) {
  path <- file.path(getwd(), string)
  exists <- dir.exists(paths = path)
  if (!exists || (exists && overwrite)) {
    dir.create(path, showWarnings = FALSE)
  }
}
