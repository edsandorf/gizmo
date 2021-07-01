#' Initialize a new project
#'
#' @param overwrite A boolean equal to TRUE if existing folders and files
#' should be overwritten. All warnings are supressed. Default is FALSE.
#'
#' @export
initialize_new_project <- function(overwrite = FALSE) {
  # Prompt the user to confirm that they wish to continue
  cli::cli_alert_info("This will overwrite existing folders and files without warning. Make sure that your working directory is empty and that you have initialized a new project.")
  proceed <- readline(prompt = "Do you wish to continue (y/n)?")
  
  # Check response and stop 
  if (!(tolower(proceed) %in% c("yes", "y"))) {
    return(cli::cli_alert_danger("Initialization aborted"))
  }
  
  # Create directories
  dirs <- c("data", "outputs")
  lapply(dirs, create_dir, overwrite = TRUE)
  
  # Draft
  drafts <- readline(prompt = "Do you wish to set up a paper draft (y/n)?")
  if (tolower(drafts) %in% c("yes", "y")) {
    # Create the presentation directory
    create_dir("drafts")
    file_name <- file.path("drafts/draft-authors-title-v01")
    rmarkdown::draft(file_name, template = "elsevier", package = "rticles",
                     create_dir = FALSE, edit = FALSE)
    cli::cli_alert_success("'drafts' folder created")
  }
  
  
  # Beamer presentations
  beamer <- readline(prompt = "Do you wish to set up the Beamer templates (y/n)?")
  if (tolower(beamer) %in% c("yes", "y")) {
    # Create the presentation directory
    setup_beamer(directory = "beamer-nmbu",
                 user = "edsandorf",
                 repo = "latex-templates")
  }
  
  # Setup a models directory (copy example files from Github)
  models <- readline(prompt = "Do you wish to set up the 'models' folder with examples (y/n)?")
  if (tolower(models) %in% c("yes", "y")) {
    setup_models(directory = "examples",
                 user = "edsandorf",
                 repo = "cmdlR")
  }
  
  # Print success message
  cli::cli_alert_success("Project sucessfully initialized")
}
