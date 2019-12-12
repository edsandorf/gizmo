#' Print package startup message
#' 
#' The function is called when the package is loaded through library or require.
#' 
#' @param libname Library name
#' @param pkgname Package name
#' 
#' @return Nothing
#' 
.onAttach <- function (libname, pkgname) {
  installed_version <- utils::packageDescription("gizmo", fields = "Version")
  
  description <- tryCatch({
    readLines("https://raw.githubusercontent.com/edsandorf/gizmo/master/DESCRIPTION")
  }, warning = function(w) {
    return("NA")
  }, error = function(e) {
    return("NA")
  })
  
  if (length(description) == 1) {
    remote_version <- description
  } else {
    remote_version <- gsub("Version:\\s*", "", description[grep('Version:', description)])    
  }
  
  packageStartupMessage("Thank you for using gizmo! \n\n",
    "You are currently using version: ",
    installed_version, "\n\n",
    "The latest version is: ", remote_version, "\n\n",
    "To access the latest version, please run \n",
    "devtools::install_github('edsandorf/gizmo') \n\n",
    "Please cite as: \n", 
    "Sandorf, E. D., 2019, gizmo: A Collection of Utility Functions for Choice Modeling and Statistical Analysis, https://github.com/edsandorf/gizmo \n\n")
}
