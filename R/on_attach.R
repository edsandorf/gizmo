#' Print package startup message
#' 
#' The function is called when the package is loaded through library or require.
#' 
#' @param libname Library name
#' @param pkgname Package name
#' 
#' @return Nothing

.onAttach <- function(libname, pkgname){
    pkg_version <- utils::packageDescription("gizmo", fields = "Version")
    
    packageStartupMessage("Thank you for using gizmo! \n\n",
                          "You are currently using version: ", pkg_version, "\n\n",
                          "To access the latest version, please run \n",
                          "devtools::install_github('edsandorf/gizmo') \n\n")
}