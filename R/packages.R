#' Load multiple packages
#' 
#' A wrapper function to quickly load multiple packages passed as a 
#' character string.
#' 
#' @param pkgs A string of length >= 1 of package names. 
#' @param silent A boolean. If TRUE suppresses package loading messages. Default
#' FALSE.
#' 
#' @export
load_packages <- function(pkgs, silent = FALSE) {
  if (silent) {
    invisible(suppressMessages(lapply(pkgs, require, character.only = TRUE)))
  } else {
    invisible(lapply(pkgs, require, character.only = TRUE))   
  }
}

#' Check whether a package is loaded
#' 
#' A simple check to see whether a package is loaded.
#' 
#' @param pkgs A string of length >= 1 of package names.
#' 
#' @return TRUE if all packages are loaded and FALSE if one or 
#' more of the packages are not loaded.
#' 
#' @export
is_package_loaded <- function(pkgs) {
  all(pkgs %in% .packages())
}
