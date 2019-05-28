#' Load multiple packages
#' 
#' A wrapper function to quickly load multiple packages passed as a 
#' character string.
#' 
#' @param packages A string of length >= 1 of package names. 
#' 
#' @export

load_packages <- function(packages){
    invisible(lapply(packages, require, character.only = TRUE))
}

#' Check whether a package is loaded
#' 
#' A simple check to see whether a package is loaded.
#' 
#' @param packages A string of length >= 1 of package names.
#' 
#' @return TRUE if all packages are loaded and FALSE if one or more of the packages are not loaded.
#' 
#' @export

is_package_loaded <- function(packages){
    all(packages %in% .packages())
}

