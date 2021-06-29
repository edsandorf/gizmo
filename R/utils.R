#' Rename a directory 
#'
#' @param old String with old name (path)
#' @param new String with new name (path)
#'
#' @export
rename_dir <- function(old, new) {
  shell(paste("rename", old, new))
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
