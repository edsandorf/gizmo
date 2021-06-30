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

#' Create the directory structure based on the supplied file list
#'
#' @inheritParams github_download_files
#' 
#' @return NULL
create_dir_structure <- function(file_list) {
  # Extract the directory structure by splitting on '/' and removing the file
  directory_structure <- unique(
    lapply(strsplit(file_list, "/"), function(x) {
      return(x[-length(x)])
    })
  )
  names(directory_structure) <- letters[seq_along(directory_structure)]
  
  # Get the directory depth and sort in ascending order
  directory_depth <- as.list(sort(unlist(lapply(directory_structure, length))))
  
  # Reorder the directory structure based on depth to ensure that top order
  # directories are created first
  directory_structure <- directory_structure[names(directory_depth)]
  
  # Create the directory structure
  lapply(directory_structure, function(x) {
    create_dir(paste(x, collapse = "/"))
    return(NULL)
  })
  
  return(NULL)
}

#' Create a regex for file extenstions
#' 
#' @param string A character vector with file extensions
#' 
#' @examples 
#' \dontrun{
#'   make_file_extensions_regex(c("jpg"))
#'   make_file_extensions_regex(c("jpg", "eps", "bmp"))
#' }
make_file_extensions_regex <- function(string) {
  return(
    paste0(
      "/.*\\.(",
      paste(string, collapse = "|"),
      ")$"
    )
  )
}
