#' Rename a folder 
#'
#' @param old String with old name (path)
#' @param new String with new name (path)
#'
#' @export
rename_folder <- function(old, new) {
  shell(paste("rename", old, new))
}
