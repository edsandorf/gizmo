#' An automatic error check and search
#'
#' When passed to `options(error = automatic_error_search`, the function will
#' run every time R encounters an error. It will check whether you have made
#' the common mistake of trying to subset a function ('closure'). If it is any
#' other type of error, it will copy the error message and launch a Google
#' search.
#'
#' This function is still being developed and tested and functionality may
#' change. For example, I may decide to include a list of errors that do not
#' trigger a Google search.
#'
#' @return Nothing
#'
#' @references
#' Adapted from the code published in this Tweet
#' https://twitter.com/RCoderWeb/status/1358474355492151297
#'
#' @examples
#' \dontrun{
#'   options(error = automatic_error_search)
#' }
#'
automatic_error_search <- function() {
  hist_tmp <- tempfile()
  savehistory(hist_tmp)
  hist_lines <- readLines(hist_tmp)
  last_line <- tail(hist_lines, 1)
  err <- pander::evals(last_line)[[1]][["msg"]][["errors"]]
  if (grepl("closure", err, fixed = TRUE)) {
    cat(
      "You have tried to subset a function... Again...\n",
      "Did you name your data 'data' again?"
    )
  } else {
    cat("Unknown error... Google is your friend... Launching automatic search")
    browseURL(paste0("https://www.google.com/search?q=", err))
  }
}
