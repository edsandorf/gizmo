#' Get the last element of a vector
#' 
#' \code{last} extracts the last element of a vector, matrix or list
#' 
#' @param x A vector, matrix or list
#' 
#' @return A single value or list element
#' 
#' @examples
#' x <- 1:4
#' last(x)
#' 
#' x <- c("my", "name", "is", "buttons")
#' last(x)
#' 
#' x <- matrix(runif(10), 5, 2)
#' last(x)
#' 
#' x <- matrix(LETTERS[1:10], 5, 2)
#' last(x)
#' 
#' x <- list(1:5, 6:10)
#' last(x)
#' 
#' @export

last <- function (x) {
  x[length(x)]
}
