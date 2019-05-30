#' An implementation of the Poe et al. (2005) test
#'
#' The function calculates the complete combinatorial of the supplied vectors
#' using a loop implementation. 
#' 
#' @param x An input vector
#' @param y An input vector
#' 
#' @return A numeric test value
#' 
#' @references 
#' Poe G. L., Giraud, K. L. & Loomis, J. B., 2005, Computational methods for 
#' measuring the difference of empirical distributions, American Journal of
#'  Agricultural Economics, 87:353-365
#' 
#' @examples 
#' x <- qnorm(runif(100), mean = -0.5, sd = 1)
#' y <- qnorm(runif(100), mean = 1.5, sd = 2)
#' poe_test(x, y)
#' 
#' @export

poe_test <- function(x, y){
    if (!is.numeric(x)) stop("X must be numeric.")
    if (!is.numeric(y)) stop("Y must be numeric.")
    if (length(x) != length(y)) stop("The vectors should be of the same length")
    
    N <- length(x)
    
    #   Preallocate space to speed up looping
    n_diff <- rep(NA, N)
    for (n in seq_len(N)){
        n_diff[n] <- sum(x[n] - y < 0)
    }
    
    return(sum(n_diff) / N^2)
}

#' Summary function for poe_test()
#' 
#' @param x An estimate obtained from \code{\link{poe_test}}.
#'
#' @examples
#' x <- qnorm(runif(100), mean = -0.5, sd = 1)
#' y <- qnorm(runif(100), mean = 1.5, sd = 2)
#' test_stat <- poe_test(x, y)
#' summary_poe_test(test_stat)
#' 
#' @export

summary_poe_test <- function(x){
    cat("Gamma: ", x, "\n")
    cat("Gamma >.95 and <.05 indicates difference at the 5% level.")
}
