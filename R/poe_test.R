#' A bootstrapping implementation of the Poe et al. (2005) test
#'
#' This is a "bootstrapping" implementation of the Poe et al. test for difference in empirical distributions.
#' Often the vectors for which we want to test for difference are large and as such the complete combinatorial
#' might be infeasible to calculate. This function runs the complete combinatorial on a large number of smaller
#' sample vectors and returns the average proportion the difference is less than zero (i.e. a one-sided test).
#' 
#' @param x An input vector
#' @param y An input vector
#' @param R An integer specifying the number of replicates. Defaults to 100
#' @param N An integer specifying the size of the samples drawn from the vectors X and Y. Defaults to 10
#' 
#' @references 
#' Poe G. L., Giraud, K. L. & Loomis, J. B., 2005, Computational methods for measuring the difference of empirical distributions, American Journal of Agricultural Economics, 87:353-365
#' 
#' @examples 
#' x <- qnorm(runif(100), mean = -0.5, sd = 1)
#' y <- qnorm(runif(100), mean = 1.5, sd = 2)
#' poe_test(x, y)
#' 
#' @export

poe_test <- function(x, y, R = 100, N = 10){
    if(!is.numeric(x)) stop("X must be numeric.")
    if(!is.numeric(y)) stop("Y must be numeric.")
    if(length(x) != length(y)) stop("The vectors should be of the same length")
    if(length(x) < N || length(y) < N) stop("Cannot take samples larger than the vector")
    
    #   Initialize an empty vector
    prop_diff <- rep(NA, R)
    
    #   "Bootstrap"
    for(r in seq_len(R)){
        #   Sample from the vectors without replacement
        x_tmp <- sample(x, N, replace = FALSE)
        y_tmp <- sample(y, N, replace = FALSE)
        
        #   Initialize an empty vector
        diff_tmp <- rep(NA, N^2)
        
        #   Take the difference between all the elements
        for(n in seq_len(N)){
            diff_tmp[(1L + ((n - 1L) * N)):(n * N)] <- x_tmp[n] - y_tmp
        }
        
        #   Store the proportion of differences less than 0
        prop_diff[r] <- length(diff_tmp[diff_tmp < 0]) / length(diff_tmp)
    }
    
    #   Print the results of the test
    cat("Gamma: ", mean(prop_diff), "\n")
    cat("The test was done using", R, "replicates and sample sizes of", N, ". Gamma >.95 and <.05 indicates difference at the 5% level.")
}
