#' Wrapper function to install commonly used packages
#' 
#' The function should only be called with a fresh install of R. It will install
#' all packages I have added to the list. The function serves no other purpose.
#' 
#' @export

install_common_packages <- function() {
  packages <- c("devtools", "packrat", "rsconnect", "config", "roxygen2",
    "testthat", "tidyverse", "pryr", "feather", "shiny", "shinyjs",
    "shinyWidgets", "aws.s3", "RMariaDB", "janitor", "data.table",
    "Rfast", "maxLik", "trustOptim", "nloptr", "numDeriv",
    "randtoolbox", "matrixStats", "evd", "apollo", "gmnl",
    "carthography", "sf", "AER", "car", "psych", "MatchIt",
    "knitr", "komadown", "xtable", "tikzDevice", "formatR",
    "swirl", "microbenchmark", "installr", "fortune", "beepr",
    "lintr", "crayon", "pillar")
  
  utils::install.packages(packages, repos = "https://cloud.r-project.org")
}
