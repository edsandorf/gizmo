#' Arrange multiple plots in a grid
#' 
#' The function takes a list of plots made using ggplot() as input and arranges 
#' them in a grid.
#' 
#' @param ... plot objects
#' @param plot_list List of plot objects. Defaults to NULL. 
#' @param columns An integer specifying the number of columns. The number of rows
#' is calculated automatically. Defaults to 1. 
#' @param by_row If TRUE, the generated layout_matrix is filled by row. Default
#' is FALSE.
#' @param layout_matrix A layout matrix. Defaults to NULL. 
#' 
#' @references 
#' The function is based on the multiplot function in "Cookbook for R". Please
#' see the original code at 
#' <http://www.cookbook-r.com/Graphs/Multiple_graphs_on_one_page_(ggplot2)/>
#' 
#' @examples
#' library(ggplot2)
#' library(tibble)
#' 
#' df <- tibble(x = seq_len(10), y = runif(10), z = runif(10) * 1.5)
#' plot_1 <- ggplot(data = df) + geom_point(aes(x = x, y = y))
#' plot_2 <- ggplot(data = df) + geom_point(aes(x = x, y = z))
#' 
#' multi_plot(plot_1, plot_2)
#' multi_plot(plot_1, plot_2, columns = 2)
#' multi_plot(plot_list = list(plot_1, plot_2))
#' 
#' @export

multi_plot <- function(..., plot_list = NULL, columns = 1,
                      by_row = FALSE, layout_matrix = NULL) {
  
  plot_list <- c(list(...), plot_list)
  
  num_plots <- length(plot_list)
  
  # If no layout_matrix is supplied, determine based on the columns arg
  if (is.null(layout_matrix)) {
    rows <- ceiling(num_plots / columns)
    layout_matrix <- matrix(seq_len(rows * columns), nrow = rows,
                            ncol = columns, byrow = by_row)
  }
  
  # Check if we have passed only onen plot to the function
  if (num_plots == 1) {
    print(plot_list[[1L]])
  } else {
    # Set up the grid page
    grid::grid.newpage()
    layout_grid <- grid::grid.layout(nrow(layout_matrix), ncol(layout_matrix))
    grid::pushViewport(grid::viewport(layout = layout_grid))
    
    # Place each plot correctly
    for (i in seq_len(num_plots)) {
      match_index <- as.data.frame(which(layout_matrix == i, arr.ind = TRUE))
      print(plot_list[[i]], vp = grid::viewport(layout.pos.row = match_index$row,
                                            layout.pos.col = match_index$col))
    }
  }
}
