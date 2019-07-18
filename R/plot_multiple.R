#' Arrange multiple plots in a grid
#' 
#' The function takes a list of plots made using ggplot() as input and arranges 
#' them in a grid.
#' 
#' @param ... plot objects
#' @param plots List of plot objects. Defaults to NULL. 
#' @param cols An integer specifying the number of columns. The number of rows
#' is calculated automatically. Defaults to 1. 
#' @param by_row If TRUE, the generated layout_matrix is filled by row. Default
#' is FALSE.
#' @param layout_matrix A layout matrix. Defaults to NULL. 
#' @param shared_legend If TRUE the plots share the same legend. Defaults to 
#' FALSE.
#' @param position Position of the shared legend. Can be either "bottom" or
#' "right".
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
#' df <- tibble(x = seq_len(10), y = runif(10), z = runif(10) * 1.5,
#'  a = sample(c(0, 1), 10, TRUE))
#' plot_1 <- ggplot(data = df) + geom_point(aes(x = x, y = y))
#' plot_2 <- ggplot(data = df) + geom_point(aes(x = x, y = z))
#' 
#' plot_multiple(plot_1, plot_2)
#' plot_multiple(plot_1, plot_2, cols = 2)
#' plot_multiple(plots = list(plot_1, plot_2))
#' 
#' plot_3 <- ggplot(data = df) + geom_point(aes(x = x, y = y, colour = a))
#' plot_4 <- ggplot(data = df) + geom_point(aes(x = x, y = z, colour = a))
#' plot_multiple(plot_3, plot_4, shared_legend = TRUE, position = "bottom")
#' plot_multiple(plot_3, plot_4, shared_legend = TRUE, position = "right")
#' 
#' @export

plot_multiple <- function (..., plots = NULL, cols = 1, by_row = FALSE,
  layout_matrix = NULL, shared_legend = FALSE, position = c("bottom", "right")) {
  
  # If plots is not a list then stop
  if (!is.list(plots) && !is.null(plots)) {
    stop("Plots must be a list")
  }
  
  # Combine plots to list
  plots <- c(list(...), plots)
  n_plots <- length(plots)
  
  # If no layout_matrix is supplied, determine based on the cols arg
  if (is.null(layout_matrix)) {
    rows <- ceiling(n_plots / cols)
    layout_matrix <- matrix(seq_len(rows * cols), nrow = rows,
      ncol = cols, byrow = by_row)
  }
  
  # Check if the plots have legends
  has_legend <- lapply(plots, function (x) {
    any(ggplot2::ggplotGrob(x)$layout$name == "guide-box")
  })
  
  # If we have a legend
  if (any(unlist(has_legend))) {
    # Get the position of the legend
    position <- match.arg(position)
    if (shared_legend) {
      plot_tmp <- plots[[which(do.call(c, has_legend) == TRUE)[1]]]
      grob_tmp <- ggplot2::ggplotGrob(plot_tmp + 
          ggplot2::theme(legend.position = position))$grobs
      legend <- grob_tmp[[which(sapply(grob_tmp, function (x) x$name) == "guide-box")]]
      height <- sum(legend$height)
      width <- sum(legend$width)
      # Set legend to none for all plots
      plot_layout <- lapply(plots,
        function (x) x + ggplot2::theme(legend.position = "none"))
      plot_layout <- c(plot_layout, nrow = rows, ncol = cols)
      
      plot_legend <- switch(position, 
        "bottom" = gridExtra::arrangeGrob(do.call(gridExtra::arrangeGrob, plot_layout),
          legend,
          ncol = 1,
          heights = grid::unit.c(grid::unit(1, "npc") - height, height)),
        "right" = gridExtra::arrangeGrob(do.call(gridExtra::arrangeGrob, plot_layout),
          legend,
          ncol = 2,
          widths = grid::unit.c(grid::unit(1, "npc") - width, width)))
    } else {
      plot_legend <- plots
    }
  }
  
  # Check if we have passed only onen plot to the function
  if (n_plots == 1) {
    print(plots[[1L]])
  } else {
    # Set up the grid page
    grid::grid.newpage()
    if (shared_legend && any(unlist(has_legend))) {
      grid::grid.draw(plot_legend)
    } else {
      layout_grid <- grid::grid.layout(nrow(layout_matrix), ncol(layout_matrix))
      grid::pushViewport(grid::viewport(layout = layout_grid))
      
      # Place each plot correctly
      for (i in seq_len(n_plots)) {
        match_index <- as.data.frame(which(layout_matrix == i, arr.ind = TRUE))
        print(plots[[i]], vp = grid::viewport(layout.pos.row = match_index$row,
          layout.pos.col = match_index$col))
      }
    }
  }
}
