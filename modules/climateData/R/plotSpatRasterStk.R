#' Function used to plot SpatRaster Stacks
#' 
#' To be used with Plots
#' 
#' @param stk a SpatRaster stack.
#' @param title character. Plot title
#' @param xlab character. X-axis title
#' @param ylab character. Y-axis title
#' 
#' @importFrom rasterVis gplot
#' @importFrom ggplot geom_tile facet_wrap scale_fill_brewer coord_equal

plotSpatRasterStk <-  function(stk, plotTitle = "", xlab = "x", ylab = "y") {
  gplot(stk) + 
    scale_fill_distiller(palette = "Blues", direction = 1, na.value = "grey90") +
    theme_classic() +
    coord_equal() +
    facet_wrap(~ variable) +
    labs(title = plotTitle, x = xlab, y = ylab)
}