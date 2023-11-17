#' Function used to plot SpatRaster
#' 
#' To be used with Plots
#' 
#' @param ras a SpatRaster layer
#' @param title character. Plot title
#' @param xlab character. X-axis title
#' @param ylab character. Y-axis title
#' 
#' @importFrom rasterVis gplot 
#' @importFrom ggplot2 geom_tile scale_fill_brewer coord_equal theme_bw

plotSpatRaster <- function(ras, plotTitle = "", xlab = "x", ylab = "y") {
  gplot(ras) + 
    geom_tile(aes(fill = value)) +
    scale_fill_distiller(palette = "Blues", direction = 1,
                         na.value = "grey90", limits = c(0,1) ) +
    theme_classic() +
    coord_equal() +
    labs(title = plotTitle, x = xlab, y = ylab)
}
