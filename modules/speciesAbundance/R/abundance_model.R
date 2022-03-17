#' Accessory function to speciesAbundance module
#' 
#' @param ras a raster layer used as template.
#' @return a fake abundance raster generated as a Gaussian map with scale = 100 and variance = 0.01
#' @import SpaDES.tools gaussMap 
abundance_model <- function(ras) {
  abund_ras <- gaussMap(ras, scale = 100, var = 0.01) 
  return(abund_ras)
}
