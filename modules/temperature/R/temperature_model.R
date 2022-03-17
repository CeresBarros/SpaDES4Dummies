#' Accessory function to temperature module
#' 
#' @param ras a raster layer used as template.
#' @return a fake temperature raster generated as a Gaussian map with scale = 100 and variance = 0.01
#' @import SpaDES.tools gaussMap 

temperature_model <- function(ras) {
  temp_ras <- gaussMap(ras, scale = 100, var = 0.01) 
  return(temp_ras)
}
