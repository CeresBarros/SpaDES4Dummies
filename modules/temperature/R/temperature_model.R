#' Accessory function to temperature module
#' 
#' @param ras a raster layer used as template.
#' @return a fake temperature raster generated as a Gaussian map with scale = 100 and variance = 0.01
#' @import NLMR nlm_mpd
temperature_model <- function(ras) {
  # temp_ras <- gaussMap(ras, scale = 100, var = 0.01) ## RandomFields no longer available
  temp_ras <- NLMR::nlm_mpd(
    ncol = ncol(ras),
    nrow = nrow(ras),
    resolution = unique(res(ras)),
    roughness = 0.5,
    rand_dev = 10,
    rescale = FALSE,
    verbose = FALSE
  )
  return(temp_ras)
}
