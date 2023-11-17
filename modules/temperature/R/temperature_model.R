#' Accessory function to temperature module
#' 
#' @param ras a raster layer used as template.
#' @return a fake temperature SpatRaster generated as a Gaussian map with scale = 100 and variance = 0.01
#' @importFrom NLMR nlm_mpd
#' @importFrom terra rast
temperature_model <- function(ras) {
  temp_ras <- NLMR::nlm_mpd(
    ncol = ncol(ras),
    nrow = nrow(ras),
    resolution = unique(res(ras)),
    roughness = 0.5,
    rand_dev = 10,
    rescale = FALSE,
    verbose = FALSE
  ) |>
    rast()
  return(temp_ras)
}
