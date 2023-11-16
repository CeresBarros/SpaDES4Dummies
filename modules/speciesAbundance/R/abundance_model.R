#' Accessory function to speciesAbundance module
#' 
#' @param ras a raster layer used as template.
#' @return a fake abundance SpatRaster generated as a Gaussian map with scale = 100 and variance = 0.01
#' @importFrom NLMR nlm_mpd
#' @importFrom terra rast
abundance_model <- function(ras) {
  abund_ras <- NLMR::nlm_mpd(
    ncol = ncol(ras),
    nrow = nrow(ras),
    resolution = unique(res(ras)),
    roughness = 0.5,
    rand_dev = 100,
    rescale = TRUE,
    verbose = FALSE
  ) |>
    rast()
  return(abund_ras)
}
