#' Accessory function to speciesAbundance module
#' 
#' @param ras a raster layer used as template.
#' @return a fake abundance raster generated as a Gaussian map with scale = 100 and variance = 0.01
#' @import NLMR nlm_mpd
abundance_model <- function(ras) {
  # abund_ras <- gaussMap(ras, scale = 100, var = 0.01) ## RandomFields no longer available
  abund_ras <- NLMR::nlm_mpd(
    ncol = ncol(ras),
    nrow = nrow(ras),
    resolution = unique(res(ras)),
    roughness = 0.5, ## TODO: adjust to approximate gaussMap version
    rand_dev = 100,
    rescale = TRUE,
    verbose = FALSE
  )
  return(abund_ras)
}
