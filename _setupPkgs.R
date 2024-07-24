## Setup - packages

options(repos = c("https://predictiveecology.r-universe.dev/", 
                  CRAN = "https://cloud.r-project.org"))

if (!isTRUE(as.logical(Sys.getenv("CI")))) {
  pkgPath <- normalizePath(file.path("packages", version$platform,
                                     paste0(version$major, ".", strsplit(version$minor, "[.]")[[1]][1])),
                           winslash = "/")
  dir.create(pkgPath, recursive = TRUE)
  .libPaths(pkgPath, include.site = FALSE)
}

install.packages("Require")

Require::setLinuxBinaryRepo()

Require::Require(c(
  "ggpubr", 
  "ggplot2",
  "ggpubr",
  "htmlwidgets",
  "quickPlot",
  "reshape2",  ## not sure why this is needed but part 1 fails to load some pkg without it
  "rmarkdown",
  "rnaturalearth",
  "ropensci/rnaturalearthhires", 
  "ropensci/NLMR",
  "rsvg",
  "SpaDES",
  "terra"
), 
require = FALSE,   ## don't load packages
upgrade = FALSE, ## don't upgrade dependencies
standAlone = ifelse(isTRUE(as.logical(Sys.getenv("CI"))), FALSE, TRUE))   
