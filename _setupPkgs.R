## Setup - packages

if (!isTRUE(as.logical(Sys.getenv("CI")))) {
  pkgPath <- normalizePath(file.path("packages", version$platform,
                                     paste0(version$major, ".", strsplit(version$minor, "[.]")[[1]][1])),
                           winslash = "/")
  dir.create(pkgPath, recursive = TRUE)
  .libPaths(pkgPath, include.site = FALSE)
}

if (!"remotes" %in% installed.packages())
  install.packages("remotes")

if (!"Require" %in% installed.packages() || packageVersion("Require") < "0.3.1.9015") {
  remotes::install_github("PredictiveEcology/Require@2788b023ad191c29346ef8c64df71b937be307e2",
                          upgrade = FALSE)
}

Require::Require(c(
  "htmlwidgets", "geodata", 
  "SpaDES",
  # "PredictiveEcology/SpaDES.experiment@75d917b70b892802fed0bbdb2a5e9f3c6772f0ba",
  "ggpubr", "rmarkdown", "rsvg",
  "formatR"   ## for tidy = TRUE option
), 
require = FALSE,   ## don't load packages
upgrade = FALSE, ## don't upgrade dependencies
standAlone = ifelse(isTRUE(as.logical(Sys.getenv("CI"))), FALSE, TRUE))   
