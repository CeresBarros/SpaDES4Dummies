## ---------------------------------------------------
## GHA RENDER SCRIPT
## ---------------------------------------------------

## not necessary when rendering via GHA
# pkgPath <- normalizePath(file.path("packages", version$platform,
#                                    paste0(version$major, ".", strsplit(version$minor, "[.]")[[1]][1])),
#                          winslash = "/")
# dir.create(pkgPath, recursive = TRUE)
# .libPaths(pkgPath, include.site = FALSE)

options(repos = c("https://predictiveecology.r-universe.dev/", 
                  CRAN = "https://cloud.r-project.org"))

## note that pkgPath is defined in common.R
if (!"remotes" %in% installed.packages())
  install.packages("remotes")

if (!"Require" %in% installed.packages() || packageVersion("Require") < "0.3.1") {
  remotes::install_github("PredictiveEcology/Require@55ec169e654214d86be62a0e13e9a2157f1aa966",
                          upgrade = FALSE)
}

## use binary linux packages if on Ubuntu
Require::setLinuxBinaryRepo()

Require::Require(c(#"bookdown", "htmlwidgets", "geodata", 
                   # "PredictiveEcology/SpaDES.experiment@75d917b70b892802fed0bbdb2a5e9f3c6772f0ba",
                   # "ggpubr", "rmarkdown", "rsvg", "png",
                   ## pkgs for Part1:
                   # "raster", "quickPlot", "ggplot2", "SpaDES.tools"
                   "SpaDES",
                   "ropensci/NLMR"
                   ), 
                 require = FALSE,   ## don't load packages
                 upgrade = FALSE)   ## don't upgrade dependencies

## create .nojekyll file
file.create(".nojekyll")

bookdown::render_book(output_format = "all", envir = new.env())
