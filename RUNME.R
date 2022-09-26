## ---------------------------------------
## SPADES4DUMMIES RENDERING SCRIPT
## ---------------------------------------

## Sets up project library and renders book
options(repos = c(CRAN = "https://cloud.r-project.org"))

## note that "rmarkdown", "bookdown", "htmlwidgets" need to be installed in the default
## libraries, because each .Rmd starts from a clean R session
needPkgs <- c("rmarkdown", "bookdown", "htmlwidgets")
needPkgs <- needPkgs[!needPkgs %in% installed.packages()] 
for (pkg in needPkgs) {
  install.packages(pkg, dependencies = TRUE)
}

pkgPath <- normalizePath(file.path("packages", version$platform,
                                   paste0(version$major, ".", strsplit(version$minor, "[.]")[[1]][1])),
                         winslash = "/")
dir.create(pkgPath, recursive = TRUE)
.libPaths(pkgPath, include.site = FALSE)

## note that pkgPath is defined in common.R
if (!"remotes" %in% installed.packages(lib.loc = pkgPath))
  install.packages("remotes")

if (!"Require" %in% installed.packages(lib.loc = pkgPath) || 
    packageVersion("Require", lib.loc = pkgPath) < "0.1.2") {
  remotes::install_github("PredictiveEcology/Require@86254b17ad2392de5c9e4dae6dd06a194b69a169",
                          upgrade = FALSE, force = TRUE)
}

## use binary linux packages if on Ubuntu
Require::setLinuxBinaryRepo()

Require::Require(c("PredictiveEcology/SpaDES.project@transition", "SpaDES"), 
                 require = FALSE, upgrade = FALSE, standAlone = TRUE)

outs <- SpaDES.project::packagesInModules(modulePath = file.path("modules"))  ## gets list of module dependencies
Require::Require(c(unname(unlist(outs)),
                   "bookdown", "DiagrammeR", "htmlwidgets", "geodata", "ggplot2", "ggpubr", "rmarkdown",
                   "PredictiveEcology/SpaDES.experiment@development"), 
                 require = FALSE,   ## don't load packages
                 upgrade = FALSE,   ## don't upgrade dependencies
                 standAlone = TRUE) 

bookdown::render_book()
