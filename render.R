## ---------------------------------------------------
## GHA RENDER SCRIPT
## ---------------------------------------------------

## not necessary when rendering via GHA
if (!isTRUE(as.logical(Sys.getenv("CI")))) {
  pkgPath <- normalizePath(file.path("packages", version$platform,
                                     paste0(version$major, ".", strsplit(version$minor, "[.]")[[1]][1])),
                           winslash = "/")
  dir.create(pkgPath, recursive = TRUE)
  .libPaths(pkgPath, include.site = FALSE)
}

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
  # "terra", "quickPlot", "ggplot2",
  "SpaDES",
  "ropensci/NLMR",
  "cffr"
), 
require = FALSE,   ## don't load packages
upgrade = FALSE)   ## don't upgrade dependencies

## create .nojekyll file
file.create(".nojekyll")
usethis::use_description(
  list(
    Type = "project", 
    Title = "A SpaDES tutorial for begginners",
    Version = "1.3.0",
    Date = "2023-09-05",
    URL = c("https://ceresbarros.github.io/SpaDES4Dummies/",
            "https://github.com/CeresBarros/SpaDES4Dummies"),
    `Authors@R` = c(
      person( "Ceres", family = "Barros", email = "ceres.barros@gov.bc.ca", role = c("aut", "cre"),
              comment = c(ORCID = "0000-0003-4036-977X")),
      person("Tati", family = "Micheletti", email = "tati.micheletti@gmail.com", role = c("ctb"),
             comment = c(ORCID = "0000-0003-4838-8342")),
      person("Alex M", family = "Chubaty", email = "achubaty@for-cast.ca", role = c("ctb"),
             comment = c(ORCID = "0000-0001-7146-8135"))
    ),
    Description = paste("This guide is an introduction to the `SpaDES` `R` modelling platform.",
                        "It covers how to make and link `SpaDES` modules using `SpaDES` in two examples.", 
                        "Both examples draw on basic uses of statistical models in ecology, notably the ",
                        "relationships between environmental variables and species abundance and presence."),
    Depends = "R (>= 4.3)",
    Imports = c("bookdown",
                "geodata",
                "ggplot2",
                "htmlwidgets",
                "png",
                "raster",
                "rcmdcheck",
                "rmarkdown",
                "rsvg",
                "quickPlot (>= 1.0.2)",
                "SpaDES",
                "SpaDES.tools",
                "SpaDES.experiment"),
    Remotes = "PredictiveEcology/SpaDES.experiment@development",
    License = "GPL-3",
    Encoding = "UTF-8",
    Language = "en-CA"
  ))


## update CITATION.cff
cffr::cff_write(dependencies = FALSE, authors_roles = c("aut", "cre", "ctb"))

bookdown::render_book(output_format = "all", envir = new.env())
