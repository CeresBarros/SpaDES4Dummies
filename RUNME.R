## ---------------------------------------
## SPADES4DUMMIES RENDERING SCRIPT
## ---------------------------------------

## Sets up project library and renders book
options(repos = c("https://predictiveecology.r-universe.dev/", 
                  CRAN = "https://cloud.r-project.org"))

## note that "rmarkdown", "bookdown", "htmlwidgets" need to be installed in the default
## libraries, because each .Rmd starts from a clean R session
needPkgs <- c("rmarkdown", "bookdown", "htmlwidgets", "tinytex", "git2r")
needPkgs <- needPkgs[!needPkgs %in% installed.packages()] 
for (pkg in needPkgs) {
  install.packages(pkg, dependencies = TRUE)
}

tinytex::install_tinytex()

pkgPath <- normalizePath(file.path("packages", version$platform,
                                   paste0(version$major, ".", strsplit(version$minor, "[.]")[[1]][1])),
                         winslash = "/")
dir.create(pkgPath, recursive = TRUE)
.libPaths(pkgPath, include.site = FALSE)

## note that pkgPath is defined in common.R
if (!"remotes" %in% installed.packages(lib.loc = pkgPath))
  install.packages("remotes")

if (!"Require" %in% installed.packages(lib.loc = pkgPath) ||
    packageVersion("Require", lib.loc = pkgPath) < "0.3.1") {
  remotes::install_github("PredictiveEcology/Require@55ec169e654214d86be62a0e13e9a2157f1aa966",
                          upgrade = FALSE)
}

## use binary linux packages if on Ubuntu
Require::setLinuxBinaryRepo()

Require::Require(c("bookdown", "htmlwidgets", "geodata", "SpaDES",
                   "PredictiveEcology/SpaDES.experiment@75d917b70b892802fed0bbdb2a5e9f3c6772f0ba",
                   "ggpubr", "rmarkdown", "rsvg"), 
                 require = FALSE,   ## don't load packages
                 upgrade = FALSE,   ## don't upgrade dependencies
                 standAlone = TRUE) 

if (FALSE) { ## not needed anymore but may come in handy
  ## before rendering, delete zips, re-zip and push
  zipFiles <- list.files("modules", pattern = ".zip$", recursive = TRUE, full.names = TRUE)
  file.remove(zipFiles)
  
  modules <- basename(list.dirs("modules", recursive = FALSE))
  for (m in modules) {
    zipModule(m, path = "modules")
  }
  
  git2r::add(path = "*.zip")
  out <- tryCatch(git2r::commit(message = "update module zips"), error = function(e) e)
  if (!inherits(out, "error")) system("git push")
} 

bookdown::render_book(output_format = "all", envir = new.env())


## make test scripts for GHA
rScripts <- c("Part1_DummyModel.R", "Part2_SDMs.R")
for (f in rScripts) {
  scriptLines <- readLines(f)
  mainPathLine <- grep("mainPath <-", scriptLines)
  scriptLines[mainPathLine] <- "mainPath <- '.'"
  ff <- sub("\\.R", "_test\\.R", f)
  writeLines(scriptLines, ff)
}

