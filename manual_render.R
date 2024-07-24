## ---------------------------------------
## SPADES4DUMMIES RENDERING SCRIPT
## ---------------------------------------
## Sets up project library and renders book
options(repos = c("https://predictiveecology.r-universe.dev/", 
                  CRAN = "https://cloud.r-project.org"))

## note that "rmarkdown", "quarto", "htmlwidgets" need to be installed in the default
## libraries, because each .qmd starts from a clean R session
needPkgs <- c("rmarkdown", "quarto", "htmlwidgets", "tinytex", "git2r")
needPkgs <- needPkgs[!needPkgs %in% installed.packages()] 
for (pkg in needPkgs) {
  install.packages(pkg, dependencies = TRUE)
}

tinytex::install_tinytex()

## note that pkgPath is defined in _common.R
pkgPath <- normalizePath(file.path("packages", version$platform,
                                   paste0(version$major, ".", strsplit(version$minor, "[.]")[[1]][1])),
                         winslash = "/")
dir.create(pkgPath, recursive = TRUE)
.libPaths(pkgPath, include.site = FALSE)

install.packages("Require")

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

## create .nojekyll file
file.create(".nojekyll")

quarto::quarto_render(output_format = "all", as_job = FALSE)

## make test scripts for GHA
rScripts <- c("Part1_DummyModel.R", "Part2_SDMs.R")
for (f in rScripts) {
  scriptLines <- readLines(f)
  projPathLine <- grep("projPath <-", scriptLines)
  scriptLines[projPathLine] <- "projPath <- '.'"
  ff <- sub("\\.R", "_test\\.R", f)
  writeLines(scriptLines, ff)
}

