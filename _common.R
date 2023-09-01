## not necessary when rendering via GHA

if (!isTRUE(as.logical(Sys.getenv("CI")))) {
  pkgPath <- file.path("packages", version$platform,
                       paste0(version$major, ".", strsplit(version$minor, "[.]")[[1]][1]))
  .libPaths(pkgPath)   ## need to include.side = TRUE to use bookdown and rmarkdown
}

# example R options set globally
options("width" = 60)

# example chunk options set globally
knitr::opts_chunk$set(
  collapse = TRUE,
  tidy = TRUE, 
  tidy.opts = list(width.cutoff = 60),
  size = "tiny"
)

rm(list = ls(all.names = TRUE))

