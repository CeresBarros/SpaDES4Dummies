pkgPath <- file.path("packages", version$platform,
                     paste0(version$major, ".", strsplit(version$minor, "[.]")[[1]][1]))
.libPaths(pkgPath)   ## need to include.side = TRUE to use bookdown and rmarkdown

# example R options set globally
options("width" = 60,
        "repos" = c(CRAN = "https://cran.rstudio.com"))
# example chunk options set globally
knitr::opts_chunk$set(
  collapse = TRUE,
  cache = 2, 
  cache.rebuild = FALSE, 
  tidy = TRUE, 
  tidy.opts = list(width.cutoff = 60),
  size = "tiny"
)
