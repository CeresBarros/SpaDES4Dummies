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
rScripts <- c("appendices/Part1_Rscript.R", "appendices/Part2_Rscript.R")
for (f in rScripts) {
  if (!requireNamespace("functionMap")) {
    remotes::install_github("MangoTheCat/functionMap")
    requireNamespace("functionMap")
  }
  scriptLines <- readLines(f)
  
  fCalls <- functionMap:::parse_r_script(f)[[1]]
  startSP <- fCalls[fCalls$to == "setupProject",]$line
  
  endSP <- startSP + 1  
  
  allFuns <- functionMap:::get_funcs_from_r_script(f)
  SPcall <- sapply(allFuns, function(x) {
    attributes(x)$src$line1[1] == startSP}
  )
  SPcall <- allFuns[[which(SPcall)]]
  endSP <- tail(attr(SPcall, "src")$line1, 1)
  
  ## replace
  SPcall <- scriptLines[startSP:endSP]
  
  modulesStart <- grep("modules =", SPcall)
  
  modulesLines <- SPcall[modulesStart:length(SPcall)]
  modulesEnd <- grep("),$", modulesLines)[1]
  modulesLines <- modulesLines[1:modulesEnd]
  
  modulesLines <- gsub("(\")([[:alpha:]]*)(\")", "\\1CeresBarros/SpaDES4Dummies@master/modules/\\2\\3", modulesLines)
  
  SPcall[modulesStart:(modulesStart+modulesEnd-1)] <- modulesLines
  
  ## add overwrite = TRUE if not there
  SPcall2 <- SPcall
  SPcall2[1] <- sub("(.*)(<-)(.*)", "\\3", SPcall2[1])
  SPcall2 <- parse(text = SPcall2) |> as.list() |> _[[1]] |> as.call()   ## as.list()[[1]] removes the `expression` part.
  
  overwritexists <- any(names(as.list(SPcall2)) == "overwrite")
  
  if (overwritexists) {
    SPcall2list <- as.list(SPcall2)
    if (isFALSE(SPcall2list$overwrite)) {
      SPcall2list$overwrite <- TRUE
      SPcall2 <- as.call(SPcall2list) |>
        deparse()
    }
  } else {
    SPcall2 <- as.call(append(as.list(SPcall2), list("overwrite" = TRUE))) |> 
      deparse()
  }
  
  ## add assignment
  assignBit <- sub("(.*)(<-)(.*)", "\\1\\2", SPcall[1])
  SPcall2[1] <- paste(assignBit, SPcall2[1])
  
  beforeSP <- scriptLines[1:(startSP-1)]
  afterSP <- scriptLines[endSP:length(scriptLines)]
  scriptLines <- c(beforeSP, SPcall2, afterSP)
  
  ff <- sub("\\.R", "_test\\.R", basename(f))
  writeLines(scriptLines, file.path("test", ff))
}

