options(repos = c(CRAN = "https://cloud.r-project.org"))

if (paste(R.Version()[c("major", "minor")], collapse = ".") < "4.2.1") {
  warning(paste("dismo::maxent may create a fatal error",
                "when using R version < v4.2.1 and from RStudio.\n", 
                "Please upgrade R, or run this script outside of RStudio.\n",
                "See https://github.com/rspatial/dismo/issues/13"))
}

## decide where you're working
mainPath <- "~/SpaDES4Dummies_Part1"
pkgPath <- file.path(mainPath, "packages", version$platform,
                     paste0(version$major, ".", strsplit(version$minor, "[.]")[[1]][1]))
dir.create(pkgPath, recursive = TRUE)
.libPaths(pkgPath, include.site = FALSE) ## install packages in project library (proj-lib)

if (!"remotes" %in% installed.packages(lib.loc = pkgPath))
  install.packages("remotes")

if (!"Require" %in% installed.packages(lib.loc = pkgPath) ||
    packageVersion("Require", lib.loc = pkgPath) < "0.3.1") {
  remotes::install_github("PredictiveEcology/Require@55ec169e654214d86be62a0e13e9a2157f1aa966",
                          upgrade = FALSE)
}

## use binary linux packages if on Ubuntu
Require::setLinuxBinaryRepo()

Require::Require(c("SpaDES"), upgrade = FALSE, standAlone = TRUE)

setPaths(cachePath = file.path(mainPath, "cache"),
                      inputPath = file.path(mainPath, "inputs"),
                      modulePath = file.path(mainPath, "modules"),
                      outputPath = file.path(mainPath, "outputs"))
getPaths() ## check that this is what you wanted

## Let's create a self-contained module that will simulate the species' abundance for any given period of time and frequency.
if (!dir.exists(file.path(getPaths()$modulePath, "speciesAbundance"))) {
  newModule(name = "speciesAbundance", path = getPaths()$modulePath)
}

if (!dir.exists(file.path(getPaths()$modulePath, "temperature"))) {
  newModule(name = "temperature", path = getPaths()$modulePath)
}

if (!dir.exists(file.path(getPaths()$modulePath, "speciesTempLM"))) {
  newModule(name = "speciesTempLM", path = getPaths()$modulePath)
}

## list the modules to use
simModules <- list("speciesAbundance", "temperature", "speciesTempLM")

## Set simulation and module parameters
simTimes <- list(start = 1, end = 10, timeunit = "year")
simParams <- list(
  speciesAbundance = list(simulationTimeStep = 1, 
                          .plotInitialTime = 1),
  temperature = list(simulationTimeStep = 1, 
                     .plotInitialTime = 1),
  speciesTempLM = list(statsTimestep = 5)
)

## make a list of directory paths
simPaths <- getPaths()

## Simulation setup
mySim <- simInit(times = simTimes, params = simParams, 
                 modules = simModules, paths = simPaths)

moduleDiagram(mySim)

objectDiagram(mySim)

## run simulation
dev() # on Windows and Mac, this opens external device if using Rstudio, it is faster
clearPlot()
mySim2 <- spades(mySim, debug = TRUE)
