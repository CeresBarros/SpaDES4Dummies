## ---------------------------------------------------
## SPADES4DUMMIES PART 1
## ---------------------------------------------------

options(repos = c("https://predictiveecology.r-universe.dev/", 
                  CRAN = "https://cloud.r-project.org"))

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

Require::Require(c("SpaDES", "DiagrammeR"), 
                 require = FALSE, upgrade = FALSE, 
                 dependencies = TRUE, standAlone = TRUE) ## automatically downloads all packages in the SpaDES family and their dependencies

library(SpaDES)

setPaths(cachePath = file.path(mainPath, "cache"),
         inputPath = file.path(mainPath, "inputs"),
         modulePath = file.path(mainPath, "modules"),
         outputPath = file.path(mainPath, "outputs"))

## make a list of directory paths and check paths are ok
simPaths <- getPaths()

## Let's create a self-contained module that will simulate the species' abundance for any given period of time and frequency.
if (!dir.exists(file.path(simPaths$modulePath, "speciesAbundance"))) {
  newModule(name = "speciesAbundance", path = simPaths$modulePath)
}

if (!dir.exists(file.path(simPaths$modulePath, "temperature"))) {
  newModule(name = "temperature", path = simPaths$modulePath)
}

if (!dir.exists(file.path(simPaths$modulePath, "speciesTempLM"))) {
  newModule(name = "speciesTempLM", path = simPaths$modulePath)
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

## Simulation setup
mySim <- simInit(times = simTimes, params = simParams, 
                 modules = simModules, paths = simPaths)

moduleDiagram(mySim)

objectDiagram(mySim)

## run simulation
dev() # on Windows and Mac, this opens external device if using Rstudio, it is faster
clearPlot()
mySim2 <- spades(mySim, debug = TRUE)
