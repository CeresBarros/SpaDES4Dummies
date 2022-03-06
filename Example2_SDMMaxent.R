## Get necessary R packages, but don't load any
if (!require("Require")) install.packages("Require")
Require::Require("PredictiveEcology/SpaDES.install@development")
# SpaDES.install::installSpaDES(ask = TRUE)

## decide where you're working
mainDir <- "."

# mainDir <- getwd()
SpaDES.core::setPaths(cachePath = file.path(mainDir, "cache"),
                      inputPath = file.path(mainDir, "inputs"),
                      modulePath = file.path(mainDir, "modules"),
                      outputPath = file.path(mainDir, "outputs"))

simPaths <- SpaDES.core::getPaths() ## check that this is what you wanted

## Let's create a self-contained module that will simulate the species' abundance for any given period of time and frequency.
if (!dir.exists(file.path(simPaths$modulePath, "speciesAbundanceData"))){
  SpaDES.core::newModule(name = "speciesAbundanceData", path = simPaths$modulePath)
}

if (!dir.exists(file.path(simPaths$modulePath, "climateData"))){
  SpaDES.core::newModule(name = "climateData", path = simPaths$modulePath)
}

if (!dir.exists(file.path(simPaths$modulePath, "projectSpeciesDist"))){
  SpaDES.core::newModule(name = "projectSpeciesDist", path = simPaths$modulePath)
}

## now, let's pretend you've created your modules and each sources a series of other packages
## it's a good idea to always make sure all necessary module dependencies are installed
## this is a particularly useful line when sharing your pacakges with someone else.
SpaDES.install::makeSureAllPackagesInstalled(simPaths$modulePath)

## you shoul restart R again if any packages were installed

## load necessary packages now
library(SpaDES)

## a few important options:
options(reproducible.useCache = TRUE,
        reproducible.cachePath = simPaths$cachePath,
        reproducible.destinationPath = simPaths$inputPath, ## all downloaded and pre-processed layers go here
        reproducible.useTerra = TRUE)  ## we want to use the terra R package

## list the modules to use
simModules <- list("speciesAbundanceData", "climateData", "projectSpeciesDist")

## Set simulation and module parameters
simTimes <- list(start = 1, end = 2, timeunit = "year")
simParams <- list(
  "speciesAbundanceData" = list(
    ".plots" = c("screen", "png"),
    ".useCache" = c(".inputObjects", "init")
  ),
  "climateData" = list(
    ".plots" = c("screen", "png"),
    ".useCache" = c(".inputObjects", "init")
  ),
  "projectSpeciesDist" = list(
    ".plots" = c("screen", "png"),
    ".useCache" = c(".inputObjects", "init")
  )
)

## make a random study area.
##  Here use seed to make sure the same study area is always generated
studyArea <- terra::vect(randomStudyArea(size = 1e10, seed = 123))
studyAreaRas <- terra::rasterize(studyArea, 
                                 terra::rast(extent = terra::ext(studyArea), 
                                             crs = terra::crs(studyArea, proj = TRUE), 
                                             resolution = 1000))
simObjects <- list(
  "studyAreaRas" = studyAreaRas
)

## Simulation setup
mySim <- simInit(times = simTimes, params = simParams, 
                 modules = simModules, objects = simObjects, 
                 paths = simPaths)
clearPlot(force = TRUE)   ## this forces wiping the graphics device and opening a new window
moduleDiagram(mySim)
objectDiagram(mySim)

## run simulation
clearPlot(force = TRUE)   ## this forces wiping the graphics device and opening a new window
spades(mySim, debug = TRUE)
