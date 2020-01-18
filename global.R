library(SpaDES)  ## should automatically download all packages in the SpaDES family and their dependencies

## decide where you're working
mainDir <- '~/SpaDES4Dummies/' # SET YOUR MAIN DIRECTORY HERE.
setPaths(cachePath = "cache",
         inputPath = "inputs",
         modulePath = "modules",
         outputPath = "outputs")

getPaths() ## check that this is what you wanted

## Let's create a self-contained module that will simulate the species' abundance for any given period of time and frequency.
if(!dir.exists(file.path(getPaths()$modulePath, "speciesAbundance"))){
  newModule(name = "speciesAbundance", path = getPaths()$modulePath)
}

## list the modules to use
simModules <- list("speciesAbundance", "temperature", "speciesTempLM")

## Set simulation and module parameters
simTimes <- list(start = 1.0, end = 10.1, timeunit = "year")
simParams <- list(
  .globals = list(simulationTimeStep = 1, .plotInitialTime = 1.5),
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
spades(mySim, debug = TRUE)
