library(SpaDES)  ## should automatically download all packages in the SpaDES family and their dependencies

## decide where you're working
mainDir <- '~/SpaDES4Dummies/' # SET YOUR MAIN DIRECTORY HERE.
# mainDir <- getwd()
setPaths(cachePath = file.path(mainDir, "cache"),
         inputPath = file.path(mainDir, "inputs"),
         modulePath = file.path(mainDir, "modules"),
         outputPath = file.path(mainDir, "outputs"))

getPaths() ## check that this is what you wanted

## Let's create a self-contained module that will simulate the species' abundance for any given period of time and frequency.
if(!dir.exists(file.path(getPaths()$modulePath, "speciesAbundance"))){
  newModule(name = "speciesAbundance", path = getPaths()$modulePath)
}

if(!dir.exists(file.path(getPaths()$modulePath, "temperature"))){
  newModule(name = "temperature", path = getPaths()$modulePath)
}

if(!dir.exists(file.path(getPaths()$modulePath, "speciesTempLM"))){
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
spades(mySim, debug = TRUE)
