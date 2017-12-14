library(SpaDES)  ## should automatically download all packages in the SpaDES family and their dependencies

## decide where you're working
mainDir <- 'C:/Users/MrMe/SpaDES4Dummies/' # SET YOUR MAIN DIRECTORY HERE.
setPaths(cachePath = "cache",
         inputPath = "inputs",
         modulePath = "m",
         outputPath = "outputs")

getPaths() ## check that this is what you wanted

## Let's create a self-contained module that will simulate the species' abundance for any given period of time and frequency.
if(!dir.exists(file.path(getPaths()$modulePath, "speciesAbundance"))){
  newModule(name = "speciesAbundance", path = getPaths()$modulePath)
}

## list the modules to use
modules <- list("speciesAbundance", "temperature", "speciesTempLM")

## Set simulation and module parameters
times <- list(start = 1.0, end = 10.1, timeunit = "year")
parameters <- list(
  .globals = list(simulationTimeStep = 1, .plotInitialTime = 1.5),
  speciesTempLM = list(statsTimestep = 5)
)

## make a list of directory paths
paths <- getPaths()

## Simulation setup
mySim <- simInit(times = times, params = parameters, 
                 modules = modules, paths =  paths)

moduleDiagram(mySim)

objectDiagram(mySim)

## run simulation
dev() # on Windows and Mac, this opens external device if using Rstudio, it is faster
clearPlot()
<<<<<<< HEAD
spades(mySim, debug = TRUE)
=======
spades(mySim, debug = TRUE)
>>>>>>> development
