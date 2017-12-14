
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList. To use objects and functions, use sim$xxx.
defineModule(sim, list(
  name = "speciesAbundance",
  description = "Species abundance simulator",
<<<<<<< Updated upstream
<<<<<<< HEAD
  keywords = c("species", "abundance", "gaussian", "spatial","dummy example"),
=======
  keywords = c("species", "abundance", "gaussian", "spatial","dummyExample"),
>>>>>>> development
=======
  keywords = c("species", "abundance", "gaussian", "spatial","dummyExample"),
>>>>>>> Stashed changes
  authors = person("Mr.", "Me", email = "mr.me@example.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.0", speciesAbundance = "0.0.1", SpaDES.addins = "0.1.0", SpaDES.tools = "0.1.0"),
  # spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "speciesAbundance.Rmd"),
  reqdPkgs = list("raster"),
  parameters = rbind(
    # defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("simulationTimeStep", "numeric", 1, NA, NA, "This describes the simulation time step interval"),
    defineParameter(".plotInitialTime", "numeric", 1, NA, NA, "This describes the simulation time at which the first plot event should occur"),
    defineParameter(".plotInterval", "numeric", 1, NA, NA, "This describes the simulation time interval between plot events")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "r", objectClass = "RasterLayer", desc = "Template raster"),
    createsOutput("abundRasters", "RasterLayer", "Raster layer of species abundance at any given year")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.speciesAbundance = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      ## do stuff for this event
      sim <- abundanceInit(sim)
      
      ## schedule future event(s)
      sim <- scheduleEvent(sim, eventTime = start(sim), moduleName = "speciesAbundance", eventType = "SimulAbund")
      sim <- scheduleEvent(sim, eventTime = P(sim)$.plotInitialTime, moduleName = "speciesAbundance", eventType = "plot")
    },
    plot = {
      ## do stuff for this event
      sim <- abundancePlot(sim)
      
      ## schedule future event(s)
      sim <- scheduleEvent(sim, eventTime = time(sim) + P(sim)$.plotInterval, moduleName = "speciesAbundance", eventType = "plot")
    },
    SimulAbund = {
      ## do stuff for this event
      sim <- abundanceSim(sim)
      
      ## schedule future event(s)
      sim <- scheduleEvent(sim, eventTime = time(sim) + P(sim)$simulationTimeStep, moduleName = "speciesAbundance", eventType = "SimulAbund")
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - follow the naming convention `modulenameEventtype()`;
#   - `modulenameInit()` function is required for initiliazation;
#   - keep event functions short and clean, modularize by calling subroutines from section below.

## Initialisation Event
abundanceInit <- function(sim) {
  ## Template raster
  sim$r <- raster(nrows = 100, ncols = 100, xmn = -50, xmx = 50, ymn = -50, ymx = 50)
  
  ## create storage list of species abundance
  sim$abundRasters <- list()
  
  return(invisible(sim))
}

## Plotting event
abundancePlot <- function(sim) {
  ## plot abundances
  plot(sim$abundRasters[[time(sim)]], 
       main = paste0("Species abundance\nat time ", time(sim)))
  
  return(invisible(sim))
}

## Abundance simulation event
abundanceSim <- function(sim) {
  ## Generate species abundances - our "simulation"
  sim$abundRasters[[time(sim)]] <- abundance_model(ras = sim$r)
  
  return(invisible(sim))
}

## This is not an event, but a function that we define separately 
## and that contains our "simulation model". 
<<<<<<< Updated upstream
<<<<<<< HEAD
## It is possible to specify it as a separate script (which is worth doing if the function is too big, for example).
=======
## It is possible to specify it as a separate script (which is worth it if the function is too big, for example).
>>>>>>> development
=======
## It is possible to specify it as a separate script (which is worth it if the function is too big, for example).
>>>>>>> Stashed changes
abundance_model <- function(ras) {
  abund_ras <- SpaDES.tools::gaussMap(ras, scale = 100, var = 0.01) 
  return(abund_ras)
}
