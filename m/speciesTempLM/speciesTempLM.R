
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList.
defineModule(sim, list(
  name = "speciesTempLM",
  description = "Statistical analysis of species ~ temperature relationships using LM",
  keywords = c("linear model"),
  authors = person("Mr.", "Me", email = "mr.me@example.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.0", speciesTempLM = "0.0.1", speciesAbundance = "0.0.1", temperature = "0.0.1", SpaDES.addins = "0.1.0", SpaDES.tools = "0.1.0"),
  spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "speciesTempLM.Rmd"),
  reqdPkgs = list("raster"),
  parameters = rbind(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("statsTimestep", "numeric", 1, NA, NA, "This describes the how often the statitiscal analysis will be done")
  ),
  inputObjects = bind_rows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput( "abundRasters", "list", "List of raster layers of species abundance at any given year"),
    expectsInput( "tempRasters", "list", "List of raster layers of temperature at any given year")
  ),
  outputObjects = bind_rows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput("outputdata", "list", "List of dataframes containing species abundances and temperature values per pixel"),
    createsOutput( "outputLM", "list", "List of output yearly LMs (abundance ~ temperature)"),
    createsOutput( "yrs", "numeric", "Vector of years used for statistical analysis")
  )
))

## event types
#   - type `init` is required for initialiazation

doEvent.speciesTempLM = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      ## do stuff for this event
      sim <- statsInit(sim)

      ## schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$statsTimestep, "speciesTempLM", "stats")
      sim <- scheduleEvent(sim, P(sim)$statsTimestep, "speciesTempLM", "plot")
    },
    plot = {
      ## do stuff for this event
      sim <- statsPlot(sim)
      
      ## schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$statsTimestep, "speciesTempLM", "plot")
    },
    stats = {
      ## do stuff for this event
      sim <- statsAnalysis(sim)
      
      ## schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$statsTimestep, "speciesTempLM", "stats")
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

## template initialization
statsInit <- function(sim) {
  ## create outputs storage lists
  sim$outputdata <- list()
  sim$outputLM <- list()
  
  return(invisible(sim))
}

## Plotting event
statsPlot <- function(sim) {

  plotLMResults(Data = sim$outputdata[[time(sim)]], model = sim$outputLM[[time(sim)]])
  
  return(invisible(sim))
}

## Statistical analysis event
statsAnalysis <- function(sim) {
  
  yrs <- seq(time(sim) - P(sim)$statsTimestep, time(sim), 1)

  sim$outputdata[[time(sim)]] <- do.call(rbind.data.frame, 
                                         lapply(yrs, FUN = function(y){
                                           temp <- data.frame(abund = sim$abundRasters[[y]][], temp = sim$tempRasters[[y]][], year = y)          
                                         }))
  
  sim$outputLM[[time(sim)]] <- linearModel(Data = sim$outputdata[[time(sim)]])
  
  return(invisible(sim))
}


## Other functions
linearModel <- function(Data){
  return(lm1 <- lm(abund ~ temp, data = Data))
}

plotLMResults <- function(Data, model){
  plot(Data$abund ~ Data$temp, xlab = "Temp.", ylab = "Species abundance")
  abline(a = model$coefficients["(Intercept)"], b = model$coefficients["temp"], lwd = 2, col = "blue")
}
