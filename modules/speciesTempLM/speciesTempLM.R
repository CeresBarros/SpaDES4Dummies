
# Everything in this file gets sourced during simInit, and all functions and objects
# are put into the simList.
defineModule(sim, list(
  name = "speciesTempLM",
  description = "Statistical analysis of species ~ temperature relationships using LM",
  keywords = c("linear model"),
  authors = person("Mr.", "Me", email = "mr.me@example.com", role = c("aut", "cre")),
  childModules = character(0),
  version = list(SpaDES.core = "0.1.1.9011", speciesAbundance = "0.0.1"),
  # spatialExtent = raster::extent(rep(NA_real_, 4)),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.txt", "speciesTempLM.Rmd"),
  reqdPkgs = list("raster", "ggplot2"),
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
      sim <- scheduleEvent(sim, P(sim)$statsTimestep + 0.1, "speciesTempLM", "stats")
      sim <- scheduleEvent(sim, P(sim)$statsTimestep + 0.1, "speciesTempLM", 
                           "statsPlot", eventPriority = .normal()+1)
    },
    statsPlot = {
      ## do stuff for this event
      sim <- statsPlot(sim)
      
      ## schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$statsTimestep, "speciesTempLM",
                           "statsPlot", eventPriority = .normal()+1)
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

  plotLMResults(Data = sim$outputdata[[as.character(time(sim))]], model = sim$outputLM[[as.character(time(sim))]])
  
  return(invisible(sim))
}

## Statistical analysis event
statsAnalysis <- function(sim) {
  
  sim$yrs <- seq(time(sim) - P(sim)$statsTimestep + 1, time(sim), 1)

  sim$outputdata[[as.character(time(sim))]] <- do.call(rbind.data.frame, 
                                         lapply(sim$yrs, FUN = function(y){
                                           temp <- data.frame(abund = sim$abundRasters[[y]][], 
                                                              temp = sim$tempRasters[[y]][], year = y)          
                                           return(temp)
                                         }))
  
  sim$outputLM[[as.character(time(sim))]] <- linearModel(Data = sim$outputdata[[as.character(time(sim))]])
  
  return(invisible(sim))
}

## Other functions
linearModel <- function(Data){
  return(lm1 <- lm(abund ~ temp, data = Data))
}

plotLMResults <- function(Data, model){
  plot1 <- ggplot(data = Data, aes(x = temp, y = abund)) +
    geom_point() +
    geom_abline(slope = model$coefficients["temp"], 
                intercept = model$coefficients["(Intercept)"],
                col = "blue", size = 1) +
    theme_bw() +
    labs(x = "Temp.", y = "Species abundance", 
         title = paste("From years", min(Data$year)-0.1, "to", max(Data$year)-0.1, sep = " "))
  Plot(plot1, new = TRUE, title = "Stats. plot")
}


.inputObjects <- function(sim) {
  if(!suppliedElsewhere(sim$abundRasters)) {
    ## make dummy abundance raster list if not supplied elsewhere.
    r <- raster(nrows = 100, ncols = 100, xmn = -50, xmx = 50, ymn = -50, ymx = 50)
    sim$abundRasters <- lapply(seq(start(sim):end(sim), 1), 
                               SpaDES.tools::gaussMap(x = sim$r, scale = 100, var = 0.01))
    }
  
  if(!suppliedElsewhere(sim$tempRasters)) {
    ## make dummy temperature raster list if not supplied elsewhere.
    r <- raster(nrows = 100, ncols = 100, xmn = -50, xmx = 50, ymn = -50, ymx = 50)
    sim$tempRasters <- lapply(seq(start(sim):end(sim), 1),
                              SpaDES.tools::gaussMap(x = sim$r, scale = 100, var = 0.01))
    }
  
  return(invisible(sim))
}