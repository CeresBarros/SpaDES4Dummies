## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "speciesAbundanceData",
  description = paste("Data module to prepare tree species cover data for species distribution modelling.", 
                      "Defaults to using Canadian National Forest Inventory data."),
  keywords = c("minimal SpaDES example", "species distribution model"),
  authors = structure(list(list(given = c("Ceres"), family = "Barros", role = c("aut", "cre"), 
                                email = "ceres.barros@ubc.ca", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(speciesAbundanceData = "1.0.0"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "speciesAbundanceData.Rmd"), ## same file
  reqdPkgs = list("SpaDES.core (>=2.0.2)",
                  "httr", "terra", "ggplot2", "rasterVis"),
  parameters = bindrows(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("sppAbundURL", "character", 
                    paste0("https://ftp.maps.canada.ca/pub/nrcan_rncan/Forests_Foret/",
                           "canada-forests-attributes_attributs-forests-canada/",
                           "2001-attributes_attributs-2001/",
                           "NFI_MODIS250m_2001_kNN_Species_Pice_Gla_v1.tif"), NA, NA,
                    paste("URL where the first SpatRaster of species abundance resides.",
                          "This will be the abundance data used to fit the species ditribution model.",
                          "Defaults to *Picea glauca* percent cover across Canada, in 2001", 
                          "(from Canadian National Forest Inventory forest attributes)")),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    defineParameter(".plotInterval", "numeric", NA, NA, NA,
                    "Describes the simulation time interval between plot events."),
    defineParameter(".saveInitialTime", "numeric", NA, NA, NA,
                    "Describes the simulation time at which the first save event should occur."),
    defineParameter(".saveInterval", "numeric", NA, NA, NA,
                    "This describes the simulation time interval between save events."),
    defineParameter(".studyAreaName", "character", NA, NA, NA,
                    "Human-readable name for the study area used. If NA, a hash of studyArea will be used."),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?")
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput("studyAreaRas", objectClass = "SpatRaster", 
                 desc = "A binary raster of the study area")
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput("sppAbundanceDT", "data.table", 
                  desc = paste("Species abundance data from `sppAbundanceRas`, with columns 'cell',",
                               "'x', 'y', 'sppAbund' and 'year' (an integer matching the number in",
                               "names(`sppAbundanceRas`)." )),
    createsOutput("sppAbundanceRas", "SpatRaster", 
                  desc = paste("A species abundance layer used to fit a species distribution model",
                               "at the start of the simulation. Layers named as:",
                               "paste('year', start(sim):end(sim), sep = '_')). Data obtained from",
                               "P(sim)$sppAbundURL"))
  )
))

## event types
#   - type `init` is required for initialization

doEvent.speciesAbundanceData = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      ## do stuff for this event
      sim <- abundanceInit(sim)
      
      ## schedule future event(s)
      sim <- scheduleEvent(sim, eventTime = P(sim)$.plotInitialTime, 
                           moduleName = "speciesAbundanceData", eventType = "abundPlot",
                           eventPriority = .normal())
    },
    abundPlot = {
      ## do stuff for this event
      sim <- abundancePlot(sim)
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

## Initialisation Event function
abundanceInit <- function(sim) {
  ## download data - prepInputs does all the heavy-lifting of dowloading and pre-processing the layer and caches.
  ## there seems to be an issue masking this particular raster with `terra` and `GDAL`, so we'll not use them here.
  opts <- options("reproducible.useTerra" = FALSE,
                  "reproducible.useGDAL" = FALSE)   
  on.exit(options(opts), add = TRUE)

  httr::with_config(config = httr::config(ssl_verifypeer = 0L), {
    sppAbundanceRas <- prepInputs(targetFile = "NFI_MODIS250m_2001_kNN_Species_Pice_Gla_v1.tif",
                                  url = P(sim)$sppAbundURL,
                                  # fun = "terra::rast",
                                  # projectTo = sim$studyAreaRas,
                                  # cropTo = sim$studyAreaRas,
                                  # maskTo = sim$studyAreaRas,
                                  rasterToMatch = raster::raster(sim$studyAreaRas),
                                  maskWithRTM = TRUE,
                                  overwrite = TRUE,
                                  cacheRepo = cachePath(sim))
  })
  
  options(opts)
  
  if (is(sppAbundanceRas, "RasterLayer")) {
    sppAbundanceRas <- terra::rast(sppAbundanceRas)
  }
  
  names(sppAbundanceRas) <- paste("year", time(sim), sep = "_")
  sppAbundanceDT <- as.data.table(as.data.frame(sppAbundanceRas, xy = TRUE, cells = TRUE))
  sppAbundanceDT[, year := as.integer(sub("year_", "", names(sppAbundanceRas)))]
  setnames(sppAbundanceDT, "year_1", "sppAbund")
  
  ## export to sim
  sim$sppAbundanceRas <- sppAbundanceRas
  sim$sppAbundanceDT <- sppAbundanceDT
  
  return(invisible(sim))
}

## Plotting event function 
abundancePlot <- function(sim) {
  ## plot species abundance
  Plots(sim$sppAbundanceRas, fn = plotSpatRaster, types = P(sim)$.plots,
        usePlot = TRUE, filename = file.path(outputPath(sim), "figures", "speciesAbundance"), 
        plotTitle = "Species abundance data", xlab = "Longitude", ylab = "Latitude")
  
  return(invisible(sim))
}


.inputObjects <- function(sim) {
  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  
  # ! ----- EDIT BELOW ----- ! #
  
  if (!suppliedElsewhere(sim$studyAreaRas)) {
    ## code check: did the user supply a study area?
    stop("Please supply a 'studyAreaRas' SpatRaster")
  }
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
