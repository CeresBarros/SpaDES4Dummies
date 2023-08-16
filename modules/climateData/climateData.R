## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "climateData",
  description = paste("Data module to prepare climate data for species distribution modelling.", 
                      "Defaults to using bioclimatic variables from Worldclim."),
  keywords = c("minimal SpaDES example", "species distribution model"),
  authors = structure(list(list(given = c("Ceres"), family = "Barros", role = c("aut", "cre"), email = "ceres.barros@ubc.ca", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(climateData = "1.0.0"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "climateData.Rmd"), ## same file
  reqdPkgs = list("SpaDES.core (>=2.0.2)",
                  "ggplot2", "rasterVis", "terra", "data.table"),
  parameters = bindrows(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
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
    expectsInput("baselineClimateURLs", "data.table", 
                 desc = paste("A table with columns 'vars', 'URL', 'targetFile' and 'year', containing",
                              "variable names, URLs and raster file names of each climate covariate",
                              "used in the species distribution models. Year is the first year of the", 
                              "simulation (not the reference climate period). Defaults to Worldclim's",
                              "'bio1', 'bio4', 'bio12' and 'bio15' bioclimatic variables for the 1970-2000",
                              "climate period, at 2.5 minutes.")),
    expectsInput("projClimateURLs", "data.table", 
                 desc = paste("Same as `baselineClimateURLs` but refering to projected climate layers.",
                              "Variable names in 'vars' need to the same as in `baselineClimateURLs`",
                              "and P(sim)$projClimateURLs. Years should correspond to simulation years.",
                              "Defaults to 2081-2100 projections using the CanESM5 climate model and the",
                              "SSP 585 climate scenario, at 2.5 minutes, obtained from Worldclim.")),
    expectsInput("studyAreaRas", objectClass = "SpatRaster", 
                 desc = "A binary raster of the study area")
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput("climateDT", "data.table", 
                  desc = paste("A data.table with as many columns as the climate covariates", 
                               "used in the species distribution model and 'year' column describing",
                               "the simulation year to which the data corresponds.")),
    createsOutput("baselineClimateRas", "SpatRaster", 
                  desc = paste("Baseline climate layers obtained from `baselineClimateURLs`")),
    createsOutput("projClimateRas", "SpatRaster", 
                  desc = paste("Baseline climate layers obtained from `projClimateURLs`"))
  )
))

## event types
#   - type `init` is required for initialization

doEvent.climateData = function(sim, eventTime, eventType, debug = FALSE) {
  switch(
    eventType,
    init = {
      ## do stuff for this event
      sim <- climateInit(sim)
      
      ## schedule future event(s)
      sim <- scheduleEvent(sim, eventTime = P(sim)$.plotInitialTime, 
                           moduleName = "climateData", eventType = "climPlot",
                           eventPriority = .normal())
    },
    climPlot = {
      ## do stuff for this event
      sim <- climatePlot(sim)
    },
    warning(paste("Undefined event type: '", current(sim)[1, "eventType", with = FALSE],
                  "' in module '", current(sim)[1, "moduleName", with = FALSE], "'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

## Initialisation Event function
climateInit <- function(sim) {
  ## GET BASELINE DATA
  ## make a vector of archive (zip) file names if the url points to one.
  archiveFiles <- sapply(sim$baselineClimateURLs$URL, function(URL) {
    if (grepl("\\.zip$", basename(URL))) {
      basename(URL)
    } else {
      NULL
    }
  }, USE.NAMES = FALSE)
  
  ## check that baseline climate data only has one year value
  if (length(unique(sim$baselineClimateURLs$year)) != 1) {
    stop(paste("'baselineClimateURLs' should all have the same 'year' value,",
               "corresponding to the first year of the simulation"))
  }
  ## download data - prepInputs does all the heavy-lifting of dowloading and pre-processing the layer and caches.
  baselineClimateRas <- Cache(Map, 
                              f = prepInputs,
                              url = sim$baselineClimateURLs$URL,
                              targetFile = sim$baselineClimateURLs$targetFile,
                              archive = archiveFiles,
                              MoreArgs = list(
                                fun = "terra::rast",
                                overwrite = TRUE,
                                projectTo = sim$studyAreaRas,
                                cropTo = sim$studyAreaRas,
                                maskTo = sim$studyAreaRas,
                                rasterToMatch = sim$studyAreaRas,
                                cacheRepo = cachePath(sim)),
                              cacheRepo = cachePath(sim))
  
  names(baselineClimateRas) <- paste0(sim$baselineClimateURLs$vars, "_year", sim$baselineClimateURLs$year)
  
  ## make a stack
  baselineClimateRas <- rast(baselineClimateRas)
  
  ## make a data.table 
  baselineClimateData <- as.data.table(as.data.frame(baselineClimateRas, xy = TRUE, cells = TRUE))
  setnames(baselineClimateData, sub("_year.*", "", names(baselineClimateData))) ## don't need year in names here 
  baselineClimateData[, year := unique(sim$baselineClimateURLs$year)]
  
  ## GET PROJECTED DATA
  ## make a vector of archive (zip) file names if the url points to one.
  archiveFiles <- lapply(sim$projClimateURLs$URL, function(URL) {
    if (grepl("\\.zip$", basename(URL))) {
      basename(URL)
    } else {
      NULL
    }
  })
  
  ## download data - prepInputs does all the heavy-lifting of dowloading and pre-processing the layer and caches.
  ## workaround Mar 30th 2022 cache issue with terra.
  projClimateRas <- Cache(Map, 
                          f = prepInputs,
                          url = sim$projClimateURLs$URL,
                          targetFile = sim$projClimateURLs$targetFile,
                          archive = archiveFiles,
                          MoreArgs = list(
                            overwrite = TRUE,
                            fun = "raster::stack",
                            projectTo = sim$studyAreaRas,
                            cropTo = sim$studyAreaRas,
                            maskTo = sim$studyAreaRas,
                            rasterToMatch = sim$studyAreaRas,
                            cacheRepo = cachePath(sim)),
                          cacheRepo = cachePath(sim))
  if (any(sapply(projClimateRas, function(x) is(x, "RasterLayer") | is(x, "RasterStack")))){
    projClimateRas <- lapply(projClimateRas, terra::rast)
  }
  
  ## these rasters are different. The tif file contains all the variables in different layers
  ## so, for each variable, we need to keep only the layer of interest
  projClimateRas <- mapply(function(stk, var) {
    lyr <- which(sub(".*_", "BIO", names(projClimateRas[[1]])) == var)
    return(stk[[lyr]])
  }, stk = projClimateRas, var = sim$projClimateURLs$vars)
  names(projClimateRas) <- paste0(sim$projClimateURLs$vars, "_year", sim$projClimateURLs$year)
  
  ## make a stack
  projClimateRas <- rast(projClimateRas)
  
  ## make a data.table 
  projClimateData <- as.data.table(as.data.frame(projClimateRas, xy = TRUE, cells = TRUE))
  
  ## melt so that year is in a column
  projClimateDataMolten <- lapply(unique(sim$projClimateURLs$vars), function(var, projClimateData) {
    cols <- grep(paste0(var, "_year"), names(projClimateData), value = TRUE)
    idCols <- names(projClimateData)[!grepl("_year", names(projClimateData))]
    
    moltenDT <-  melt(projClimateData, id.vars = idCols, measure.vars = cols, 
                      variable.name = "year", value.name = var)
    moltenDT[, year := sub(paste0(var, "_year"), "", year)]
    moltenDT[, year := as.integer(year)]
    return(moltenDT)
  }, projClimateData = projClimateData)
  
  idCols <- c(names(projClimateData)[!grepl("_year", names(projClimateData))], "year")
  ## set keys for merge
  projClimateDataMolten <- lapply(projClimateDataMolten, function(DT, cols) {
    setkeyv(DT, cols = cols)
    return(DT)
  }, cols = idCols)
  
  projClimateData <- Reduce(merge, projClimateDataMolten)
  
  ## bind the two data.tables
  if (!identical(sort(names(baselineClimateData)), sort(names(projClimateData)))) {
    stop("Variable names in `projClimateURLs` differ from those in `baselineClimateURLs`")
  }
  
  ## check
  if (!compareGeom(baselineClimateRas, projClimateRas, res = TRUE, stopOnError = FALSE)) {
    stop("`baselineClimateRas` and `projClimateRas` do not have the same raster properties")
  }
  
  ## export to sim
  sim$baselineClimateRas <- baselineClimateRas
  sim$projClimateRas <- projClimateRas
  sim$climateDT <- rbindlist(list(baselineClimateData, projClimateData), use.names = TRUE)
  
  return(invisible(sim))
}

## Plotting event function 
climatePlot <- function(sim) {
  ## plot climate rasters 
  allRasters <- rast(list(sim$baselineClimateRas, sim$projClimateRas))
  lapply(sim$baselineClimateURLs$vars, function(var, allRasters) {
    lrs <- grep(paste0(var, "_"), names(allRasters))
    file_name <- paste0("climateRas_", var)
    Plots(allRasters[[lrs]],
          fn = plotSpatRasterStk, types = P(sim)$.plots,
          usePlot = FALSE,
          filename = file.path(outputPath(sim), "figures", file_name),
          xlab = "Longitude", ylab = "Latitude")
  }, allRasters = allRasters)
  
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
  
  if (!is(sim$studyAreaRas, "SpatRaster")) {
    sim$studyAreaRas <- rast(sim$studyAreaRas)
  }
  
  if (!suppliedElsewhere(sim$baselineClimateURLs)) {
    sim$baselineClimateURLs <- data.table(
      vars = c("BIO1", "BIO4", "BIO12", "BIO15"),
      URL = c("https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_2.5m_bio.zip",
              "https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_2.5m_bio.zip",
              "https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_2.5m_bio.zip",
              "https://biogeo.ucdavis.edu/data/worldclim/v2.1/base/wc2.1_2.5m_bio.zip"),
      targetFile = c("wc2.1_2.5m_bio_1.tif", "wc2.1_2.5m_bio_4.tif", 
                     "wc2.1_2.5m_bio_12.tif", "wc2.1_2.5m_bio_15.tif"),
      year = rep(1L, 4)
    )
  }
  
  if (!suppliedElsewhere(sim$projClimateURLs)) {
    sim$projClimateURLs <- data.table(
      vars = rep(c("BIO1", "BIO4", "BIO12", "BIO15"), times = 4),
      URL = rep(c("https://geodata.ucdavis.edu/cmip6/2.5m/CanESM5/ssp585/wc2.1_2.5m_bioc_CanESM5_ssp585_2021-2040.tif",
                  "https://geodata.ucdavis.edu/cmip6/2.5m/CanESM5/ssp585/wc2.1_2.5m_bioc_CanESM5_ssp585_2041-2060.tif",
                  "https://geodata.ucdavis.edu/cmip6/2.5m/CanESM5/ssp585/wc2.1_2.5m_bioc_CanESM5_ssp585_2061-2080.tif",
                  "https://geodata.ucdavis.edu/cmip6/2.5m/CanESM5/ssp585/wc2.1_2.5m_bioc_CanESM5_ssp585_2081-2100.tif"),
                each = 4),
      targetFile = rep(c("wc2.1_2.5m_bioc_CanESM5_ssp585_2021-2040.tif",
                         "wc2.1_2.5m_bioc_CanESM5_ssp585_2041-2060.tif",
                         "wc2.1_2.5m_bioc_CanESM5_ssp585_2061-2080.tif",
                         "wc2.1_2.5m_bioc_CanESM5_ssp585_2081-2100.tif"),
                       each = 4),
      year = rep(2L:5L, each = 4)
    )
  } 
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
