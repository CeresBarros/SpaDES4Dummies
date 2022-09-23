## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "projectSpeciesDist",
  description = "",
  keywords = "",
  authors = structure(list(list(given = c("Ceres"), family = "Barros", role = c("aut", "cre"), email = "ceres.barros@ubc.ca", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(projectSpeciesDist = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("README.md", "projectSpeciesDist.Rmd"), ## same file
  reqdPkgs = list("PredictiveEcology/SpaDES.core@development (>=1.0.10.9000)", "ggplot2",
                  "data.table", "dismo", "rJava", "rasterVis"),
  parameters = bindrows(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("predVars", "character", c("BIO1", "BIO4", "BIO12", "BIO15"), NA, NA,
                    "Predictors used in statistical model."),
    defineParameter("statModel", "character", "MaxEnt", NA, NA,
                    paste("What statitical algorith to use. Currently only 'MaxEnt' and 'GLM' are",
                          "supported. 'MaxEnt will fit a MaxEnt model using dismo::maxent; 'GLM'",
                          "will fit a generalised linear model with a logit link using",
                          "glm(..., family = 'binomial'). In both cases all predictor variables are used,",
                          "and for GLM only additive effects are considered." )),
    defineParameter(".plots", "character", "screen", NA, NA,
                    "Used by Plots function, which can be optionally used here"),
    defineParameter(".plotInitialTime", "numeric", start(sim), NA, NA,
                    "Describes the simulation time at which the first plot event should occur."),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?")
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput("climateDT", "data.table", 
                 desc = paste("A data.table with as many columns as the climate covariates", 
                              "used in the species distribution model and 'year' column describing",
                              "the simulation year to which the data corresponds.")),
    expectsInput("sppAbundanceDT", "data.table", 
                 desc = paste("A species abundance data. Converted to presence/absence data, if not binary")),
    expectsInput("studyAreaRas", objectClass = "RasterLayer", 
                 desc = "A binary raster of the study area")
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "sppDistProj", objectClass = "SpatRaster",
                  desc = paste("Species distribution projections - raw predictions.",
                               "Each layer corresponds to a prediciton year")),
    createsOutput(objectName = "evalOut", objectClass = "ModelEvaluation", 
                  desc = paste("`sdmOut` model evaluation statistics. Model evaluated on the 20% of",
                               "the data. See `?dismo::evaluation`.")),
    createsOutput(objectName = "sdmData", objectClass = "data.table", 
                  desc = "Input data used to fit `sdmOut`."),
    createsOutput(objectName = "sdmOut", objectClass = c("MaxEnt", "glm"),
                  desc = paste("Fitted species distribution model. Model fitted on 80%",
                               "of `sdmData`, with remaining 20% used for evaluation.")),
    createsOutput(objectName = "thresh", objectClass = "numeric",
                  desc = paste("Threshold of presence that maximises the sum of the sensitivity",
                               "(true positive rate) and specificity (true negative rate).",
                               "See `dismo::threshold(..., stat = 'spec_sens')`."))
  )
))

## event types
#   - type `init` is required for initialization

doEvent.projectSpeciesDist = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)
      
      # do stuff for this event
      sim <- SDMInit(sim)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, start(sim), "projectSpeciesDist", "fitSDM")
      sim <- scheduleEvent(sim, start(sim), "projectSpeciesDist", "evalSDM", 
                           eventPriority = .normal() + 1)
      sim <- scheduleEvent(sim, start(sim), "projectSpeciesDist", "projSDM", 
                           eventPriority = .normal() + 2)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "projectSpeciesDist", "plotProjSDM",
                           eventPriority = .normal() + 3)
      
    },
    fitSDM = {
      # ! ----- EDIT BELOW ----- ! #
      sim <- fitSDMEvent(sim)
      # ! ----- STOP EDITING ----- ! #
    },
    evalSDM = {
      # ! ----- EDIT BELOW ----- ! #
      sim <- evalSDMEvent(sim)
      # ! ----- STOP EDITING ----- ! #
    },
    projSDM = {
      # ! ----- EDIT BELOW ----- ! #
      sim <- projSDMEvent(sim)
      
      sim <- scheduleEvent(sim, time(sim) + 1L, "projectSpeciesDist", "projSDM")
      # ! ----- STOP EDITING ----- ! #
    },
    plotProjSDM = {
      # ! ----- EDIT BELOW ----- ! #
      plotProjEvent(sim)
      
      sim <- scheduleEvent(sim, time(sim) + 1L, "projectSpeciesDist", "plotProjSDM",
                           eventPriority = .normal() + 1)
      
      # ! ----- STOP EDITING ----- ! #
    },
    warning(paste("Undefined event type: \'", current(sim)[1, "eventType", with = FALSE],
                  "\' in module \'", current(sim)[1, "moduleName", with = FALSE], "\'", sep = ""))
  )
  return(invisible(sim))
}

## event functions
#   - keep event functions short and clean, modularize by calling subroutines from section below.

### template initialization
SDMInit <- function(sim) {
  # # ! ----- EDIT BELOW ----- ! #
  ## at this point we can only have the following columns
  if (!identical(sort(names(sim$sppAbundanceDT)), sort(c("cell", "x", "y", "sppAbund", "year")))) {
    stop(paste("sim$sppAbundanceDT can only have the following columns at the start of year 1:\n",
               paste(c("cell", "x", "y", "sppAbund", "year"), collapse = ", ")))
  }
  
  if (length(setdiff(sim$climateDT$cell, sim$sppAbundanceDT$cell)) > 0 ||
      length(setdiff(sim$sppAbundanceDT$cell, sim$climateDT$cell)) > 0) {
    stop("'cell' columns in `climateDT` and `sppAbundanceDT` have different values")
  }
  
  if (!P(sim)$statModel %in% c("MaxEnt", "GLM")) {
    stop("'statModel' parameter must be 'MaxEnt' or 'GLM'")
  }
  
  ## a few data cleaning steps to make sure we have presences and absences:
  sppAbundanceDT <- copy(sim$sppAbundanceDT)
  if (min(range(sppAbundanceDT$sppAbund)) < 0) {
    sppAbundanceDT[sppAbund < 0, sppAbund := 0]
  }
  
  if (max(range(sppAbundanceDT$sppAbund)) > 1) {
    message("Species data is > 1. Converting to presence/absence")
    sppAbundanceDT[sppAbund > 0, sppAbund := 1]
  }
  
  ## join the two datasets - note that there are no input species abundances beyond year 1
  sim$sdmData <- merge(sim$climateDT, sppAbundanceDT[, .(cell, sppAbund, year)], 
                       by = c("cell", "year"), all = TRUE)
  setnames(sim$sdmData, "sppAbund", "presAbs")
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

fitSDMEvent <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  ## break data into training and testing subsets
  dataForFitting <- sim$sdmData[year == time(sim)]
  
  if (nrow(dataForFitting) == 0) {
    stop(paste("No data for year", time(sim), "provided to fit the model"))
  }
  
  group <- kfold(dataForFitting, 5)
  ## save the the split datasets as internal objects to this module
  mod$trainData <- dataForFitting[group != 1, ]
  mod$testData <-  dataForFitting[group == 1, ]
  
  predVars <- P(sim)$predVars
  if (P(sim)$statModel == "MaxEnt") {
    sim$sdmOut <- maxent(x = as.data.frame(mod$trainData[, ..predVars]), 
                         p = mod$trainData$presAbs)
  } else {
    ## make an additive model with all predictors - avoid using as.formula, which drags the whole environment
    form <- enquote(paste("presAbs ~", paste(predVars, collapse = "+")))
    sim$sdmOut <- glm(formula = eval(expr = parse(text = form)), 
                      family = "binomial", data = mod$trainData)
  }
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

evalSDMEvent <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  ## validate model
  predVars <- P(sim)$predVars
  sim$evalOut <- evaluate(p = mod$testData[presAbs == 1, ..predVars],
                          a = mod$testData[presAbs == 0, ..predVars],
                          model = sim$sdmOut)
  ## save the threshold of presence/absence in an internal object to this module
  sim$thresh <- threshold(sim$evalOut, 'spec_sens')
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

projSDMEvent <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  ## predict across the full data and make a map
  dataForPredicting <- sim$sdmData[year == time(sim)]
  
  if (nrow(dataForPredicting) == 0) {
    stop(paste("No data for year", time(sim), "provided to calculate predictions"))
  }
  
  predVars <- P(sim)$predVars
  preds <- predict(sim$sdmOut, as.data.frame(dataForPredicting[, ..predVars]),
                   progress = '')
  sppDistProj <- replace(sim$studyAreaRas, which(!is.na(sim$studyAreaRas[])), preds)
  names(sppDistProj) <- paste0("year", time(sim))
  
  if (is.null(sim$sppDistProj)) {
    sim$sppDistProj <- sppDistProj
  } else {
    sim$sppDistProj <- rast(list(sim$sppDistProj, sppDistProj))
  }
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

plotProjEvent <- function(sim) {
  # ! ----- EDIT BELOW ----- ! #
  checkPath(file.path(outputPath(sim), "figures"), create = TRUE)
  
  if (any(!is.na(P(sim)$.plots))) {
    
    ## response plot
    ## we can't use Plots to plot and save SDM predictions with dismo.
    ## these are only saved to disk
    fileSuffix <- paste0(P(sim)$statModel, ".png")
  
      notScreen <- setdiff(P(sim)$.plots, "screen")
    if (any(notScreen != "png")) {
      warning(paste(currentModule(sim), "only saves to PNG at the moment."))
    } 
    png(file.path(outputPath(sim), "figures", paste0("SDMresponsePlot_", fileSuffix)))
    response(sim$sdmOut)
    dev.off()
    
    ## species projections
    fileSuffix <- paste0(P(sim)$statModel, "_Year", time(sim))
    clearPlot()
    rawValsPlot <- sim$sppDistProj[[paste0("year", time(sim))]]
    Plots(rawValsPlot, fn = plotSpatRaster, types = P(sim)$.plots,
          usePlot = TRUE, filename = file.path(outputPath(sim), "figures", paste0("projRawVals_", fileSuffix)), 
          plotTitle = paste("Projected raw values -", "year", time(sim)),
          xlab = "Longitude", ylab = "Latitude")
    PAsPlot <- sim$sppDistProj[[paste0("year", time(sim))]] > sim$thresh
    Plots(PAsPlot, fn = plotSpatRaster, types = P(sim)$.plots,
          usePlot = TRUE, filename = file.path(outputPath(sim), "figures", paste0("projPA_", fileSuffix)), 
          plotTitle = paste("Projected presence/absence -", "year", time(sim)),
          xlab = "Longitude", ylab = "Latitude")
  }
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

.inputObjects <- function(sim) {
  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")
  
  # ! ----- EDIT BELOW ----- ! #
  ## check that necessary objects are in the simList or WILL BE supplied  by another module
  if (!suppliedElsewhere("climateDT") | !suppliedElsewhere("sppAbundanceDT") ) {
    stop("Please provide `climateDT` and `sppAbundanceDT`")
  }
  
  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}
