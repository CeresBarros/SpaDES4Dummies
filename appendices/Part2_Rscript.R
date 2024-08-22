if (getRversion() < "4.2.1") {
  warning(paste("dismo::maxent may create a fatal error",
                "when using R version < v4.2.1 and from RStudio.\n", 
                "Please upgrade R, or run this script outside of RStudio.\n",
                "See https://github.com/rspatial/dismo/issues/13"))
}

repos <- c("https://predictiveecology.r-universe.dev/",
           "https://cloud.r-project.org",
           getOption("repos"))
install.packages(c("SpaDES.project", "SpaDES.core"), repos = repos)

## decide where you're working
projPath <- file.path("~/SpaDES4Dummies_Part2")
modPath <- file.path(projPath, "modules")

## Let's create a self-contained module that will simulate the species' abundance for any given period of time and frequency.
if (!dir.exists(file.path(modPath, "speciesAbundanceData"))) {
  SpaDES.core::newModule(name = "speciesAbundanceData", path = modPath)
}

if (!dir.exists(file.path(modPath,"climateData"))) {
  SpaDES.core::newModule(name = "climateData", path = modPath)
}

if (!dir.exists(file.path(modPath, "projectSpeciesDist"))) {
  SpaDES.core::newModule(name = "projectSpeciesDist", path = modPath)
}

## assuming you've created your modules and each sources a series of other packages
## we will use setupProject to install and prepare all inputs to initialise the workflow 
library(SpaDES.project)

mySimMaxEnt <- setupProject(paths = list("packagePath" = "packages/",
                                         "projectPath" = projPath),
                            Restart = TRUE,  ## RStudio will restart. You have to run the code above and this call again
                            modules = c("speciesAbundanceData", "climateData", "projectSpeciesDist"),
                            options = list(reproducible.useCache = TRUE,
                                           reproducible.cachePath = paths$cachePath,  ## you can use "paths" which is the first argument (and become an internal object). See ?setupProject
                                           reproducible.destinationPath = paths$inputPath, ## all downloaded and pre-processed layers go here
                                           spades.moduleCodeChecks = FALSE,
                                           repos = repos),
                            packages = c(## the following packages are needed outside the modules
                              "DiagrammeR",
                              "ggpubr",
                              "SpaDES.experiment",
                              "SpaDES.tools"
                            ),
                            params = list(
                              "speciesAbundanceData" = list(
                                ".plots" = c("png"),
                                ".useCache" = c(".inputObjects", "init") 
                              ),
                              "climateData" = list(
                                ".plots" = c("png"),
                                ".useCache" = c(".inputObjects", "init")
                              ),
                              "projectSpeciesDist" = list(
                                "statModel" = "MaxEnt",
                                ".plots" = c("png"),
                                ".useCache" = c(".inputObjects", "init")
                              )
                            ),
                            times = list(start = 1, end = 5, timeunit = "year"),
                            studyAreaRas = {
                              studyArea <- SpaDES.tools::randomStudyArea(size = 1e10, seed = 123)
                              terra::rasterize(studyArea,
                                               terra::rast(extent = terra::ext(studyArea),
                                                           crs = terra::crs(studyArea, proj = TRUE),
                                                           resolution = 1000))
                            },
                            sideEffects = {
                              ## dismo needs a few tweaks
                              out <- reproducible::preProcess(targetFile = "maxent.jar",
                                                              url = "https://github.com/mrmaxent/Maxent/blob/master/ArchivedReleases/3.4.4/maxent.jar?raw=true",
                                                              destinationPath = paths$inputPath,
                                                              fun = NA)
                              file.copy(out$targetFilePath, 
                                        file.path(system.file("java", package = "dismo"), "maxent.jar"),
                                        overwrite = TRUE)
                            }
)

## check that rJava can be loaded.
if (!require(rJava, quietly = TRUE)) {
  stop(paste("Your Java installation may have problems, please check.\n", 
             "See https://www.java.com/en/download/manual.jsp for Java installation"))
}

## check study area:
terra::plot(mySimMaxEnt$studyAreaRas)

## the GLM alternative only needs one parameter to be changed, so we can avoid 
## repeating the `setupProject()` call
mySimGLM <- mySimMaxEnt
mySimGLM$params$projectSpeciesDist$statModel <- "GLM"

## Simulation setup - create two "simulations", one for MaxEnt another for GLM
## SpaDES.experiment::experiment2, will take care of subdirectories to store outputs
simInitMaxEnt <- SpaDES.core::simInit2(mySimMaxEnt)
simInitGLM <- SpaDES.core::simInit2(mySimGLM)

SpaDES.core::moduleDiagram(simInitMaxEnt)
SpaDES.core::objectDiagram(simInitMaxEnt)

## run simulation
quickPlot::clearPlot(force = TRUE)   ## this forces wiping the graphics device and opening a new window

## This runs one simulation and stores outputs in the main 'outputs' folder 
## - not what we want, but good for testing
# mySimOut <- SpaDES.core::spades(simInitMaxEnt, debug = TRUE)  

## Better to use after checking that spades() runs error-free on both simLists
myExperiment <- SpaDES.experiment::experiment2(MaxEnt = simInitMaxEnt, 
                                               GLM = simInitGLM, 
                                               debug = TRUE, 
                                               replicates = 1,
                                               clearSimEnv = FALSE)   ## prevent removing objects from the simLists at the end

## save outputs -- both simLists have the same output path, so we can use the first one
qs::qsave(myExperiment, file.path(SpaDES.core::outputPath(simInitMaxEnt), paste0("myExperiment", ".qs")))

## easily check models
tryCatch(myExperiment$MaxEnt_rep1$sdmOut)   ## this links to an html page
plot(myExperiment$MaxEnt_rep1$sdmOut)

sets <- par(mfrow = c(2,2))
plot(myExperiment$GLM_rep1$sdmOut)
par(sets)

## easily check validation results for the two models
myExperiment$MaxEnt_rep1$evalOut
myExperiment$GLM_rep1$evalOut


## Run with another climate scenario - the most contrasting scenario to SSP 585.
## Make copies of the outputs of setupProject, then get the original table from 
## one of the simulations, and replace the climate scenario. 
mySimMaxEnt2 <- mySimMaxEnt
mySimGLM2 <- mySimGLM
projClimateURLsNew <- copy(myExperiment$MaxEnt_rep1$projClimateURLs)
projClimateURLsNew[, `:=`(URL = gsub("ssp585", "ssp126", URL),
                          targetFile = gsub("ssp585", "ssp126", targetFile))]

mySimMaxEnt2$projClimateURLs <- projClimateURLsNew
mySimGLM2$projClimateURLs <- projClimateURLsNew

## now initialise the new simulations/workflows with the new scenarios
simInitMaxEnt2 <- SpaDES.core::simInit2(mySimMaxEnt2)
simInitGLM2 <-  SpaDES.core::simInit2(mySimGLM2)

myExperiment2 <- experiment2(MaxEnt2 = simInitMaxEnt2, 
                             GLM2 = simInitGLM2, 
                             debug = TRUE, 
                             replicates = 1,
                             clearSimEnv = FALSE)
## save outputs
qs::qsave(myExperiment2, file.path(SpaDES.core::outputPath(mySimMaxEnt2), paste0("myExperiment2", ".qs")))

## MaxEnt predictions across time and for each climate scenario --------------
## combine plots from two distinct simulations in a single figure
## (the same can be done to compare MaxEnt and GLM, or plot all projections)

## fetch the internal plotting function rom any of the two simulations,
## instead of repeating code here
plotFun <- myExperiment$MaxEnt_rep1@.envir$.mods$climateData$plotSpatRasterStk

## raw predictions exported by the module
sppDistProjMaxEnt <- myExperiment$MaxEnt_rep1$sppDistProj
sppDistProjMaxEnt2 <- myExperiment2$MaxEnt2_rep1$sppDistProj

## we convert the raw predictions into presence absence
## using exported threshold
sppDistProjMaxEnt_PA <- as.int(myExperiment$MaxEnt_rep1$sppDistProj > myExperiment$MaxEnt_rep1$thresh)
sppDistProjMaxEnt2_PA <- as.int(myExperiment2$MaxEnt2_rep1$sppDistProj > myExperiment2$MaxEnt2_rep1$thresh)

## rename layers from plotting
names(sppDistProjMaxEnt) <- names(sppDistProjMaxEnt2) <- c("2001", "2021-2040", "2041-2060", "2061-2080", "2081-2100")
names(sppDistProjMaxEnt_PA) <- names(sppDistProjMaxEnt2_PA) <- c("2001", "2021-2040", "2041-2060", "2061-2080", "2081-2100")

## for a simpler plot choose only years 2001, 2041-2060 and 2081-2100
yrs <- c("2001", "2041-2060", "2081-2100")
plotMaxEnt <- plotFun(sppDistProjMaxEnt[[yrs]], 
                      xlab = "Longitude", y = "Latitude",
                      plotTitle = "MaxEnt raw predictions - SSP 585") +
  scale_fill_viridis_c(na.value = "grey90", limits = c(0,1), begin = 0.25)
plotMaxEnt2 <- plotFun(sppDistProjMaxEnt2[[yrs]], 
                       xlab = "Longitude", y = "Latitude",
                       plotTitle = "MaxEnt raw predictions - SSP 126") +
  scale_fill_viridis_c(na.value = "grey90", limits = c(0,1), begin = 0.25)
plotMaxEnt_PA <- plotFun(sppDistProjMaxEnt_PA[[yrs]], 
                         xlab = "Longitude", y = "Latitude",
                         plotTitle = "MaxEnt presence/absence - SSP 585") +
  scale_fill_viridis_c(na.value = "grey90", limits = c(0,1), begin = 0.25)
plotMaxEnt2_PA <- plotFun(sppDistProjMaxEnt2_PA[[yrs]], 
                          xlab = "Longitude", y = "Latitude",
                          plotTitle = "MaxEnt presence/absence - SSP 126") +
  scale_fill_viridis_c(na.value = "grey90", limits = c(0,1), begin = 0.25)

## organise the plots with mildest scenario first
## It is clear that MaxEnt and GLM do not agree in their prediction
plotAll <- ggpubr::ggarrange(plotMaxEnt2 + labs(title = expression(bold("Scenario - SSP 126")),
                                                y = expression(atop(bold("Raw predictions"), "Latitude"))) +
                               theme(legend.title = element_blank(), legend.key.height = unit(3, "lines"),
                                     plot.title = element_text(hjust = 0.5), plot.margin = margin(0,0,0,0)), 
                             plotMaxEnt + labs(title = expression(bold("Scenario - SSP 585")),
                                               y = expression(atop(bold(""), ""))) +
                               theme(plot.title = element_text(hjust = 0.5), plot.margin = margin(0,0,0,0)),
                             plotMaxEnt2_PA + labs(title = expression(bold("")),
                                                   y = expression(atop(bold("Presence/absence"), "Latitude"))) +
                               theme(plot.margin = margin(0,0,0,0)), 
                             plotMaxEnt_PA + labs(title = expression(bold("")),
                                                  y = expression(atop(bold(""), ""))) +
                               theme(plot.margin = margin(0,0,0,0)), 
                             legend = "right", common.legend = TRUE, labels = c("a)", "b)", "c)", "d)"))
figDir <- checkPath(file.path(mySimMaxEnt2$paths$outputPath, "generalFigures"), create = TRUE)
ggsave(file.path(figDir, "MaxEntPredictions.png"), width = 13.5, height = 5.5, units = "in", dpi = 300)
