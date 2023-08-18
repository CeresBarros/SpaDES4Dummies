## ---------------------------------------------------
## SPADES4DUMMIES PART 2
## ---------------------------------------------------

options(repos = c("https://predictiveecology.r-universe.dev/", 
                  CRAN = "https://cloud.r-project.org"))

if (getRversion() < "4.2.1") {
  warning(paste("dismo::maxent may create a fatal error",
                "when using R version < v4.2.1 and from RStudio.\n", 
                "Please upgrade R, or run this script outside of RStudio.\n",
                "See https://github.com/rspatial/dismo/issues/13"))
}

## decide where you're working
mainPath <- '.'
pkgPath <- file.path(mainPath, "packages", version$platform,
                     paste0(version$major, ".", strsplit(version$minor, "[.]")[[1]][1]))
dir.create(pkgPath, recursive = TRUE)
.libPaths(pkgPath, include.site = FALSE) ## install packages in project library (proj-lib)

if (!"remotes" %in% installed.packages(lib.loc = pkgPath))
  install.packages("remotes")

if (!"Require" %in% installed.packages(lib.loc = pkgPath) ||
    packageVersion("Require", lib.loc = pkgPath) < "0.3.1") {
  remotes::install_github("PredictiveEcology/Require@55ec169e654214d86be62a0e13e9a2157f1aa966",
                          upgrade = FALSE)
}

## use binary linux packages if on Ubuntu
Require::setLinuxBinaryRepo()

## Notes: 
## 1) if you are working from RStudio and have an older version of base packages like `Rcpp`, `rlang` 
## (and others) installed, you may  need to run the following lines (and code above) directly from R
## in order to update these base packages
## 2) Please ensure the appropriate Rtools version is installed (see)

Require::Require(c("PredictiveEcology/SpaDES.project@transition (HEAD)", 
                   "PredictiveEcology/SpaDES.core@master (HEAD)",
                   ## these will be needed later on:
                   "ggpubr",
                   "SpaDES.tools",
                   "PredictiveEcology/SpaDES.experiment@75d917b70b892802fed0bbdb2a5e9f3c6772f0ba"),
                 require = FALSE,  ## don't load packages yet 
                 upgrade = FALSE, standAlone = TRUE)

## there seems to be a problem with `ragg` and a forced install solves it
install.packages("ragg")

Require::Require("SpaDES.core", install = FALSE)  ## load only
setPaths(cachePath = file.path(mainPath, "cache"),
         inputPath = file.path(mainPath, "inputs"),
         modulePath = file.path(mainPath, "modules"),
         outputPath = file.path(mainPath, "outputs"))

simPaths <- getPaths() ## check that this is what you wanted

## Let's create a self-contained module that will simulate the species' abundance for any given period of time and frequency.
if (!dir.exists(file.path(simPaths$modulePath, "speciesAbundanceData"))) {
  newModule(name = "speciesAbundanceData", path = simPaths$modulePath)
}

if (!dir.exists(file.path(simPaths$modulePath, "climateData"))) {
  newModule(name = "climateData", path = simPaths$modulePath)
}

if (!dir.exists(file.path(simPaths$modulePath, "projectSpeciesDist"))) {
  newModule(name = "projectSpeciesDist", path = simPaths$modulePath)
}

## now, let's pretend you've created your modules and each sources a series of other packages
## it's a good idea to always make sure all necessary module dependencies are installed
## this is a particularly useful line when sharing your packages with someone else.
outs <- SpaDES.project::packagesInModules(modulePath = simPaths$modulePath)  ## gets list of module dependencies
Require::Require(c(unname(unlist(outs)),
                   "DiagrammeR"), 
                 require = FALSE,   ## don't load packages
                 upgrade = FALSE,   ## don't upgrade dependencies
                 standAlone = TRUE, 
                 purge = TRUE) ## install all dependencies in proj-lib (ignore user/system lib)

## now load packages - SpaDES.core may have been loaded already, which is fine
Require::Require(c("reproducible", "SpaDES.core", "SpaDES.experiment"), 
                 install = FALSE) 

## dismo needs a few tweaks to run MaxEnt
out <- preProcess(targetFile = "maxent.jar",
                  url = "https://github.com/mrmaxent/Maxent/blob/master/ArchivedReleases/3.4.4/maxent.jar?raw=true",
                  destinationPath = simPaths$inputPath,
                  fun = NA)
file.copy(out$targetFilePath, file.path(system.file("java", package="dismo"), "maxent.jar"),
          overwrite = TRUE)

out <- require(rJava)
if (!out) {
  stop(paste("Your Java installation may have problems, please check.\n", 
             "See https://www.java.com/en/download/manual.jsp for Java installation"))
}
## a few important options:
options(reproducible.useCache = TRUE,
        reproducible.cachePath = simPaths$cachePath,
        reproducible.destinationPath = simPaths$inputPath, ## all downloaded and pre-processed layers go here
        reproducible.useTerra = TRUE, ## we want to use the terra R package
        spades.moduleCodeChecks = FALSE,
        spades.useRequire = FALSE)  

## list the modules to use
simModules <- list("speciesAbundanceData", "climateData", "projectSpeciesDist")

## Set simulation and module parameters
simTimes <- list(start = 1, end = 5, timeunit = "year")

## we create two lists of parameters, one using the default MaxEnt
## the other a GLM
simParamsMaxEnt <- list(
  "speciesAbundanceData" = list(
    ".plots" = c("png"),
    ".useCache" = FALSE
  ),
  "climateData" = list(
    ".plots" = c("png"),
    ".useCache" = FALSE
  ),
  "projectSpeciesDist" = list(
    "statModel" = "MaxEnt",
    ".plots" = c("png"),
    ".useCache" = FALSE
  )
)
simParamsGLM <- simParamsMaxEnt
simParamsGLM$projectSpeciesDist$statModel <- "GLM"

## make a random study area.
##  Here use seed to make sure the same study area is always generated
studyArea <- SpaDES.tools::randomStudyArea(size = 1e10, seed = 123)
studyAreaRas <- terra::rasterize(studyArea, 
                                 terra::rast(extent = terra::ext(studyArea), 
                                             crs = terra::crs(studyArea, proj = TRUE), 
                                             resolution = 1000))
simObjects <- list(
  "studyAreaRas" = studyAreaRas
)

## remove CHECKSUMS.txt (GHA)
file.remove(file.path(simPaths$inputPath, "CHECKSUMS.txt"))

## Simulation setup - create two simulations, one for MaxEnt another for GLM
## SpaDES.experiment::experiment2, will take care of subdirectories to store outputs
mySimMaxEnt <- simInit(times = simTimes, params = simParamsMaxEnt, 
                       modules = simModules, objects = simObjects, 
                       paths = simPaths)
mySimGLM <- simInit(times = simTimes, params = simParamsGLM, 
                    modules = simModules, objects = simObjects, 
                    paths = simPaths)

moduleDiagram(mySimMaxEnt)
objectDiagram(mySimMaxEnt)

## run simulation
clearPlot(force = TRUE)   ## this forces wiping the graphics device and opening a new window

## This runs one simulation and stores outputs in the main 'outputs' folder 
## - not what we want, but good for testing
# mySimOut <- spades(mySimMaxEnt, debug = TRUE)  

## Better to use when spades runs error-free on the simLists
myExperiment <- SpaDES.experiment::experiment2(MaxEnt = mySimMaxEnt, 
                                               GLM = mySimGLM, 
                                               debug = TRUE, 
                                               replicates = 1,
                                               clearSimEnv = FALSE)   ## prevent removing objects from the simLists at the end
## save outputs
qs::qsave(myExperiment, file.path(simPaths$outputPath, paste0("myExperiment", ".qs")))

## check models
tryCatch(myExperiment$MaxEnt_rep1$sdmOut)   ## this links to an html page
sets <- par(mfrow = c(2,2))
plot(myExperiment$MaxEnt_rep1$sdmOut)
par(sets)

## check validation results for the two models
myExperiment$MaxEnt_rep1$evalOut
myExperiment$GLM_rep1$evalOut


## Run with another climate scenario - the most contrasting scenario to SSP 585
## get the original table from one of the simulations and replace the climate scenario
projClimateURLs <- copy(mySimMaxEnt$projClimateURLs)
projClimateURLs[, `:=`(URL = gsub("ssp585", "ssp126", URL),
                       targetFile = gsub("ssp585", "ssp126", targetFile))]

## this time we pass the new table or URLs to the modules, so that climate layers are changed
simObjects2 <- list(
  "studyAreaRas" = studyAreaRas,
  "projClimateURLs" = projClimateURLs
)

mySimMaxEnt2 <- simInit(times = simTimes, params = simParamsMaxEnt, 
                        modules = simModules, objects = simObjects2, 
                        paths = simPaths)
mySimGLM2 <- simInit(times = simTimes, params = simParamsGLM, 
                     modules = simModules, objects = simObjects2, 
                     paths = simPaths)

myExperiment2 <- experiment2(MaxEnt = mySimMaxEnt2, 
                             GLM = mySimGLM2, 
                             debug = TRUE, 
                             replicates = 1,
                             clearSimEnv = FALSE)
## save outputs
qs::qsave(myExperiment2, file.path(simPaths$outputPath, paste0("myExperiment2", ".qs")))

## MaxEnt predictions across time and for each climate scenario --------------
## combine plots from two distinct simulations in a single figure
## (the same can be done to compare MaxEnt and GLM, or plot all projections)

## fetch the internal plotting function instead of repeating code here
plotFun <- myExperiment$GLM_rep1@.envir$.mods$climateData$plotSpatRasterStk

## raw predictions exported by the module
sppDistProjMaxEnt <- myExperiment$MaxEnt_rep1$sppDistProj
sppDistProjMaxEnt2 <- myExperiment2$MaxEnt_rep1$sppDistProj

## we convert the raw predictions into presence absence
## using exported threshold
sppDistProjMaxEnt_PA <- myExperiment$MaxEnt_rep1$sppDistProj > myExperiment$MaxEnt_rep1$thresh
sppDistProjMaxEnt2_PA <- myExperiment2$MaxEnt_rep1$sppDistProj > myExperiment2$MaxEnt_rep1$thresh

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
figDir <- checkPath(file.path(simPaths$outputPath, "generalFigures"), create = TRUE)
ggsave(file.path(figDir, "MaxEntPredictions.png"), width = 13.5, height = 5.5, units = "in", dpi = 300)
