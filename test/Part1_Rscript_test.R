## start from a clean R session
options(repos = c("https://predictiveecology.r-universe.dev/",
                  CRAN = "https://cloud.r-project.org"))
install.packages(c("SpaDES.project", "SpaDES.core"))

library(SpaDES.project)

projPath <- "~/SpaDES4Dummies_Part1"

## Create modules if need be
if (!dir.exists(file.path(projPath, "modules", "speciesAbundance"))) {
  SpaDES.core::newModule(name = "speciesAbundance", path = file.path(projPath, "modules"))
}

if (!dir.exists(file.path(projPath, "temperature"))) {
  SpaDES.core::newModule(name = "temperature", path = file.path(projPath, "modules"))
}

if (!dir.exists(file.path(projPath, "speciesTempLM"))) {
  SpaDES.core::newModule(name = "speciesTempLM", path = file.path(projPath, "modules"))
}

out <- setupProject(paths = list(projectPath = "~/SpaDES4Dummies_Part1",
                                 packagePath = "packages"),
                    overwrite = FALSE,
                    modules = c("speciesAbundance",
                                "temperature",
                                "speciesTempLM"),
                    params = list(speciesAbundance = list(simulationTimeStep = 1,
                                                          .plotInitialTime = 1),
                                  temperature = list(simulationTimeStep = 1,
                                                     .plotInitialTime = 1),
                                  speciesTempLM = list(statsTimestep = 5)),
                    times = list(start = 1, end = 10, timeunit = "year"))

simInitOut <- SpaDES.core::simInit2(out)

simOut <- SpaDES.core::spades(simInitOut)
