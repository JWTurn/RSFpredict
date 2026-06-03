## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "RSFpredict",
  description = "",
  keywords = "",
  authors = structure(list(list(given = c("Julie", "W"), family = "Turner", role = c("aut", "cre"), email = "julwturner@gmail.com", comment = NULL)), class = "person"),
  childModules = character(0),
  version = list(RSFpredict = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "RSFpredict.Rmd"),
  reqdPkgs = list("PredictiveEcology/SpaDES.core@development (>= 3.0.3.9003)", "reproducible", "ggplot2", "glmmTMB",
                  'tidyterra', 'terra', 'viridis', 'ggthemes'),
  parameters = bindrows(
    #defineParameter("paramName", "paramClass", value, min, max, "parameter description"),
    defineParameter("numBins", "integer", 10, NA, NA,
                    "number of bins for RSF, default = 10"),
    defineParameter("simulationProcess", "character", "static", NA, NA,
                    paste0("Should the simulation use LandR (dynamic) or land cover map (static)?",
                           "defaults to static")),
    defineParameter("predictionInterval", "numeric", 5, NA, NA, "Time between predictions"),
    defineParameter("predictStartYear", "numeric", 2025, NA, NA,
                    paste0("The first year to start forecasted simulations if dynamic.",
                           " This is because we start forecasting landcovers earlier.")),
    defineParameter("predictLastYear", "logical", TRUE, NA, NA,
                    paste0("If last year of simulation is not multiple of")),
    defineParameter("ts_else", "integer", 100, NA, NA,
                    paste0("This is the value to fill in NAs in time since disturbance layers",
                           "This parameter would need to be updated if want a different default year.")),
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
                    "Human-readable name for the study area used - e.g., a hash of the study",
                          "area obtained using `reproducible::studyAreaName()`"),
    ## .seed is optional: `list('init' = 123)` will `set.seed(123)` for the `init` event only.
    defineParameter(".seed", "list", list(), NA, NA,
                    "Named list of seeds to use for each event (names)."),
    defineParameter(".useCache", "logical", FALSE, NA, NA,
                    "Should caching of events or module be used?")
  ),
  inputObjects = bindrows(
    #expectsInput("objectName", "objectClass", "input object description", sourceURL, ...),
    expectsInput(objectName = 'model', objectClass = c('glmmTMB'),
                 desc = 'RSF model'),
    expectsInput(objectName = 'studyArea', objectClass = 'spatVector',
                 desc = 'unbuffered study area'),
    expectsInput(objectName = 'modelLand', objectClass = 'spatRaster',
                 desc = 'stack of raster data as start layers that data were extracted from'),
    expectsInput("cohortData", "data.table",
                 desc = paste("`data.table` with cohort-level information on age and biomass, by `pixelGroup` and ecolocation",
                              "(i.e., `ecoregionGroup`). If supplied, it must have the following columns: `pixelGroup` (integer),",
                              "`ecoregionGroup` (factor), `speciesCode` (factor), `B` (integer in $g/m^2$), `age` (integer in years)")),
    expectsInput("pixelGroupMap", "SpatRaster",
                 desc = paste("A raster layer with `pixelGroup` IDs per pixel. Pixels are grouped" ,
                              "based on identical `ecoregionGroup`, `speciesCode`, `age` and `B` composition,",
                              "even if the user supplies other initial groupings (e.g., via the `Biomass_borealDataPrep`",
                              "module.")),
    expectsInput(objectName = "timeSinceFire", objectClass = "SpatRaster",
                 desc = "If dynamic, map of time since last burn used in model")
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "pred", objectClass = 'spatRaster', desc = 'raw predicted RSF'),
    createsOutput(objectName = "binMap", objectClass = 'spatRaster', desc = 'binned predicted RSF'),
    createsOutput("timeSinceFire", "SpatRaster",
                  "If dynamic, map of time since last burn used in model - with pixels that never burn receiving NA"),
    createsOutput(objectName = "simLand", objectClass = 'spatRaster', desc = 'simulated landcover layers for RSF'),
    createsOutput(objectName = "simPred", objectClass = 'spatRaster', desc = 'simulated raw predicted RSF'),
    createsOutput(objectName = "simBinMap", objectClass = 'spatRaster', desc = 'binned predicted RSF')


  )
))

doEvent.RSFpredict = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {
      ### check for more detailed object dependencies:
      ### (use `checkObject` or similar)


      # Baseline always runs
      sim <- scheduleEvent(sim, time(sim), "RSFpredict", "buildBaselineRSFmap")

      if (Par$simulationProcess == "dynamic") {

        # always run simLayers
        sim <- scheduleEvent(sim, P(sim)$predictStartYear, "RSFpredict", "simLayers")
        sim <- scheduleEvent(sim, P(sim)$predictStartYear, "RSFpredict", "simRSFmap")

      }

      if (Par$predictLastYear) {
        sim <- scheduleEvent(sim, end(sim), "RSFpredict", "simLayers")
      }



      # schedule future event(s)
      # sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "RSFpredict", "plot")
      # sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "RSFpredict", "save")
    },

    buildBaselineRSFmap = {

      message("Building baseline RSF map")

      sim$pred <- terra::predict(sim$landStack, sim$model, type = "response", re.form = NA) |>
        Cache()
      # predict(sim$model,
      #               newdata = sim$landStack,#[!is.na(Covar.brick.values$DEM),],
      #               allow.new.levels=TRUE)
      pred.mask <- terra::mask(sim$pred, sim$studyArea)
      # TODO get rid of weird extremes
      # quantPred <- global(pred.mask, quantile, probs = c(0.9), na.rm = T)
      # predClmp <- terra::clamp(pred.mask, upper = quantPred[[1]])
      breaks <- terra::global(pred.mask, quantile, na.rm = T, probs = seq(0,1,1/Par$numBins))
      v.breaks <- t(breaks)
      sim$binMap <- terra::classify(pred.mask, v.breaks, include.lowest=TRUE, brackets=TRUE)

      # p.rsf <- ggplot() +
      #   geom_spatraster(data = as.numeric(sim$binMap, show.legend = T)) +
      #   scale_fill_viridis(option = 'viridis', na.value = NA, name = 'Intensity of Selection') +
      #   theme_minimal() +
      #   labs(x = NULL, y = NULL)
      # p.rsf
      #
      # outPath <- outputPath(sim)
      # ggsave(plot = p.rsf, filename = file.path(outPath, 'map.png'))

      if (Par$simulationProcess == "dynamic") {

          sim$timeSinceFire <- postProcess(sim$modelLand$timeSinceFire,
                                           to = sim$rasterToMatch)
        }
      #

    },

    simLayers = {

      thisYear <- as.integer(time(sim))
      key <- paste0("year", thisYear)
      message(paste0("Simulating landscape for ", thisYear))

      template <- sim$rasterToMatch

      templateCoarse <- terra::aggregate(template, fact = 2)


      # Dynamic forest layers from LandR
      reclassForest <- reclassifyCohortData(
        cohortData = sim$cohortData,
        sppEquivCol = "LandR",
        pixelGroupMap = sim$pixelGroupMap,
        mixedForestCutoffs = c(0.33, 0.66)
      )

      ft <- reclassForest$`forest type`


        # Needleleaf
        needle_mask <- terra::classify(
          ft, rcl = matrix(c(210, 1), ncol = 2, byrow = TRUE), others = 0
        )
        prop_needleleaf <- terra::resample(needle_mask, template, method = "average")
        names(prop_needleleaf) <- "prop_needleleaf"

        # Mixedforest
        mixed_mask <- terra::classify(
          ft, rcl = matrix(c(220, 1, 230, 1), ncol = 2, byrow = TRUE),
          others = 0
        )
        prop_mixedforest <- terra::resample(mixed_mask, template, method = "average")
        names(prop_mixedforest) <- "prop_mixedforest"

        # scale up to RSF res
        forests <- reproducible::postProcess(c(prop_needleleaf, prop_mixedforest), templateCoarse, method = 'average')


      message("Complated reclassifying forest layers")


      # Dynamic timeSinceFire from scfmSpread
      tsf <- reproducible::postProcess(sim$timeSinceFire, to = templateCoarse)
      tsf[is.na(tsf)] <- P(sim)$ts_else + (thisYear - 2020) #TODO this is hardcoded for now, now grab start time?

      message("Updated timeSinceFire using scfm for RSF")

      names(tsf) <- "timeSinceFire"

      tsRasts <- tsf



      # Bundle bundle updates
      sim$simLand[[key]] <- c(forests, tsRasts)
      # simLand needs to be exported to workflowOutputs for normalization model

      # sim$simEnv[[key]] <- list2env(
      #   setNames(lapply(names(simLand), \(n) simLand[[n]]), names(simLand)),
      #   parent = baseenv()
      # )
      message("needleleaf mean: ", terra::global(prop_needleleaf, "mean", na.rm=TRUE)[1,1])
      message("mixed mean: ", terra::global(prop_mixedforest, "mean", na.rm=TRUE)[1,1])
      message("tsf mean: ", terra::global(tsf, "mean", na.rm=TRUE)[1,1])



      # save layers
      outDir <- reproducible::checkPath(file.path(outputPath(sim), paste0(Par$.studyAreaName, '_', 'sims')), create = T)


        terra::writeRaster(sim$simLand[[key]], file.path(outDir, paste0("mapLayers_", Par$.studyAreaName, "_", key, ".tif")), overwrite = TRUE)


      sim <- scheduleEvent(sim, time(sim) + P(sim)$predictionInterval, "RSFpredict", "simLayers")
    },

    simRSFmap = {
      thisYear <- as.integer(time(sim))
      key <- paste0("year", thisYear)
      message(paste0("Calculating simulated RSF map for: ", thisYear))
      if (is.null(sim$simEnv[[key]]))
        stop("Missing sim$simEnv[['", key, "']]. Did simLayers run first?")


      outDir <- reproducible::checkPath(file.path(outputPath(sim), paste0(Par$.studyAreaName), '_', 'sims'), create = T)

      sim$simPred[[key]] <- terra::predict(sim$landStack, sim$model, type = "response", re.form = NA) |>
        Cache()

      pred.mask <- terra::mask(sim$simPred, sim$studyArea)
      # TODO get rid of weird extremes
      # quantPred <- global(pred.mask, quantile, probs = c(0.9), na.rm = T)
      # predClmp <- terra::clamp(pred.mask, upper = quantPred[[1]])
      breaks <- terra::global(pred.mask, quantile, na.rm = T, probs = seq(0,1,1/Par$numBins))
      v.breaks <- t(breaks)
      sim$simBinMap[[key]] <- terra::classify(pred.mask, v.breaks, include.lowest=TRUE, brackets=TRUE)


        terra::writeRaster(pred.mask, file.path(outDir, paste0("rsfMap", Par$.studyAreaName, '_', key, '.tif')), overwrite = TRUE)
        terra::writeRaster(sim$simBinMap[[key]], file.path(outDir, paste0("rsfBinMap_", Par$.studyAreaName, '_', key, '.tif')), overwrite = TRUE)




      message(paste0("Finished creating RSF for: ", thisYear))
      message(paste0("Outputs saved to: ", outDir))
      sim <- scheduleEvent(sim, time(sim) + P(sim)$predictionInterval, "RSFpredict", "simRSFmap")
    },

    warning(noEventWarning(sim))
  )
  return(invisible(sim))
  #return(sim)

}

.inputObjects <- function(sim) {
  # Any code written here will be run during the simInit for the purpose of creating
  # any objects required by this module and identified in the inputObjects element of defineModule.
  # This is useful if there is something required before simulation to produce the module
  # object dependencies, including such things as downloading default datasets, e.g.,
  # downloadData("LCC2005", modulePath(sim)).
  # Nothing should be created here that does not create a named object in inputObjects.
  # Any other initiation procedures should be put in "init" eventType of the doEvent function.
  # Note: the module developer can check if an object is 'suppliedElsewhere' to
  # selectively skip unnecessary steps because the user has provided those inputObjects in the
  # simInit call, or another module will supply or has supplied it. e.g.,
  # if (!suppliedElsewhere('defaultColor', sim)) {
  #   sim$map <- Cache(prepInputs, extractURL('map')) # download, extract, load file from url in sourceURL
  # }

  #cacheTags <- c(currentModule(sim), "function:.inputObjects") ## uncomment this if Cache is being used
  dPath <- asPath(getOption("reproducible.destinationPath", dataPath(sim)), 1)
  message(currentModule(sim), ": using dataPath '", dPath, "'.")

  # ! ----- EDIT BELOW ----- ! #
  if (!suppliedElsewhere("model", sim)) {

      stop("RSF model not provided")

  }

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

ggplotFn <- function(data, ...) {
  ggplot2::ggplot(data, ggplot2::aes(TheSample)) +
    ggplot2::geom_histogram(...)
}

