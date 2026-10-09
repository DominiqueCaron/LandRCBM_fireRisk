## Everything in this file and any files in the R directory are sourced during `simInit()`;
## all functions and objects are put into the `simList`.
## To use objects, use `sim$xxx` (they are globally available to all modules).
## Functions can be used inside any function that was sourced in this module;
## they are namespaced to the module, just like functions in R packages.
## If exact location is required, functions will be: `sim$.mods$<moduleName>$FunctionName`.
defineModule(sim, list(
  name = "LandRCBM_fireRisk",
  description = "",
  keywords = "",
  authors = structure(list(list(given = "Dominique", family = "Caron", role = c("aut", "cre"), email = "dominique.caron@nrcan-rncan.gc.ca")), class = "person"),
  childModules = character(0),
  version = list(LandRCBM_fireRisk = "0.0.0.9000"),
  timeframe = as.POSIXlt(c(NA, NA)),
  timeunit = "year",
  citation = list("citation.bib"),
  documentation = list("NEWS.md", "README.md", "LandRCBM_fireRisk.Rmd"),
  reqdPkgs = list("PredictiveEcology/SpaDES.core@development (>= 3.1.2.9005)", "ggplot2", "terra", "data.table", "SpaDES.tools"),
  parameters = bindrows(
    defineParameter("iterations", "numeric", 100000L, 100L, NA, "Number of simulations of annual fires used to estimate probability of burning of pixels."),
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
    expectsInput("pEscape", "SpatRaster", desc = "escape probability raster."),
    expectsInput("pIgnition", "SpatRaster", desc = "ignition probability raster."),
    expectsInput("pSpread", "SpatRaster", desc = "spread probability raster."),
    expectsInput("cbm_vars", "list", desc = paste("List of 5 data tables defining active cohorts in the current year:",
                                                  "key, parameters, pools, flux, and state.",
                                                  "This is created initially during the spinup and updated each year.")),
    expectsInput("cTransfers", "data.table", desc = "Carbon transfer values table with associated disturbance names and IDs."),
    expectsInput("rasterToMatch", "SpatRaster", desc = "Raster template used to rasterize the fire susceptibility output.")
  ),
  outputObjects = bindrows(
    #createsOutput("objectName", "objectClass", "output object description", ...),
    createsOutput(objectName = "fireProbability", objectClass = "SpatRaster", desc = "Burn probability raster."),
    createsOutput(objectName = "fireSusceptibility", objectClass = "SpatRaster", desc = "Raster of carbon emitted to the atmosphere if burned."),
    createsOutput(objectName = "fireRisk", objectClass = "SpatRaster", desc = "Raster of fire risk to carbon.")
  )
))

doEvent.LandRCBM_fireRisk = function(sim, eventTime, eventType) {
  switch(
    eventType,
    init = {

      # do stuff for this event
      sim <- scheduleEvent(sim, start(sim), "LandRCBM_fireRisk", "calculateFireRisk", eventPriority = 5.20)
      
      # schedule future event(s)
      sim <- scheduleEvent(sim, P(sim)$.plotInitialTime, "LandRCBM_fireRisk", "plot")
      sim <- scheduleEvent(sim, P(sim)$.saveInitialTime, "LandRCBM_fireRisk", "save")
    },
    plot = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event

      plotFun(sim) # example of a plotting function
      # schedule future event(s)

      # e.g.,
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.plotInterval, "LandRCBM_fireRisk", "plot")

      # ! ----- STOP EDITING ----- ! #
    },
    save = {
      # ! ----- EDIT BELOW ----- ! #
      # do stuff for this event
      saveFiles(sim)

      # schedule future event(s)
      sim <- scheduleEvent(sim, time(sim) + P(sim)$.saveInterval, "LandRCBM_fireRisk", "save")

      # ! ----- STOP EDITING ----- ! #
    },
    calculateFireRisk = {
      
      sim$fireProbability <- calculateFireProbability(sim$pIgnition, sim$pEscape, sim$pSpread, P(sim)$iterations)
      
      sim$fireSusceptibility <- calculateFireSusceptibility(sim$cbm_vars, sim$cTransfers, sim$rasterToMatch)
      
      sim$fireRisk <- sim$fireProbability * sim$fireSusceptibility

      # schedule future event(s)
      if (time(sim) < end(sim)) {
        sim <- scheduleEvent(sim, min(time(sim) + 1, end(sim)), "LandRCBM_fireRisk", "calculateFireRisk", eventPriority = 5.20)
      }

    },
    warning(noEventWarning(sim))
  )
  return(invisible(sim))
}

calculateFireProbability <- function(pIgnition, pEscape, pSpread, iterations = 100L){
  
  # Set up the progress bar
  pb <- txtProgressBar(min = 1, max = iterations, style = 3)
  
  # Get the probabilities as vectors
  ign_probs <- values(pIgnition)
  esc_probs <- values(pEscape)
  spread_probs <- values(pSpread)
  
  # Set up the number of fires for each cell
  fires <- rep(0, ncell(pSpread))
  
  # Repeat ignition, escape, spread x times to estimate the probability that a fire 
  # spreads to pixels (the fire must not be ignited in that same pixels)
  for (i in 1:iterations) {
    fires <- fires + simulateFire(ign_probs, esc_probs, spread_probs, pSpread)
    # Print progress
    setTxtProgressBar(pb, i)
  }
  
  # Create the fire probability raster
  fireProbability <- rast(pSpread)
  
  # Add the probability of ignition and the probability that a fire ignited 
  # in another pixel spread to a given pixels
  # Clamp to [0, 1]: the sum is not itself guaranteed to be a valid probability
  values(fireProbability) <- pmin(pmax(fires / iterations + ign_probs, 0), 1)
  return(fireProbability)
}

simulateFire <- function(ign_probs, esc_probs, spread_probs, landscape){
  ignitions <- runif(length(ign_probs)) < ign_probs
  # predict escapes
  # NA escape probabilities are treated as "does not escape" (0) rather than dropped,
  # to keep indices aligned with which(ignitions)
  esc_probs2 <- esc_probs[ignitions]
  esc_probs2[is.na(esc_probs2)] <- 0
  escapes <- runif(length(esc_probs2)) < esc_probs2
  
  startSpread <- which(ignitions)[escapes]
  
  # simulate spread
  spread_prediction <- SpaDES.tools::spread2(
    landscape = landscape,
    start = startSpread,
    spreadProb = spread_probs,
    asRaster = FALSE
  )
  fires <- rep(0L, length(ignitions))
  fires[spread_prediction$pixels] <- 1L
  return(fires)
}

calculateFireSusceptibility <- function(cbm_vars, cTransfers, rasterToMatch){
  # keep only rows needed in cTransfers: spatial units and transfer to the atmosphere
  spuids <- unique(cbm_vars$state$spatial_unit_id)
  transfers <- cTransfers[cTransfers$disturbance_type_id == 1 & cTransfers$spatial_unit_id %in% spuids, ]
  transfers <- transfers[transfers$sink_pool %in% c("CO2", "CO", "CH4"),]
  
  # apply each transfer to the cohort groups
  cohortGroupConsequences <- rep(0L, nrow(cbm_vars$pools))
  
  for (i in seq_len(nrow(transfers))){
    t <- transfers[i,]
    
    cohortGroups <- (cbm_vars$state$sw_hw == (t$sw_hw == "hw")) & cbm_vars$state$spatial_unit_id == t$spatial_unit_id
    
    cohortGroupConsequences <- cohortGroupConsequences + (cbm_vars$pools[, get(t$source_pool)] * t$proportion * cohortGroups)
  }
  
  # convert the cohort group consequences to pixels
  pixelConsequences <- data.table(pixelIndex = cbm_vars$key$pixelIndex, consequence = cohortGroupConsequences[cbm_vars$key$row_idx])
  pixelConsequences <- pixelConsequences[ , .(consequence = sum(consequence)), by = "pixelIndex"]
  
  # rasterize
  fireSusceptibility <- rast(rasterToMatch, names = "fireSusceptibility", vals = NA)
  fireSusceptibility[pixelConsequences$pixelIndex] <- pixelConsequences$consequence
  
  return(fireSusceptibility)
}

### template for plot events
plotFun <- function(sim) {

  sampleData <- data.frame("TheSample" = sample(1:10, replace = TRUE))
  Plots(sampleData, fn = ggplotFn)

  return(invisible(sim))
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

  # ! ----- STOP EDITING ----- ! #
  return(invisible(sim))
}

ggplotFn <- function(data, ...) {
  ggplot2::ggplot(data, ggplot2::aes(TheSample)) +
    ggplot2::geom_histogram(...)
}

