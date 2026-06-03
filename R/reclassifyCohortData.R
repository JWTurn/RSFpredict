#' @title reclassify LandR cohort data to NTEMs forest classes
#' @export
#' @author Ian Eddy, adapted by Julie W. Turner

#' 
reclassifyCohortData <- function(cohortData, sppEquivCol, sppEquiv = LandR::sppEquivalencies_CA,
                                 pixelGroupMap, mixedForestCutoffs = c(0.33, 0.66)) {
  #cohortData <- copy(outSim$cohortData)
  
  #find conifers
  conifers <- sppEquiv[Type == "Conifer",][[sppEquivCol]]
  conifers <- conifers[conifers %in% cohortData$speciesCode]
  
  #calculate biomass sum and biomass-weighted age
  cohortData[, sumB := sum(B), .(pixelGroup)]
  cohortData[, BweightedAge := round(sum(age * B/sumB)), .(pixelGroup)]
  
  #find percent coniferous
  cohortData[, isConifer := speciesCode %in% conifers]
  Pgs <- cohortData[, .(propB = B/sumB), .(pixelGroup, isConifer)]
  #sum by conifer/deciduous in each pixelGroup
  Pgs <- Pgs[, .(propB = sum(propB)), .(pixelGroup, isConifer)]
  Pgs[isConifer == TRUE & propB >= max(mixedForestCutoffs), ForestType := 210,] #conifer
  Pgs[propB < max(mixedForestCutoffs) & propB > min(mixedForestCutoffs), ForestType := 230] #mixed
  #whatever is left is broadleaf
  Pgs <- unique(Pgs[, .(pixelGroup, ForestType)])
  Pgs[is.na(ForestType), ForestType := 220] # deciduous
  #Pgs[, ForestType := as.factor(ForestType)]
  
  standInfo <- unique(cohortData[, .(BweightedAge, sumB, pixelGroup)])[Pgs, on = c("pixelGroup")]
  summary(standInfo)
  standInfo[, B_MgHa := sumB/100]
  
  #make into long form
  pixelIndex <- as.data.table(terra::as.data.frame(pixelGroupMap, cells = TRUE))
  pixelIndex <- standInfo[pixelIndex, on = c("pixelGroup")]
  standAge <- rast(pixelGroupMap)
  standAge[pixelIndex$cell] <- pixelIndex$BweightedAge
  
  forestType <- rast(pixelGroupMap)
  forestType[pixelIndex$cell] <- pixelIndex$ForestType
  
  AGB <- rast(pixelGroupMap)
  AGB[pixelIndex$cell] <- pixelIndex$B_MgHa
  crudeNTEMS <- c(standAge, forestType, AGB)
  terra::set.names(crudeNTEMS, c("stand age", "forest type", "AGB (Mg/ha)"))
  return(crudeNTEMS)
}
