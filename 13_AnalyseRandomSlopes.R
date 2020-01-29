suppressMessages(suppressWarnings(library(StatisticalModels)))
suppressMessages(suppressWarnings(library(raster)))

dataDir <- "0_data/"
sitesDir <- "2_PrepareSiteData/"
modelDir <- "12_RunRandomSlopeModels/"

wgsCRS <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
behrCRS <- CRS('+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs')

randomSlopeModel <- readRDS(file = paste0(modelDir,"RichnessRandomSlopeModel.rds"))

studySlopes <- ranef(randomSlopeModel$model)$SS

studySlopes$Human <- apply(X = studySlopes[,c(
  'LandUse3Harvested','LandUse3Pasture')],
  MARGIN = 1,FUN = mean)

load(paste0(dataDir,"modelling_data.Rd"))

study.avg.endemicity <- tapply(X = sites.div$CWM_log.range,INDEX = sites.div$SS,FUN = mean)

studySlopes$avg.endemicity <- as.numeric(study.avg.endemicity[
  match(row.names(studySlopes),names(study.avg.endemicity))])

rm(sites.div)

load(paste0(sitesDir,"modelling_data.Rd"))

studyBiomes <- tapply(
  X = sites.div$Biome2,INDEX = sites.div$SS,FUN = function(x){
    
    b <- names(sort(table(x),decreasing = TRUE))[1]
    
  })

studySlopes$Biome <- factor(paste(studyBiomes[match(
  row.names(studySlopes),names(studyBiomes))]))

studyLons <- tapply(X = sites.div$Longitude,INDEX = sites.div$SS,FUN = mean)
studyLats <- tapply(X = sites.div$Latitude,INDEX = sites.div$SS,FUN = mean)

studySlopes$Longitude <- as.numeric(studyLons[
  match(row.names(studySlopes),names(studyLons))])
studySlopes$Latitude <- as.numeric(studyLats[
  match(row.names(studySlopes),names(studyLats))])

tempSeas <- raster(paste0(dataDir,"bio_4"))

studySlopes$TempSeas <- raster::extract(
  x = tempSeas,y = studySlopes[,c('Longitude','Latitude')])

precipSeas <- raster(paste0(dataDir,"bio_15"))

studySlopes$PrecipSeas <- raster::extract(
  x = precipSeas,y = studySlopes[,c('Longitude','Latitude')])

ThermalIndex <- mean(stack(c(paste0(dataDir,"TI_Amphibians.asc"),
                             paste0(dataDir,"TI_Birds.asc"),
                             paste0(dataDir,"TI_Mammals.asc"),
                             paste0(dataDir,"TI_Reptiles.asc"))),
                     na.rm=TRUE)

ThermalIndex <- raster::crop(
  x = ThermalIndex,
  y = extent(-17357529,17362471,-6337770,7332230))

crs(ThermalIndex) <- behrCRS

ThermalIndex <- raster::projectRaster(
  from = ThermalIndex,crs = wgsCRS,
  method = 'bilinear')

studySlopes$ThermalIndex <- raster::extract(
  x = ThermalIndex,y = studySlopes[,c('Longitude','Latitude')])

PrecipIndex <- mean(stack(c(paste0(dataDir,"PI_Amphibians.asc"),
                            paste0(dataDir,"PI_Birds.asc"),
                            paste0(dataDir,"PI_Mammals.asc"),
                            paste0(dataDir,"PI_Reptiles.asc"))),
                    na.rm=TRUE)

PrecipIndex <- raster::crop(
  x = PrecipIndex,
  y = extent(-17357529,17362471,-6337770,7332230))

crs(PrecipIndex) <- behrCRS

PrecipIndex <- raster::projectRaster(
  from = PrecipIndex,crs = wgsCRS,
  method = 'bilinear')

studySlopes$PrecipIndex <- raster::extract(
  x = PrecipIndex,y = studySlopes[,c('Longitude','Latitude')])

studySlopes$LogTempSeas <- log(studySlopes$TempSeas)
studySlopes$LogPrecipSeas <- log(studySlopes$PrecipSeas)

modDat <- na.omit(studySlopes)

rsMod <- lm(Human~Biome+poly(Latitude,2)+poly(avg.endemicity,2)+
              poly(LogTempSeas,2)+poly(LogPrecipSeas,2)+
              poly(ThermalIndex,2)+poly(PrecipIndex,2)+
              poly(LogTempSeas,2):poly(LogPrecipSeas,2)+
              poly(ThermalIndex,2):poly(PrecipIndex,2)+
              poly(LogTempSeas,2):poly(ThermalIndex,2)+
              poly(LogPrecipSeas,2):poly(PrecipIndex,2),
            data=modDat)

