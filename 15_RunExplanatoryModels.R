outDir <- "15_RunExplanatoryModels/"

dataDir <- "0_data/"
sitesDir <- "2_PrepareSiteData/"

sink(file = paste0(outDir,"log.txt"))

t.start <- Sys.time()

print(t.start)

suppressMessages(suppressWarnings(library(StatisticalModels)))
suppressMessages(suppressWarnings(library(raster)))

print(sessionInfo())

wgsCRS <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
behrCRS <- CRS('+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs')

load(paste0(dataDir,"modelling_data.Rd"))

sites.div.range <- sites.div
rm(sites.div)

load(paste0(sitesDir,"modelling_data.Rd"))

sites.div$LogAbund <- log(sites.div$Total_abundance+1)

sites.div <- droplevels(sites.div)

sites.div$CWM_log.range <- sites.div.range$CWM_log.range[match(sites.div$SSBS,sites.div.range$SSBS)]

tempSeas <- raster(paste0(dataDir,"bio_4"))

sites.div$TempSeas <- raster::extract(
  x = tempSeas,y = sites.div[,c('Longitude','Latitude')])

sites.div$LogTempSeas <- log(sites.div$TempSeas)

precipSeas <- raster(paste0(dataDir,"bio_15"))

sites.div$PrecipSeas <- raster::extract(
  x = precipSeas,y = sites.div[,c('Longitude','Latitude')])

sites.div$LogPrecipSeas <- log(sites.div$PrecipSeas)

ThermalIndex <- raster(paste0(dataDir,"VertebrateThermalIndex.grd"))
# ThermalIndex <- mean(stack(c(paste0(dataDir,"TI_Amphibians.asc"),
#                              paste0(dataDir,"TI_Birds.asc"),
#                              paste0(dataDir,"TI_Mammals.asc"),
#                              paste0(dataDir,"TI_Reptiles.asc"))),
#                      na.rm=TRUE)

ThermalIndex <- raster::crop(
  x = ThermalIndex,
  y = extent(-17357529,17362471,-6337770,7332230))

crs(ThermalIndex) <- behrCRS

ThermalIndex <- raster::projectRaster(
  from = ThermalIndex,crs = wgsCRS,
  method = 'bilinear')

sites.div$ThermalIndex <- raster::extract(
  x = ThermalIndex,y = sites.div[,c('Longitude','Latitude')])

PrecipIndex <- raster(paste0(dataDir,"VertebratePrecipitationIndex.grd"))
# PrecipIndex <- mean(stack(c(paste0(dataDir,"PI_Amphibians.asc"),
#                             paste0(dataDir,"PI_Birds.asc"),
#                             paste0(dataDir,"PI_Mammals.asc"),
#                             paste0(dataDir,"PI_Reptiles.asc"))),
#                     na.rm=TRUE)

PrecipIndex <- raster::crop(
  x = PrecipIndex,
  y = extent(-17357529,17362471,-6337770,7332230))

crs(PrecipIndex) <- behrCRS

PrecipIndex <- raster::projectRaster(
  from = PrecipIndex,crs = wgsCRS,
  method = 'bilinear')

sites.div$PrecipIndex <- raster::extract(
  x = PrecipIndex,y = sites.div[,c('Longitude','Latitude')])

PropSpec <- raster(paste0(dataDir,"Prop_spe_raster.grd"))

PropSpec <- raster::crop(
  x = PropSpec,
  y = extent(-17357529,17362471,-6337770,7332230))

crs(PropSpec) <- behrCRS

PropSpec <- raster::projectRaster(
  from = PropSpec,crs = wgsCRS,
  method = 'bilinear')

sites.div$PropSpec <- raster::extract(
  x = PropSpec,y = sites.div[,c('Longitude','Latitude')])

YOC30 <- raster(paste0(dataDir,"yocPS15002005_1_0.3.asc"))
values(YOC30)[values(YOC30)==0] <- 2005
AgeConv <- 2005 - YOC30

sites.div$AgeConv <- raster::extract(
  x = AgeConv,
  y = sites.div[,c('Longitude','Latitude')])

sites.div$LogAgeConv <- log(sites.div$AgeConv+1)

# testModelRichRange <- GLMER(modelData = sites.div,
#                             responseVar = "Species_richness",
#                             fitFamily = "poisson",
#                             fixedStruct = "LandUse3+Biome2+poly(CWM_log.range,2)+LandUse3:Biome2+LandUse3:poly(CWM_log.range,2)",
#                             randomStruct = "(1|SS)+(1|SSBS)")

# Weak interaction between CWM range size and land use - don't take forward for final analysis

# testModelRichTempSeas <- GLMER(modelData = sites.div,
#                        responseVar = "Species_richness",
#                        fitFamily = "poisson",
#                        fixedStruct = "LandUse3+Biome2+poly(LogTempSeas,2)+LandUse3:Biome2+LandUse3:poly(LogTempSeas,2)",
#                        randomStruct = "(1|SS)+(1|SSBS)")

# PlotGLMERContinuous(model = testModelRichTempSeas$model,data = testModelRichTempSeas$data,
#                     effects = "LogTempSeas",otherFactors = list(Biome2="Tropical Forest"),
#                     byFactor = "LandUse3",xlab = "(Log) Temperature seasonality",
#                     ylab = "Species richness",
#                     line.cols = c("#66A61E","#E6AB02","#D95F02","#1B9E77"))

# Moderate interaction between temperature seasonality and land use - consider in final model

# testModelRichPrecipSeas <- GLMER(modelData = sites.div,
#                        responseVar = "Species_richness",
#                        fitFamily = "poisson",
#                        fixedStruct = "LandUse3+Biome2+poly(LogPrecipSeas,2)+LandUse3:Biome2+LandUse3:poly(LogPrecipSeas,2)",
#                        randomStruct = "(1|SS)+(1|SSBS)")
# 
# PlotGLMERContinuous(model = testModelRichPrecipSeas$model,data = testModelRichPrecipSeas$data,
#                     effects = "LogPrecipSeas",otherFactors = list(Biome2="Tropical Forest"),
#                     byFactor = "LandUse3",xlab = "(Log) Precipitation seasonality",
#                     ylab = "Species richness",
#                     line.cols = c("#66A61E","#E6AB02","#D95F02","#1B9E77"))

# Strong interaction between precipitation seasonality and land use - consider in final model

# testModelRichThermalIndex <- GLMER(modelData = sites.div,
#                        responseVar = "Species_richness",
#                        fitFamily = "poisson",
#                        fixedStruct = "LandUse3+Biome2+poly(ThermalIndex,2)+LandUse3:Biome2+LandUse3:poly(ThermalIndex,2)",
#                        randomStruct = "(1|SS)+(1|SSBS)")
# 
# PlotGLMERContinuous(model = testModelRichThermalIndex$model,data = testModelRichThermalIndex$data,
#                     effects = "ThermalIndex",otherFactors = list(Biome2="Tropical Forest"),
#                     byFactor = "LandUse3",xlab = "Temperature position",
#                     ylab = "Species richness",
#                     line.cols = c("#66A61E","#E6AB02","#D95F02","#1B9E77"))

# Moderate interaction between temperature index and land use - consider in final model

# testModelRichPrecipIndex <- GLMER(modelData = sites.div,
#                        responseVar = "Species_richness",
#                        fitFamily = "poisson",
#                        fixedStruct = "LandUse3+Biome2+poly(PrecipIndex,2)+LandUse3:Biome2+LandUse3:poly(PrecipIndex,2)",
#                        randomStruct = "(1|SS)+(1|SSBS)")
# 
# PlotGLMERContinuous(model = testModelRichPrecipIndex$model,data = testModelRichPrecipIndex$data,
#                     effects = "PrecipIndex",otherFactors = list(Biome2="Tropical Forest"),
#                     byFactor = "LandUse3",xlab = "Precipiation position",
#                     ylab = "Species richness",
#                     line.cols = c("#66A61E","#E6AB02","#D95F02","#1B9E77"))

# Strong interaction between precipitation index and land use - consider in final model

# testModelRichPropSpec <- GLMER(modelData = sites.div,
#                        responseVar = "Species_richness",
#                        fitFamily = "poisson",
#                        fixedStruct = "LandUse3+Biome2+poly(PropSpec,2)+LandUse3:Biome2+LandUse3:poly(PropSpec,2)",
#                        randomStruct = "(1|SS)+(1|SSBS)")
# 
# PlotGLMERContinuous(model = testModelRichPropSpec$model,data = testModelRichPropSpec$data,
#                     effects = "PropSpec",otherFactors = list(Biome2="Tropical Forest"),
#                     byFactor = "LandUse3",xlab = "Proportion of specialists",
#                     ylab = "Species richness",
#                     line.cols = c("#66A61E","#E6AB02","#D95F02","#1B9E77"))

# Strong interaction between proportion of specialists and land use
# However, correlated with time since landscape conversion and represents 
# only vertebrates so don't consider in final model

# testModelRichAgeConv <- GLMER(modelData = sites.div,
#                        responseVar = "Species_richness",
#                        fitFamily = "poisson",
#                        fixedStruct = "LandUse3+Biome2+poly(LogAgeConv,2)+LandUse3:Biome2+LandUse3:poly(LogAgeConv,2)",
#                        randomStruct = "(1|SS)+(1|SSBS)")
# 
# PlotGLMERContinuous(model = testModelRichAgeConv$model,data = testModelRichAgeConv$data,
#                     effects = "LogAgeConv",otherFactors = list(Biome2="Tropical Forest"),
#                     byFactor = "LandUse3",xlab = "Time since landscape conversion",
#                     ylab = "Species richness",
#                     line.cols = c("#66A61E","#E6AB02","#D95F02","#1B9E77"))

# Moderate interaction between age of landscape conversion and land use - consider in final model

# testModelRich <- GLMER(modelData = sites.div,
#                        responseVar = "Species_richness",
#                        fitFamily = "poisson",
#                        fixedStruct = "LandUse3+Biome2+poly(TempSeas,2)+poly(PrecipSeas,2)+poly(ThermalIndex,2)+poly(PrecipIndex,2)+poly(LogAgeConv,2)+LandUse3:Biome2+LandUse3:poly(TempSeas,2)+LandUse3:poly(PrecipSeas,2)+LandUse3:poly(ThermalIndex,2)+LandUse3:poly(PrecipIndex,2)+LandUse3:poly(LogAgeConv,2)",
#                        randomStruct = "(1|SS)+(1|SSBS)")

modelRich <- GLMERSelect(modelData = sites.div,responseVar = "Species_richness",
                          fitFamily = "poisson",
                          fixedFactors = c("LandUse3","Biome2"),
                          fixedTerms = list(TempSeas=2,
                                            PrecipSeas=2,ThermalIndex=2,
                                            PrecipIndex=2,
                                            LogAgeConv=2),
                          randomStruct = "(1|SS)+(1|SSBS)",
                          fixedInteractions = c("LandUse3:Biome2",
                                                # "LandUse3:poly(CWM_log.range,2)",
                                                "LandUse3:poly(TempSeas,2)",
                                                "LandUse3:poly(PrecipSeas,2)",
                                                "LandUse3:poly(ThermalIndex,2)",
                                                "LandUse3:poly(PrecipIndex,2)",
                                                "LandUse3:poly(LogAgeConv,2)"),
                          verbose = TRUE)

saveRDS(object = modelRich,file = paste0(outDir,"RichnessModel.rds"))

modelRichPlotting <- GLMER(modelData = sites.div,responseVar = "Species_richness",
                           fitFamily = "poisson",
                           fixedStruct = "LandUse3+poly(TempSeas,2)+poly(PrecipSeas,2)+poly(ThermalIndex,2)+poly(PrecipIndex,2)+poly(LogAgeConv,2)+LandUse3:poly(TempSeas,2)+LandUse3:poly(PrecipSeas,2)+LandUse3:poly(ThermalIndex,2)+LandUse3:poly(PrecipIndex,2)+LandUse3:poly(LogAgeConv,2)",
                           randomStruct = "(1|SS)+(1|SSBS)",REML = TRUE)

saveRDS(object = modelRichPlotting,file = paste0(outDir,"RichnessModelForPlotting.rds"))



modelAbund <- GLMERSelect(modelData = sites.div,responseVar = "LogAbund",
                         fitFamily = "gaussian",
                         fixedFactors = c("LandUse3","Biome2"),
                         fixedTerms = list(CWM_log.range=2,TempSeas=2,
                                           PrecipSeas=2,ThermalIndex=2,
                                           PrecipIndex=2,PropSpec=2),
                         randomStruct = "(1|SS)",
                         fixedInteractions = c("LandUse3:Biome2",
                                               "LandUse3:poly(CWM_log.range,2)",
                                               "LandUse3:poly(TempSeas,2)",
                                               "LandUse3:poly(PrecipSeas,2)",
                                               "LandUse3:poly(ThermalIndex,2)",
                                               "LandUse3:poly(PrecipIndex,2)",
                                               "LandUse3:poly(PropSpec,2)"),
                         verbose = TRUE)

saveRDS(object = modelAbund,file = paste0(outDir,"AbundanceModel.rds"))

modelAbundPlotting <- GLMER(modelData = sites.div,responseVar = "LogAbund",
                           fitFamily = "gaussian",
                           fixedStruct = "LandUse3+poly(CWM_log.range,2)+poly(TempSeas,2)+poly(PrecipSeas,2)+poly(ThermalIndex,2)+poly(PrecipIndex,2)+LandUse3:poly(CWM_log.range,2)+LandUse3:poly(TempSeas,2)+LandUse3:poly(PrecipSeas,2)+LandUse3:poly(ThermalIndex,2)+LandUse3:poly(PrecipIndex,2)",
                           randomStruct = "(1|SS)",REML = TRUE)

saveRDS(object = modelAbundPlotting,file = paste0(outDir,"AbundanceModelForPlotting.rds"))

t.end <- Sys.time()

print(round(t.end - t.start,0))

sink()