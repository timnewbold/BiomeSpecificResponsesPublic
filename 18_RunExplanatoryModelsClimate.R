outDir <- "18_RunExplanatoryModelsClimate/"

dataDir <- "0_data/"
climateSensDir <- "10_EstimateClimateSensitivityByBiome/"
meanRangeDir <- "17_CalculateMeanRangeMap/"
biomeDir <- "9_ProcessBiomeMap/"

sink(file = paste0(outDir,"log.txt"))

t.start <- Sys.time()

print(t.start)

suppressMessages(suppressWarnings(library(raster)))
suppressMessages(suppressWarnings(library(spdep)))

print(sessionInfo())

climateSens <- raster(paste0(climateSensDir,"ClimateSensitivity_85.grd"))

wgsCRS <- CRS('+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0')
behrCRS <- CRS('+proj=cea +lon_0=0 +lat_ts=30 +x_0=0 +y_0=0 +datum=WGS84 +ellps=WGS84 +units=m +no_defs')

tempSeas <- raster(paste0(dataDir,"bio_4"))

tempSeas <- suppressWarnings(
  projectRaster(from = tempSeas,crs = behrCRS))

tempSeas <- raster::resample(x = tempSeas,y = climateSens)

precipSeas <- raster(paste0(dataDir,"bio_15"))

precipSeas <- suppressWarnings(
  projectRaster(from = precipSeas,crs = behrCRS))

precipSeas <- raster::resample(x = precipSeas,y = climateSens)

ThermalIndex <- raster(paste0(dataDir,"VertebrateThermalIndex.grd"))

PrecipIndex <- raster(paste0(dataDir,"VertebratePrecipitationIndex.grd"))

MeanRange <- readRDS(paste0(dataDir,"MeanRangeSizeMap.rds"))

MeanRange <- raster::resample(x = MeanRange,y = climateSens)

PropSpec <- raster(paste0(dataDir,"Prop_spe_raster.grd"))

PropSpec <- raster::resample(x = PropSpec,y = climateSens)

Biome <- raster(paste0(biomeDir,"BiomeRaster.tif"))

mod.data <- data.frame(ClimateSens=values(climateSens),
                       TempSeas=values(tempSeas),
                       PrecipSeas=values(precipSeas),
                       ThermalIndex=values(ThermalIndex),
                       PrecipIndex=values(PrecipIndex),
                       MeanRange=values(MeanRange),
                       PropSpec=values(PropSpec),
                       Biome=factor(values(Biome)),
                       Longitude=coordinates(climateSens)[,1],
                       Latitude=coordinates(climateSens)[,2])
mod.data <- na.omit(mod.data)

mod.data <- mod.data[
  (mod.data$ClimateSens<0),]

mod.data$TempSeas <- log(mod.data$TempSeas)
mod.data$PrecipSeas <- log(mod.data$PrecipSeas+1)
mod.data$PrecipIndex <- asin(sqrt(mod.data$PrecipIndex))
mod.data$MeanRange <- log(mod.data$MeanRange)

mod.data$ClimateSens <- 
  sign(mod.data$ClimateSens) * 
  sqrt(abs(mod.data$ClimateSens))

m1 <- lm(ClimateSens~TempSeas+PrecipSeas+ThermalIndex+
           PrecipIndex+Biome,
         data=mod.data)

mod.data <- mod.data[sample(x = 1:nrow(mod.data),size = 10000,replace = FALSE),]

nb <- tri2nb(coords = mod.data[,c('Longitude','Latitude')])
lw <- nb2listw(neighbours = nb)

m2 <- lagsarlm(ClimateSens~TempSeas+PrecipSeas+
                 ThermalIndex+PrecipIndex+MeanRange+
                 PropSpec+Biome,
               data=mod.data,listw = lw)

saveRDS(object = m2,file = paste0(outDir,"SARModel.rds"))

pdf(file = paste0(outDir,"Figure5.pdf"),
     width = 18/2.54,height = 13/2.54)

par(mfrow=c(2,2))
par(tck=-0.01)
par(mar=c(2.4,2.4,0.3,0.2))
par(mgp=c(1.4,0.2,0))
par(las=1)
par(cex=1.0)
par(cex.axis=1.0)
par(cex.lab=1.0)
par(cex.main=1.0)
par(ps=10)

nd <- data.frame(TempSeas=seq(from=min(mod.data$TempSeas),
                              to = max(mod.data$TempSeas),
                              length.out=100),
                 PrecipSeas=median(mod.data$PrecipSeas),
                 ThermalIndex=median(mod.data$ThermalIndex),
                 PrecipIndex=median(mod.data$PrecipIndex),
                 MeanRange=median(mod.data$MeanRange),
                 PropSpec=median(mod.data$PropSpec),
                 Biome=factor("1",levels=levels(mod.data$Biome)))

preds <- predict(object = m1,newdata=nd,se.fit=TRUE)

y <- preds$fit
yplus <- preds$fit + 1.96*preds$se.fit
yminus <- preds$fit - 1.96*preds$se.fit

xVals <- c(150,400,1000,3000,8000,22000)
xValsT <- log(xVals)

yVals <- c(0,-1,-4,-9,-16,-25,-36)
yValsT <- sign(yVals) * sqrt(abs(yVals))

plot(mod.data$TempSeas,
     mod.data$ClimateSens,
     pch=16,cex=0.2,
     xlab="Temperature seasonality",
     ylab="% Richness change / \u00b0C",
     xaxt="n",yaxt="n",bty="l")

axis(1,at=xValsT,labels=xVals)
axis(2,at=yValsT,labels=yVals)

points(nd$TempSeas,y,type="l",col="#E41A1C",lwd=1)
points(nd$TempSeas,yplus,type="l",lty=2,col="#E41A1C",lwd=1)
points(nd$TempSeas,yminus,type="l",lty=2,col="#E41A1C",lwd=1)

mtext(text = "a",side = 3,line = -0.6,adj = -0.14,font = 2,ps = 10)

nd <- data.frame(TempSeas=median(mod.data$TempSeas),
                 PrecipSeas=seq(from=2.2,
                              to = max(mod.data$PrecipSeas),
                              length.out=100),
                 ThermalIndex=median(mod.data$ThermalIndex),
                 PrecipIndex=median(mod.data$PrecipIndex),
                 MeanRange=median(mod.data$MeanRange),
                 PropSpec=median(mod.data$PropSpec),
                 Biome=factor("1",levels=levels(mod.data$Biome)))

preds <- predict(object = m1,newdata=nd,se.fit=TRUE)

y <- preds$fit
yplus <- preds$fit + 1.96*preds$se.fit
yminus <- preds$fit - 1.96*preds$se.fit

xVals <- c(8,20,50,150)
xValsT <- log(xVals+1)

plot(mod.data$PrecipSeas,
     mod.data$ClimateSens,
     pch=16,cex=0.2,
     xlab="Precipitation seasonality",
     ylab="% Richness change / \u00b0C",
     xlim=c(2,5.4),
     xaxt="n",yaxt="n",bty="l")

axis(1,at=xValsT,labels=xVals)
axis(2,at=yValsT,labels=yVals)

points(nd$PrecipSeas,y,type="l",col="#E41A1C",lwd=1)
points(nd$PrecipSeas,yplus,type="l",lty=2,col="#E41A1C",lwd=1)
points(nd$PrecipSeas,yminus,type="l",lty=2,col="#E41A1C",lwd=1)

mtext(text = "b",side = 3,line = -0.6,adj = -0.14,font = 2,ps = 10)

nd <- data.frame(TempSeas=median(mod.data$TempSeas),
                 PrecipSeas=median(mod.data$PrecipSeas),
                 ThermalIndex=seq(from=min(mod.data$ThermalIndex),
                              to = max(mod.data$ThermalIndex),
                              length.out=100),
                 PrecipIndex=median(mod.data$PrecipIndex),
                 MeanRange=median(mod.data$MeanRange),
                 PropSpec=median(mod.data$PropSpec),
                 Biome=factor("1",levels=levels(mod.data$Biome)))

preds <- predict(object = m1,newdata=nd,se.fit=TRUE)

y <- preds$fit
yplus <- preds$fit + 1.96*preds$se.fit
yminus <- preds$fit - 1.96*preds$se.fit

xVals <- c(0,0.05,0.15,0.3,0.5,0.7)
xValsT <- asin(sqrt(xVals))

plot(mod.data$ThermalIndex,
     mod.data$ClimateSens,
     pch=16,cex=0.2,
     xlab="Thermal position index",
     ylab="% Richness change / \u00b0C",
     yaxt="n",bty="l")

axis(2,at=yValsT,labels=yVals)

points(nd$ThermalIndex,y,type="l",col="#E41A1C",lwd=1)
points(nd$ThermalIndex,yplus,type="l",lty=2,col="#E41A1C",lwd=1)
points(nd$ThermalIndex,yminus,type="l",lty=2,col="#E41A1C",lwd=1)

mtext(text = "c",side = 3,line = -0.6,adj = -0.14,font = 2,ps = 10)

nd <- data.frame(TempSeas=median(mod.data$TempSeas),
                 PrecipSeas=median(mod.data$PrecipSeas),
                 ThermalIndex=median(mod.data$ThermalIndex),
                 PrecipIndex=seq(from=min(mod.data$PrecipIndex),
                              to = max(mod.data$PrecipIndex),
                              length.out=100),
                 MeanRange=median(mod.data$MeanRange),
                 PropSpec=median(mod.data$PropSpec),
                 Biome=factor("1",levels=levels(mod.data$Biome)))

preds <- predict(object = m1,newdata=nd,se.fit=TRUE)

y <- preds$fit
yplus <- preds$fit + 1.96*preds$se.fit
yminus <- preds$fit - 1.96*preds$se.fit

plot(mod.data$PrecipIndex,
     mod.data$ClimateSens,
     pch=16,cex=0.2,
     xlab="Precipitation position index",
     ylab="% Richness change / \u00b0C",
     xaxt="n",yaxt="n",bty="l")

axis(1,at=xValsT,labels=xVals)
axis(2,at=yValsT,labels=yVals)

points(nd$PrecipIndex,y,type="l",col="#E41A1C",lwd=1)
points(nd$PrecipIndex,yplus,type="l",lty=2,col="#E41A1C",lwd=1)
points(nd$PrecipIndex,yminus,type="l",lty=2,col="#E41A1C",lwd=1)

mtext(text = "d",side = 3,line = -0.6,adj = -0.14,font = 2,ps = 10)

invisible(dev.off())

t.end <- Sys.time()

print(round(t.end - t.start,0))

sink()