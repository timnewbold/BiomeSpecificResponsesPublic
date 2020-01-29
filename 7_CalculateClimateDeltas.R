suppressMessages(suppressWarnings(library(raster)))

print(installed.packages()['base',]['Package'])
print(installed.packages()['base',]['Version'])

print(installed.packages()['raster',]['Package'])
print(installed.packages()['raster',]['Version'])

dataDir <- "0_data/"

outDir <- "7_CalculateClimateDeltas/"

currentTemp <- raster::raster(paste0(dataDir,"meantemp"))
futureTemp85 <- raster::raster(paste0(dataDir,"85bi701"))
futureTemp26 <- raster::raster(paste0(dataDir,"26bi701"))

stopifnot(all.equal(raster::extent(currentTemp),raster::extent(futureTemp85)))
stopifnot(all.equal(raster::res(currentTemp),raster::res(futureTemp85)))

stopifnot(all.equal(raster::extent(currentTemp),raster::extent(futureTemp26)))
stopifnot(all.equal(raster::res(currentTemp),raster::res(futureTemp26)))

deltaTemp85 <- futureTemp85 - currentTemp
deltaTemp26 <- futureTemp26 - currentTemp

raster::writeRaster(x = deltaTemp85,filename = paste0(
  outDir,"DeltaTemp_85_2070.tif"),format="GTiff")
raster::writeRaster(x = deltaTemp26,filename = paste0(
  outDir,"DeltaTemp_26_2070.tif"),format="GTiff")
