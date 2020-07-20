outDir <- "7_CalculateClimateDeltas/"

dataDir <- "0_data/"

sink(file = paste0(outDir,"log.txt"))

t.start <- Sys.time()

print(t.start)

suppressMessages(suppressWarnings(library(raster)))

print(sessionInfo())

currentTemp <- raster::raster(paste0(dataDir,"meantemp"))
futureTemp85 <- raster::raster(paste0(dataDir,"85bi701"))
futureTemp60 <- raster::raster(paste0(dataDir,"60bi701"))
futureTemp45 <- raster::raster(paste0(dataDir,"45bi701"))
futureTemp26 <- raster::raster(paste0(dataDir,"26bi701"))

stopifnot(all.equal(raster::extent(currentTemp),raster::extent(futureTemp85)))
stopifnot(all.equal(raster::res(currentTemp),raster::res(futureTemp85)))

stopifnot(all.equal(raster::extent(currentTemp),raster::extent(futureTemp26)))
stopifnot(all.equal(raster::res(currentTemp),raster::res(futureTemp26)))

deltaTemp85 <- futureTemp85 - currentTemp
deltaTemp60 <- futureTemp60 - currentTemp
deltaTemp45 <- futureTemp45 - currentTemp
deltaTemp26 <- futureTemp26 - currentTemp

raster::writeRaster(x = deltaTemp85,filename = paste0(
  outDir,"DeltaTemp_85_2070.tif"),format="GTiff")
raster::writeRaster(x = deltaTemp60,filename = paste0(
  outDir,"DeltaTemp_60_2070.tif"),format="GTiff")
raster::writeRaster(x = deltaTemp45,filename = paste0(
  outDir,"DeltaTemp_45_2070.tif"),format="GTiff")
raster::writeRaster(x = deltaTemp26,filename = paste0(
  outDir,"DeltaTemp_26_2070.tif"),format="GTiff")

t.end <- Sys.time()

print(round(t.end - t.start,0))

sink()