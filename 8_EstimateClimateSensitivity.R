outDir <- "8_EstimateClimateSensitivity/"

dataDir <- "0_data/"
inDir <- "7_CalculateClimateDeltas/"

sink(file = paste0(outDir,"log.txt"))

t.start <- Sys.time()

print(t.start)

suppressMessages(suppressWarnings(library(raster)))

print(sessionInfo())

load(paste0(dataDir,"77_ProcessFutureProjectionsRandomForestsRealisticDispersal/ProjectionResults.Rd"))

deltaRichness_85_2070 <- raster(projResults$AllVertebrates$`85`$`70`$deltaMap)

values(deltaRichness_85_2070) <- values(deltaRichness_85_2070)+1

deltaTemperature <- raster(paste0(inDir,"DeltaTemp_85_2070.tif"))

stopifnot(all.equal(extent(deltaRichness_85_2070),extent(deltaTemperature)))
stopifnot(all.equal(res(deltaRichness_85_2070),res(deltaTemperature)))

# plot(values(deltaTemperature)+1,values(deltaRichness_85_2070)+1,log="xy")

m1 <- lm(log(values(deltaRichness_85_2070)+1)~log(values(deltaTemperature)+1))

print(summary(m1))

t.end <- Sys.time()

print(round(t.end - t.start,0))

sink()