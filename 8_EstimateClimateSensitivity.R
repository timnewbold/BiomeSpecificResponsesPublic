suppressMessages(suppressWarnings(library(raster)))

print(installed.packages()['base',]['Package'])
print(installed.packages()['base',]['Version'])

print(installed.packages()['raster',]['Package'])
print(installed.packages()['raster',]['Version'])

dataDir <- "0_data/"
inDir <- "7_CalculateClimateDeltas/"

load(paste0(dataDir,"77_ProcessFutureProjectionsRandomForestsRealisticDispersal/ProjectionResults.Rd"))

deltaRichness_85_2070 <- raster(projResults$AllVertebrates$`85`$`70`$deltaMap)

values(deltaRichness_85_2070) <- values(deltaRichness_85_2070)+1

deltaTemperature <- raster(paste0(inDir,"DeltaTemp_85_2070.tif"))

stopifnot(all.equal(extent(deltaRichness_85_2070),extent(deltaTemperature)))
stopifnot(all.equal(res(deltaRichness_85_2070),res(deltaTemperature)))

# plot(values(deltaTemperature)+1,values(deltaRichness_85_2070)+1,log="xy")

m1 <- lm(log(values(deltaRichness_85_2070)+1)~log(values(deltaTemperature)+1))

print(summary(m1))

