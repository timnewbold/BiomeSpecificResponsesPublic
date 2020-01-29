suppressMessages(suppressWarnings(library(raster)))

sensMapsDir <- "10_EstimateClimateSensitivityByBiome/"

outDir <- "19_CompareScenarioClimateSensitivities/"

climSens26 <- readRDS(paste0(sensMapsDir,"ClimateSensitivity_26.rds"))
climSens85 <- readRDS(paste0(sensMapsDir,"ClimateSensitivity_85.rds"))

df <- data.frame(rcp26=values(climSens26),
                 rcp85=values(climSens85))

df <- na.omit(df)

posVals85 <- df[df$rcp85>0,]
negVals85 <- df[df$rcp85<0,]

tiff(filename = paste0(outDir,"ScenarioClimateSensitivityComparison.tif"),
     width = 17.5,height = 8,units = "cm",compression = "lzw",res = 150)

par(mfrow=c(1,2))

par(las=1)
par(tck=-0.01)
par(mgp=c(1.8,0.2,0))
par(mar=c(3.2,3.2,0.2,0.2))
par(cex=1)
par(cex.axis=1)
par(cex.lab=1)

cat("For cells where RCP8.5 predicts biodiversity decrease:\n")
cat("RCP8.5 - RCP2.6:\n")
print(summary(negVals85$rcp85 - negVals85$rcp26))
# RCP 2.6 predicts more positive outcomes on average
# Median = 1.7% more species lost per degree increase under RCP8.5

with(negVals85[sample(x = 1:nrow(negVals85),size = 10000,replace = FALSE),],
     plot(rcp85,rcp26,pch=16,cex=0.5,
          xlab="% Richness change / \u00b0C (RCP8.5)",
          ylab="% Richness change / \u00b0C (RCP2.6)"))
abline(0,1,lwd=2,col="#ff0000")

cat("For cells where RCP8.5 predicts biodiversity increase:\n")
cat("RCP8.5 - RCP2.6:\n")
print(summary(posVals85$rcp85 - posVals85$rcp26))
# RCP 2.6 predicts more positive outcomes on average
# Median = 4.3% more species lost per degree increase under RCP8.5

with(posVals85[sample(x = 1:nrow(posVals85),size = 10000,replace = FALSE),],
     plot(rcp85,rcp26,pch=16,cex=0.5,
          xlab="% Richness change / \u00b0C (RCP8.5)",
          ylab="% Richness change / \u00b0C (RCP2.6)"))
abline(0,1,lwd=2,col="#ff0000")

invisible(dev.off())