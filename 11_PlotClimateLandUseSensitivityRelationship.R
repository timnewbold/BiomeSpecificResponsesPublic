
landuseSensitivityDir <- "6_PlotLandUseModels/"
climateSensitivityDir <- "10_EstimateClimateSensitivityByBiome/"

outDir <- "11_PlotClimateLandUseSensitivityRelationship/"

luSens <- readRDS(paste0(landuseSensitivityDir,"BiomeRichnessSensitivityHumanLU.rds"))

climateSens <- readRDS(paste0(climateSensitivityDir,"BiomeClimateSensitivities_85.rds"))

stopifnot(all(names(luSens)==names(climateSens)))

biome.cols <- c('Tropical Forest' = '#0072B2',
                'Tropical Grasslands' = '#E69F00',
                'Drylands' = '#D55E00',
                'Mediterranean' = '#CC79A7',
                'Temperate Forest' = '#56B4E9',
                'Temperate Grasslands' = '#F0E442')

png(filename = paste0(outDir,"ClimateVersusLandUseSensitivity.png"),width = 12.5,height = 10,units = "cm",res = 150)

par(las=1)
par(tck=-0.01)
par(mgp=c(1.8,0.2,0))
par(mar=c(3.2,3.2,0.2,0.2))
par(cex.axis=1)
par(cex.lab=1.3)

plot(climateSens,luSens,pch=21,cex=2,bg=biome.cols[names(luSens)],
     xlab="Richness change / \u00b0C (%)",
     ylab="Richness change human land (%)")

legend(
  x = -12,-2.5,xjust=0,yjust=1,pch=21,pt.bg=biome.cols,pt.cex=2,cex=1,bty="n",
  c("Tropical Forest","Tropical Grasslands","Drylands","Mediterranean","Temperate Forest","Temperate Grasslands"))

invisible(dev.off())