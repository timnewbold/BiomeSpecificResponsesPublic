outDir <- "11_PlotClimateLandUseSensitivityRelationship/"

landuseSensitivityDir <- "6_PlotLandUseModels/"
climateSensitivityDir <- "10_EstimateClimateSensitivityByBiome/"

sink(file = paste0(outDir,"log.txt"))

t.start <- Sys.time()

print(t.start)

print(sessionInfo())

luSens <- readRDS(paste0(
  landuseSensitivityDir,
  "BiomeRichnessSensitivityHumanLU.rds"))

climateSens <- readRDS(paste0(
  climateSensitivityDir,
  "BiomeClimateSensitivities_85.rds"))

stopifnot(all(names(luSens)==names(climateSens)))

biome.cols <- c('Tropical Forest' = '#0072B2',
                'Tropical Grasslands' = '#E69F00',
                'Drylands' = '#D55E00',
                'Mediterranean' = '#CC79A7',
                'Temperate Forest' = '#56B4E9',
                'Temperate Grasslands' = '#F0E442')

pdf(file = paste0(outDir,"Figure3.pdf"),
     width = 8.8/2.54,height = 9/2.54)

layout.mat <- matrix(data = c(1,2),nrow = 2,ncol = 1,byrow = TRUE)
layout(mat = layout.mat,widths = 8.8,heights = c(7,2))

par(las=1)
par(tck=-0.01)
par(mgp=c(1.4,0.2,0))
par(mar=c(2.4,2.4,0.2,0.2))
par(cex=1.0)
par(cex.axis=1.0)
par(cex.lab=1.0)
par(cex.main=1.0)
par(ps=10)

plot(climateSens,luSens,pch=21,cex=2,bg=biome.cols[names(luSens)],
     xlab="Richness change / \u00b0C (%)",
     ylab="Richness change human-dominated LU (%)",
     xlim=c(-12,1),ylim=c(-35,3),
     bty="l")

plot.new()

par(mar=c(0,0,0,0))

legend(
  x = -0.2,y = 1,xjust=0,yjust=1,pch=21,pt.bg=biome.cols,
  pt.cex=2,cex=1,bty="n",ncol = 2,y.intersp = 2.2,xpd = TRUE,
  c("Tropical Forest","Tropical Grasslands",
    "Drylands","Mediterranean","Temperate Forest",
    "Temperate Grasslands"))

invisible(dev.off())

t.end <- Sys.time()

print(round(t.end - t.start,0))

sink()