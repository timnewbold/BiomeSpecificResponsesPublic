suppressMessages(suppressWarnings(library(StatisticalModels)))

modelsDir <- "15_RunExplanatoryModels/"

outDir <- "16_PlotExplanatoryModels/"

modelRichPlotting <- readRDS(paste0(modelsDir,"RichnessModelForPlotting.rds"))
modelAbundPlotting <- readRDS(paste0(modelsDir,"AbundanceModelForPlotting.rds"))

tiff(filename = paste0(outDir,"RichnessResults.tif"),width = 17.5,
    height = 18,units = "cm",res = 150,compression = "lzw")

par(mfrow=c(3,2))

suppressWarnings(PlotGLMERContinuous(model = modelRichPlotting$model,data = modelRichPlotting$data,
                    effects = c("TempSeas"),
                    otherContEffects = c("LogAgeConv",
                                         "ThermalIndex","PrecipIndex",
                                         "PrecipSeas"),
                    # otherFactors = list(Biome2="Tropical Forest"),
                    byFactor = "LandUse3",xlab="Temperature seasonality (s.d.\u00d7100)",
                    ylab = "Richness",logLink = "e",seMultiplier = 1.0,
                    line.cols = c("#66A61E","#E6AB02","#D95F02","#1B9E77"),
                    xlim = c(712,6128),ylim=c(9.5,20),
                    params = list(tck=-0.01,mar=c(2.6,2.6,0.2,0.2),
                                  mgp=c(1.4,0.2,0),las=1,cex=1,
                                  cex.axis=1,cex.lab=1)))

mtext(text = "a",side = 3,line = -0.7,adj = -0.14,font = 2,ps=10)

suppressWarnings(PlotGLMERContinuous(model = modelRichPlotting$model,data = modelRichPlotting$data,
                    effects = c("PrecipSeas"),
                    otherContEffects = c("TempSeas","LogAgeConv",
                                         "ThermalIndex","PrecipIndex"),
                    # otherFactors = list(Biome2="Tropical Forest"),
                    byFactor = "LandUse3",xlab="Precipitation seasonality (coef. var.)",
                    ylab = "Richness",logLink = "e",seMultiplier = 1.0,
                    line.cols = c("#66A61E","#E6AB02","#D95F02","#1B9E77"),
                    xlim = c(21,66),ylim=c(11,20),
                    params = list(tck=-0.01,mar=c(2.6,2.6,0.2,0.2),
                                  mgp=c(1.4,0.2,0),las=1,cex=1,
                                  cex.axis=1,cex.lab=1)))

mtext(text = "b",side = 3,line = -0.7,adj = -0.14,font = 2,ps=10)

suppressWarnings(PlotGLMERContinuous(model = modelRichPlotting$model,data = modelRichPlotting$data,
                    effects = c("ThermalIndex"),
                    otherContEffects = c("TempSeas","LogAgeConv",
                                         "PrecipSeas","PrecipIndex"),
                    # otherFactors = list(Biome2="Tropical Forest"),
                    byFactor = "LandUse3",xlab="Thermal position index",
                    ylab = "Richness",logLink = "e",seMultiplier = 1.0,
                    line.cols = c("#66A61E","#E6AB02","#D95F02","#1B9E77"),
                    xlim = c(0.38,0.85),
                    params = list(tck=-0.01,mar=c(2.6,2.6,0.2,0.2),
                                  mgp=c(1.4,0.2,0),las=1,cex=1,
                                  cex.axis=1,cex.lab=1)))

mtext(text = "c",side = 3,line = -0.7,adj = -0.14,font = 2,ps=10)

suppressWarnings(PlotGLMERContinuous(model = modelRichPlotting$model,data = modelRichPlotting$data,
                    effects = c("PrecipIndex"),
                    otherContEffects = c("TempSeas","LogAgeConv",
                                         "PrecipSeas","ThermalIndex"),
                    # otherFactors = list(Biome2="Tropical Forest"),
                    byFactor = "LandUse3",xlab="Precipitation position index",
                    ylab = "Richness",logLink = "e",seMultiplier = 1.0,
                    line.cols = c("#66A61E","#E6AB02","#D95F02","#1B9E77"),
                    xlim = c(0.05,0.38),
                    params = list(tck=-0.01,mar=c(2.6,2.6,0.2,0.2),
                                  mgp=c(1.4,0.2,0),las=1,cex=1,
                                  cex.axis=1,cex.lab=1)))

mtext(text = "d",side = 3,line = -0.7,adj = -0.14,font = 2,ps=10)

suppressWarnings(PlotGLMERContinuous(model = modelRichPlotting$model,data = modelRichPlotting$data,
                    effects = c("LogAgeConv"),
                    otherContEffects = c("PrecipSeas","TempSeas",
                                         "ThermalIndex","PrecipIndex"),
                    # otherFactors = list(Biome2="Tropical Forest"),
                    byFactor = "LandUse3",xlab="Years since landscape conversion",
                    ylab = "Richness",logLink = "e",seMultiplier = 1.0,
                    line.cols = c("#66A61E","#E6AB02","#D95F02","#1B9E77"),transformX = TRUE,
                    xlim=c(1,600),
                    params = list(tck=-0.01,mar=c(2.6,2.6,0.2,0.2),
                                  mgp=c(1.4,0.2,0),las=1,cex=1,
                                  cex.axis=1,cex.lab=1)))

mtext(text = "e",side = 3,line = -0.7,adj = -0.14,font = 2,ps=10)

plot.new()

legend(x = 0.05,y = 0.7,lty=1,cex=1,bty="n",
       col=c("#66A61E","#1B9E77","#E6AB02","#D95F02"),
       legend = c("Primary Vegetation","Secondary Vegetation",
                  "Harvested agriculture","Pasture"))

invisible(dev.off())



png(filename = paste0(outDir,"AbundanceResults.png"),width = 17.5,
    height = 24,units = "cm",res = 1200)

par(mfrow=c(3,2))


PlotGLMERContinuous(model = modelAbundPlotting$model,data = modelAbundPlotting$data,
                    effects = c("CWM_log.range"),
                    otherContEffects = c("PrecipSeas","TempSeas",
                                         "ThermalIndex","PrecipIndex"),
                    # otherFactors = list(Biome2="Tropical Grasslands"),
                    byFactor = "LandUse3",xlab="CWM range",
                    ylab = "Abundance",logLink = "e",seMultiplier = 1.0,
                    line.cols = c("#66A61E","#E6AB02","#D95F02","#1B9E77"))

PlotGLMERContinuous(model = modelAbundPlotting$model,data = modelAbundPlotting$data,
                    effects = c("TempSeas"),
                    otherContEffects = c("PrecipSeas","CWM_log.range",
                                         "ThermalIndex","PrecipIndex"),
                    # otherFactors = list(Biome2="Tropical Grasslands"),
                    byFactor = "LandUse3",xlab="Temperature seasonality",
                    ylab = "Abundance",logLink = "e",seMultiplier = 1.0,
                    line.cols = c("#66A61E","#E6AB02","#D95F02","#1B9E77"))

PlotGLMERContinuous(model = modelAbundPlotting$model,data = modelAbundPlotting$data,
                    effects = c("PrecipSeas"),
                    otherContEffects = c("TempSeas","CWM_log.range",
                                         "ThermalIndex","PrecipIndex"),
                    # otherFactors = list(Biome2="Tropical Grasslands"),
                    byFactor = "LandUse3",xlab="Precipitation seasonality",
                    ylab = "Abundance",logLink = "e",seMultiplier = 1.0,
                    line.cols = c("#66A61E","#E6AB02","#D95F02","#1B9E77"))

PlotGLMERContinuous(model = modelAbundPlotting$model,data = modelAbundPlotting$data,
                    effects = c("ThermalIndex"),
                    otherContEffects = c("TempSeas","CWM_log.range",
                                         "PrecipSeas","PrecipIndex"),
                    # otherFactors = list(Biome2="Tropical Grasslands"),
                    byFactor = "LandUse3",xlab="Temperature position",
                    ylab = "Abundance",logLink = "e",seMultiplier = 1.0,
                    line.cols = c("#66A61E","#E6AB02","#D95F02","#1B9E77"))

PlotGLMERContinuous(model = modelAbundPlotting$model,data = modelAbundPlotting$data,
                    effects = c("PrecipIndex"),
                    otherContEffects = c("TempSeas","CWM_log.range",
                                         "PrecipSeas","ThermalIndex"),
                    # otherFactors = list(Biome2="Tropical Grasslands"),
                    byFactor = "LandUse3",xlab="Precipitation position",
                    ylab = "Abundance",logLink = "e",seMultiplier = 1.0,
                    line.cols = c("#66A61E","#E6AB02","#D95F02","#1B9E77"))

invisible(dev.off())



