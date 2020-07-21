outDir <- "16_PlotExplanatoryModels/"

modelsDir <- "15_RunExplanatoryModels/"

sink(file = paste0(outDir,"log.txt"))

t.start <- Sys.time()

print(t.start)

suppressMessages(suppressWarnings(library(StatisticalModels)))

print(sessionInfo())

modelRichPlotting <- readRDS(paste0(modelsDir,"RichnessModelForPlotting.rds"))
modelAbundPlotting <- readRDS(paste0(modelsDir,"AbundanceModelForPlotting.rds"))

pdf(file = paste0(outDir,"Figure4.pdf"),width = 18/2.54,
    height = 18/2.54)

par(mfrow=c(3,2))

par(tck=-0.01)
par(mgp=c(1.4,0.2,0))
par(mar=c(2.4,2.4,0.2,0.2))
par(las=1)
par(bty="l")
par(cex=1.0)
par(cex.axis=1.0)
par(cex.lab=1.0)
par(cex.main=1.0)
par(ps=10)

plot(-9e99,-9e99,xlim=c(700,6200),ylim=c(10,18.4),
     xlab="Temperature seasonality (s.d.\u00d7100)",ylab = "Richness")

invisible(mapply(FUN = function(lu,col){
  
  nd <- with(modelRichPlotting$data,
             data.frame(TempSeas=seq(from = 712,
                                     to = 6128,
                                     length.out = 100),
                        LandUse3=factor(x = lu,levels = levels(LandUse3)),
                        Species_richness=0,
                        PrecipSeas=median(PrecipSeas),
                        ThermalIndex=median(ThermalIndex),
                        PrecipIndex=median(PrecipIndex),
                        LogAgeConv=median(LogAgeConv)))

  preds <- PredictGLMERRandIter(model = modelRichPlotting$model,data = nd,nIters = 100)
   
  preds <- exp(preds)
  
  preds.median <- apply(X = preds,MARGIN = 1,FUN = median)
  preds.upper <- apply(X = preds,MARGIN = 1,FUN = quantile,probs = (1/6)) 
  preds.lower <- apply(X = preds,MARGIN = 1,FUN = quantile,probs = (5/6)) 
  
  X.Vec <- c(nd$TempSeas, max(nd$TempSeas), 
             rev(nd$TempSeas), min(nd$TempSeas))
  Y.Vec <- c(preds.lower, tail(preds.upper, 1), 
             rev(preds.upper), (preds.lower)[1])
  
  polygon(x = X.Vec,y = Y.Vec,col=paste0(col,"33"),border=NA)
  
  points(x = nd$TempSeas,y = preds.median,type="l",col=col,lwd=2)
  
},c("Primary Vegetation","Secondary Vegetation","Harvested","Pasture"),
c("#66A61E","#1B9E77","#E6AB02","#D95F02")))

mtext(text = "a",side = 3,line = -0.7,adj = -0.14,font = 2,ps = 10)

plot(-9e99,-9e99,xlim=c(20,70),ylim=c(11.6,18.8),
     xlab="Precipitation seasonality (coef. var.)",ylab = "Richness")

invisible(mapply(FUN = function(lu,col){
  
  nd <- with(modelRichPlotting$data,
             data.frame(PrecipSeas=seq(from = 21,
                                     to = 66,
                                     length.out = 100),
                        LandUse3=factor(x = lu,levels = levels(LandUse3)),
                        Species_richness=0,
                        TempSeas=median(TempSeas),
                        ThermalIndex=median(ThermalIndex),
                        PrecipIndex=median(PrecipIndex),
                        LogAgeConv=median(LogAgeConv)))
  
  preds <- PredictGLMERRandIter(model = modelRichPlotting$model,data = nd,nIters = 100)
  
  preds <- exp(preds)
  
  preds.median <- apply(X = preds,MARGIN = 1,FUN = median)
  preds.upper <- apply(X = preds,MARGIN = 1,FUN = quantile,probs = (1/6)) 
  preds.lower <- apply(X = preds,MARGIN = 1,FUN = quantile,probs = (5/6)) 
  
  X.Vec <- c(nd$PrecipSeas, max(nd$PrecipSeas), 
             rev(nd$PrecipSeas), min(nd$PrecipSeas))
  Y.Vec <- c(preds.lower, tail(preds.upper, 1), 
             rev(preds.upper), (preds.lower)[1])
  
  polygon(x = X.Vec,y = Y.Vec,col=paste0(col,"33"),border=NA)
  
  points(x = nd$PrecipSeas,y = preds.median,type="l",col=col,lwd=2)
  
},c("Primary Vegetation","Secondary Vegetation","Harvested","Pasture"),
c("#66A61E","#1B9E77","#E6AB02","#D95F02")))

mtext(text = "b",side = 3,line = -0.7,adj = -0.14,font = 2,ps=10)

plot(-9e99,-9e99,xlim=c(0.54,0.77),ylim=c(11.6,19.3),
     xlab="Thermal position index",ylab = "Richness")

invisible(mapply(FUN = function(lu,col){
  
  nd <- with(modelRichPlotting$data,
             data.frame(ThermalIndex=seq(from = 0.56,
                                       to = 0.75,
                                       length.out = 100),
                        LandUse3=factor(x = lu,levels = levels(LandUse3)),
                        Species_richness=0,
                        TempSeas=median(TempSeas),
                        PrecipSeas=median(PrecipSeas),
                        PrecipIndex=median(PrecipIndex),
                        LogAgeConv=median(LogAgeConv)))
  
  preds <- PredictGLMERRandIter(model = modelRichPlotting$model,data = nd,nIters = 100)
  
  preds <- exp(preds)
  
  preds.median <- apply(X = preds,MARGIN = 1,FUN = median)
  preds.upper <- apply(X = preds,MARGIN = 1,FUN = quantile,probs = (1/6)) 
  preds.lower <- apply(X = preds,MARGIN = 1,FUN = quantile,probs = (5/6)) 
  
  X.Vec <- c(nd$ThermalIndex, max(nd$ThermalIndex), 
             rev(nd$ThermalIndex), min(nd$ThermalIndex))
  Y.Vec <- c(preds.lower, tail(preds.upper, 1), 
             rev(preds.upper), (preds.lower)[1])
  
  polygon(x = X.Vec,y = Y.Vec,col=paste0(col,"33"),border=NA)
  
  points(x = nd$ThermalIndex,y = preds.median,type="l",col=col,lwd=2)
  
},c("Primary Vegetation","Secondary Vegetation","Harvested","Pasture"),
c("#66A61E","#1B9E77","#E6AB02","#D95F02")))

mtext(text = "c",side = 3,line = -0.7,adj = -0.14,font = 2,ps=10)
 
plot(-9e99,-9e99,xlim=c(0.07,0.19),ylim=c(11.6,20),
     xlab="Precipitation position index",ylab = "Richness")

invisible(mapply(FUN = function(lu,col){
  
  nd <- with(modelRichPlotting$data,
             data.frame(PrecipIndex=seq(from = 0.077,
                                         to = 0.183,
                                         length.out = 100),
                        LandUse3=factor(x = lu,levels = levels(LandUse3)),
                        Species_richness=0,
                        TempSeas=median(TempSeas),
                        PrecipSeas=median(PrecipSeas),
                        ThermalIndex=median(ThermalIndex),
                        LogAgeConv=median(LogAgeConv)))
  
  preds <- PredictGLMERRandIter(model = modelRichPlotting$model,data = nd,nIters = 100)
  
  preds <- exp(preds)
  
  preds.median <- apply(X = preds,MARGIN = 1,FUN = median)
  preds.upper <- apply(X = preds,MARGIN = 1,FUN = quantile,probs = (1/6)) 
  preds.lower <- apply(X = preds,MARGIN = 1,FUN = quantile,probs = (5/6)) 
  
  X.Vec <- c(nd$PrecipIndex, max(nd$PrecipIndex), 
             rev(nd$PrecipIndex), min(nd$PrecipIndex))
  Y.Vec <- c(preds.lower, tail(preds.upper, 1), 
             rev(preds.upper), (preds.lower)[1])
  
  polygon(x = X.Vec,y = Y.Vec,col=paste0(col,"33"),border=NA)
  
  points(x = nd$PrecipIndex,y = preds.median,type="l",col=col,lwd=2)
  
},c("Primary Vegetation","Secondary Vegetation","Harvested","Pasture"),
c("#66A61E","#1B9E77","#E6AB02","#D95F02")))

mtext(text = "d",side = 3,line = -0.7,adj = -0.14,font = 2,ps=10)

plot(9e99,9e99,xlim=c(1,170),ylim=c(11.6,22),log="x",
     xlab="Years since landscape conversion",ylab = "Richness")

invisible(mapply(FUN = function(lu,col){
  
  nd <- with(modelRichPlotting$data,
             data.frame(LogAgeConv=seq(from = 0.6931472,
                                        to = 5.13,
                                        length.out = 100),
                        LandUse3=factor(x = lu,levels = levels(LandUse3)),
                        Species_richness=0,
                        TempSeas=median(TempSeas),
                        PrecipSeas=median(PrecipSeas),
                        ThermalIndex=median(ThermalIndex),
                        PrecipIndex=median(PrecipIndex)))
  
  nd$AgeConv <- exp(nd$LogAgeConv)-1
  
  preds <- PredictGLMERRandIter(model = modelRichPlotting$model,data = nd,nIters = 100)
  
  preds <- exp(preds)
  
  preds.median <- apply(X = preds,MARGIN = 1,FUN = median)
  preds.upper <- apply(X = preds,MARGIN = 1,FUN = quantile,probs = (1/6)) 
  preds.lower <- apply(X = preds,MARGIN = 1,FUN = quantile,probs = (5/6)) 
  
  X.Vec <- c(nd$AgeConv, max(nd$AgeConv), 
             rev(nd$AgeConv), min(nd$AgeConv))
  Y.Vec <- c(preds.lower, tail(preds.upper, 1), 
             rev(preds.upper), (preds.lower)[1])
  
  polygon(x = X.Vec,y = Y.Vec,col=paste0(col,"33"),border=NA)
  
  points(x = nd$AgeConv,y = preds.median,type="l",col=col,lwd=2)
  
},c("Primary Vegetation","Secondary Vegetation","Harvested","Pasture"),
c("#66A61E","#1B9E77","#E6AB02","#D95F02")))

mtext(text = "e",side = 3,line = -0.7,adj = -0.14,font = 2,ps=10)

plot.new()

legend(x = 0.05,y = 0.7,lty=1,cex=1,bty="n",lwd=2,
       col=c("#66A61E","#1B9E77","#E6AB02","#D95F02"),
       legend = c("Primary Vegetation","Secondary Vegetation",
                  "Harvested agriculture","Pasture"))

invisible(dev.off())

t.end <- Sys.time()

print(round(t.end - t.start,0))

sink()