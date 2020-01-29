suppressMessages(suppressWarnings(library(StatisticalModels)))

modelsDir <- "5_RunFinalModels/"

outDir <- "6_PlotLandUseModels/"

load(paste0(modelsDir,"FinalModels.Rd"))

biome.cols <- c('Tropical Forest' = '#0072B2',
                'Tropical Grasslands' = '#E69F00',
                'Drylands' = '#D55E00',
                'Mediterranean' = '#CC79A7',
                'Temperate Forest' = '#56B4E9',
                'Temperate/Montane Grasslands' = '#F0E442')

png(filename = paste0(outDir,"RichnessResults.png"),width = 17.5,height = 12,units = "cm",res = 150)

par(mfrow=c(2,3))
par(las=1)
par(tck=-0.01)
par(mgp=c(1.8,0.4,0))
par(mar=c(2.2,3.2,1.8,0.8))
par(cex.axis=1.2)
par(cex.lab=1.5)
par(cex.main=1.6)

humanLUSens <- mapply(FUN = function(biome,panelLetter){
  
  nd <- data.frame(LandUse3=factor(
    c('Primary Vegetation','Secondary Vegetation','Pasture','Harvested'),
    levels=levels(finalModelRich$data$LandUse3)))
  
  nd$Biome2 <- factor(biome,levels=levels(finalModelRich$data$Biome2))
  
  nd$Species_richness <- 0
  
  preds <- PredictGLMER(model = finalModelRich$model,data = nd,se.fit = TRUE,
                        seMultiplier = 1.96)
  
  preds$yplus <- ((exp(preds$yplus)/exp(preds$y[1]))*100)-100
  preds$yminus <- ((exp(preds$yminus)/exp(preds$y[1]))*100)-100
  preds$y <- ((exp(preds$y)/exp(preds$y[1]))*100)-100
  
  errbar(x = 1:4,y = preds$y,yplus = preds$yplus,yminus = preds$yminus,
         cap = 0,pch=21,cex=2,bg=c("#66A61E","#1B9E77","#D95F02","#E6AB02"),
         lwd=2,xaxt="n",
         xlab=NA,ylab="Richness change (%)",ylim=c(-60,50),xlim=c(0.7,4.3))
  points(x = 1:4,y = preds$y,pch=21,cex=2,
         bg=c("#66A61E","#1B9E77","#D95F02","#E6AB02"))
  
  axis(side = 1,at = 1:4,labels = c("PV","SV","PAS","HARV"))
  
  axis(side = 1,at = 1:4,line = -1.5,tick = FALSE,lty = 0,cex.axis=0.8,
       labels = paste0("n = ",table(finalModelRich$data$LandUse3[finalModelRich$data$Biome2==biome])[c(1,4,3,2)]))
  
  abline(h=0,lty=2,col="#00000044")
  
  title(main=gsub("[/]Montane","",biome),col.main=biome.cols[which(names(biome.cols)==biome)])
  
  mtext(text = panelLetter,side = 3,line = 0,adj = -0.1,font = 2,ps=10)
  
  
  return(mean(preds$y[c(3,4)]))
  
},unique(finalModelRich$data$Biome2)[c(1,4,5,3,2,6)],
list("a","b","c","d","e","f"))

invisible(dev.off())

names(humanLUSens) <- unique(finalModelRich$data$Biome2)[c(1,4,5,3,2,6)]
names(humanLUSens) <- gsub("[/]Montane","",names(humanLUSens))

saveRDS(object = humanLUSens,file = paste0(outDir,"BiomeRichnessSensitivityHumanLU.rds"))

png(filename = paste0(outDir,"AbundanceResults.png"),width = 17.5,height = 12,units = "cm",res = 1200)

par(mfrow=c(2,3))
par(las=1)
par(tck=-0.01)
par(mgp=c(1.8,0.4,0))
par(mar=c(2.2,3.2,1.8,0.8))
par(cex.axis=1.2)
par(cex.lab=1.5)
par(cex.main=1.6)

invisible(sapply(X = unique(finalModelAbund$data$Biome2)[c(1,4,5,3,2,6)],FUN = function(biome){
  
  nd <- data.frame(LandUse3=factor(
    c('Primary Vegetation','Secondary Vegetation','Pasture','Harvested'),
    levels=levels(finalModelAbund$data$LandUse3)))
  
  nd$Biome2 <- factor(biome,levels=levels(finalModelAbund$data$Biome2))
  
  nd$LogAbund <- 0
  
  preds <- PredictGLMER(model = finalModelAbund$model,data = nd,se.fit = TRUE,seMultiplier = 1.96)
  
  preds$yplus <- ((exp(preds$yplus)/exp(preds$y[1]))*100)-100
  preds$yminus <- ((exp(preds$yminus)/exp(preds$y[1]))*100)-100
  preds$y <- ((exp(preds$y)/exp(preds$y[1]))*100)-100
  
  errbar(x = 1:4,y = preds$y,yplus = preds$yplus,yminus = preds$yminus,
         cap = 0,pch=21,cex=2,bg=c("#66A61E","#1B9E77","#D95F02","#E6AB02"),
         lwd=2,xaxt="n",
         xlab=NA,ylab="Abundance change (%)",ylim=c(-75,75))
  points(x = 1:4,y = preds$y,pch=21,cex=2,
         bg=c("#66A61E","#1B9E77","#D95F02","#E6AB02"))
  
  axis(side = 1,at = 1:4,labels = c("PV","SV","PAS","HARV"))
  
  abline(h=0,lty=2,col="#00000044")
  
  title(main=gsub("[/]Montane","",biome),col.main=biome.cols[which(names(biome.cols)==biome)])
  
}))

invisible(dev.off())


