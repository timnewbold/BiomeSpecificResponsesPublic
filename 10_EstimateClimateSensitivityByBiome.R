outDir <- "10_EstimateClimateSensitivityByBiome/"

dataDir <- "0_data/"
inDir <- "7_CalculateClimateDeltas/"
biomeDir <- "9_ProcessBiomeMap/"

sink(file = paste0(outDir,"log.txt"))

t.start <- Sys.time()

print(t.start)

suppressMessages(suppressWarnings(library(raster)))
suppressMessages(suppressWarnings(library(RColorBrewer)))
suppressMessages(suppressWarnings(library(snow)))

print(sessionInfo())

invisible(mapply(FUN = function(climateScenario,figureFilename){

  ensembleDirs <- list('75_ProcessFutureProjectionsMaxentRealisticDispersal',
                       '76_ProcessFutureProjectionsGLMRealisticDispersal',
                       '77_ProcessFutureProjectionsRandomForestsRealisticDispersal',
                       '78_ProcessFutureProjectionsBIOCLIMRealisticDispersal',
                       '79_ProcessFutureProjectionsDOMAINRealisticDispersal')

  cores <- parallel::detectCores()-1

  cl <- snow::makeCluster(cores)

  snow::clusterExport(cl = cl,list = c("dataDir","climateScenario"),
                      envir = environment())

  richnessLoss_70 <- stackApply(x = stack(snow::parLapply(
    cl = cl,x = ensembleDirs,fun = function(d){

      load(paste0(dataDir,d,"/ProjectionResults.Rd"))

      deltaRichness <- raster::raster(
        projResults$AllVertebrates[[climateScenario]]$`70`$deltaMap)

      richnessLoss <- deltaRichness * 100

      return(richnessLoss)

    })),indices = rep(1,5),fun = median)

  snow::stopCluster(cl)

  deltaTemperature <- raster::raster(paste0(
    inDir,"DeltaTemp_",climateScenario,"_2070.tif"))

  deltaTemperature <- deltaTemperature/10

  richnessSensitivity <- richnessLoss_70/deltaTemperature

  tiff(filename = paste0(outDir,"SensitivityMap_",climateScenario,".tif"),
       width = 17.5,height = 10,units = "cm",compression = "lzw",res = 1200)

  par(mar=c(0,0,0,4))

  brks <- c(-70,-35,-30,-25,-20,-15,-10,-5,0,25,50,100)
  cols <- c(rev(brewer.pal(n = 9,name = "Reds"))[1:8],brewer.pal(n = 9,name = "Blues")[2:4])

  raster::plot(x = richnessSensitivity,breaks = brks,col = cols,xaxt="n",yaxt="n",bty="n",box=FALSE)

  invisible(dev.off())

  writeRaster(x = richnessSensitivity,filename = paste0(outDir,"ClimateSensitivity_",climateScenario),format = "raster")

  biomeMap <- raster::raster(paste0(biomeDir,"BiomeRaster.tif"))

  biome_key <- read.csv(paste0(dataDir,"biomes_key_with_cols.csv"))

  stopifnot(all.equal(extent(richnessLoss_70),extent(deltaTemperature)))
  stopifnot(all.equal(res(richnessLoss_70),res(deltaTemperature)))

  stopifnot(all.equal(extent(richnessLoss_70),extent(biomeMap)))
  stopifnot(all.equal(res(richnessLoss_70),res(biomeMap)))

  biomeNames <- biome_key$WWF_MHTNAM[match(values(biomeMap),biome_key$WWF_MHTNUM)]

  biomeNames <- dplyr::recode(biomeNames,
                              'Boreal Forests/Taiga' = 'BoF',
                              'Deserts and Xeric Shrublands' = 'Dry',
                              'Flooded Grasslands and Savannas' = NA_character_,
                              'Inland Water' = NA_character_,
                              'Mangroves' = NA_character_,
                              'Mediterranean Forests, Woodlands and Scrub' = 'Med',
                              'Montane Grasslands and Shrublands' = 'TeG',
                              'Rock and Ice' = NA_character_,
                              'Temperate Broadleaf and Mixed Forests' = 'TeF',
                              'Temperate Conifer Forests' = 'TeF',
                              'Temperate Grasslands, Savannas and Shrublands' = 'TeG',
                              'Tropical and Subtropical Coniferous Forests' = 'TrF',
                              'Tropical and Subtropical Dry Broadleaf Forests' = 'TrF',
                              'Tropical and Subtropical Grasslands, Savannas and Shrublands' = 'TrG',
                              'Tropical and Subtropical Moist Broadleaf Forests' = 'TrF',
                              'Tundra' = NA_character_)

  biomeNames <- factor(biomeNames,levels=c('TrF','TrG',
                                           'Dry','Med','TeF',
                                           'TeG','BoF'))

  biome.cols <- c('TrF' = '#0072B2',
                  'TrG' = '#E69F00',
                  'Dry' = '#D55E00',
                  'Med' = '#CC79A7',
                  'TeF' = '#56B4E9',
                  'TeG' = '#F0E442',
                  'BoF' = '#009E73')

  pdf(file = paste0(outDir,figureFilename,".pdf"),
       width = 8.8/2.54,height = 6/2.54)

  par(mar=c(2.2,2.0,0.2,0.2))
  par(mgp=c(1.2,0.2,0))
  par(las=1)
  par(cex=1.0)
  par(cex.axis=1.0)
  par(cex.lab=1.0)
  par(cex.main=1.0)
  par(tck=-0.01)
  par(ps=10)

  df <- na.omit(data.frame(Biome=biomeNames,Sens=values(richnessSensitivity)))

  # vioplot::vioplot(formula=df$Sens~df$Biome)
  graphics::boxplot(
    df$Sens~df$Biome,outline=FALSE,frame=FALSE,
    ylab = "Richness change / \u00b0C (%)",
    xlab="Biome",
    col=biome.cols[levels(df$Biome)])
  box(bty="l",which = "plot")

  abline(h=0,lty=2,col="#00000044")

  # graphics::boxplot(
  #   df$Sens~df$Biome,
  #   horizontal=TRUE,outline=FALSE,
  #   xlab="Species loss / \u00b0 temperature increase (%)")

  invisible(dev.off())

  df <- droplevels(df[(df$Biome != 'BoF'),])

  biomeClimateSens <- tapply(X = df$Sens,INDEX = df$Biome,FUN = mean)
  names(biomeClimateSens) <- c('Tropical Forest','Tropical Grasslands',
                               'Drylands','Mediterranean','Temperate Forest',
                               'Temperate Grasslands')

  saveRDS(object = biomeClimateSens,file = paste0(outDir,"BiomeClimateSensitivities_",climateScenario,".rds"))

},list('85','26'),list('Figure2','ExtendedData7')))

t.end <- Sys.time()

print(round(t.end - t.start,0))

sink()