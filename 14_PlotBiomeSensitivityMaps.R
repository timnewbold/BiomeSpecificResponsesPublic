suppressMessages(suppressWarnings(library(rgdal)))
suppressMessages(suppressWarnings(library(maptools)))
suppressMessages(suppressWarnings(library(dplyr)))
suppressMessages(suppressWarnings(library(RColorBrewer)))

dataDir <- "0_data"
sitesDir <- "2_PrepareSiteData/"
landuseSensitivityDir <- "6_PlotLandUseModels/"
climateSensitivityDir <- "10_EstimateClimateSensitivityByBiome/"

outDir <- "14_PlotBiomeSensitivityMaps/"

ecoreg <- readOGR(dsn = dataDir,layer = "tnc_terr_ecoregions",verbose = FALSE)

ecoreg@data$Biome <- recode(ecoreg@data$WWF_MHTNAM,
                            'Tropical and Subtropical Moist Broadleaf Forests' = 'Tropical Forest',
                            'Tropical and Subtropical Dry Broadleaf Forests' = 'Tropical Forest',
                            'Temperate Broadleaf and Mixed Forests' = 'Temperate Forest',
                            'Tropical and Subtropical Grasslands, Savannas and Shrublands' = 'Tropical Grasslands',
                            'Temperate Grasslands, Savannas and Shrublands' = 'Temperate Grasslands',
                            'Montane Grasslands and Shrublands' = 'Temperate Grasslands',
                            'Tundra' = 'Other',
                            'Mangroves' = 'Other',
                            'Flooded Grasslands and Savannas' = 'Other',
                            'Mediterranean Forests, Woodlands and Scrub' = 'Mediterranean',
                            'Deserts and Xeric Shrublands' = 'Drylands',
                            'Tropical and Subtropical Coniferous Forests' = 'Tropical Forest',
                            'Temperate Conifer Forests' = 'Temperate Forest',
                            'Inland Water' = 'Other',
                            'Rock and Ice' = 'Other',
                            'Boreal Forests/Taiga' = 'Other')

biomes <- unionSpatialPolygons(SpP = ecoreg,IDs = ecoreg@data$Biome)

biome.cols <- c('Tropical Forest' = '#0072B2',
                'Tropical Grasslands' = '#E69F00',
                'Drylands' = '#D55E00',
                'Mediterranean' = '#CC79A7',
                'Temperate Forest' = '#56B4E9',
                'Temperate Grasslands' = '#F0E442',
                'Other' = "#cccccc")

load(paste0(sitesDir,"modelling_data.Rd"))

plot.cols <- biome.cols[names(biomes)]

png(filename = paste0(outDir,"BiomesAndSites.png"),width = 12.5,height = 7,units = "cm",res = 150)

par(mar=c(0,0,0,0))

plot(biomes,col=plot.cols,border=NA)

points(sites.div$Longitude,sites.div$Latitude,pch=16,cex=0.5,col="#00000011")

legend(x = -180,y = -20,legend = c(
  "Tropical Forest","Tropical Grasslands","Drylands",
  "Mediterranean","Temperate Forest",
  "Temperate Grasslands","Other"),xjust=0,yjust=0.5,
  fill=biome.cols,bty="n",cex=0.5)

invisible(dev.off())

luSens <- readRDS(paste0(landuseSensitivityDir,"BiomeRichnessSensitivityHumanLU.rds"))

climateSens <- readRDS(paste0(climateSensitivityDir,"BiomeClimateSensitivities_85.rds"))

stopifnot(all(names(luSens)==names(climateSens)))

sens.cols <- rev(c(brewer.pal(n = 9,name = "Blues")[5],brewer.pal(n = 9,name = "Reds")[3:9]))

luSens <- luSens[names(biomes)]
climateSens <- climateSens[names(biomes)]

luSens.cols <- paste(cut(x = luSens,breaks = seq(from=max(luSens,na.rm=TRUE)+0.01,
                                           to = min(luSens,na.rm=TRUE)-0.01,length.out=9),
                   labels=sens.cols))
luSens.cols[luSens.cols=="NA"] <- "#cccccc"

climateSens.cols <- paste(cut(x = climateSens,breaks = seq(from=max(climateSens,na.rm=TRUE)+0.01,
                                                      to = min(climateSens,na.rm=TRUE)-0.01,length.out=9),
                              labels=sens.cols))
climateSens.cols[climateSens.cols=="NA"] <- "#cccccc"

png(filename = paste0(outDir,"BiomeSensitivityMaps.png"),width = 12.5,height = 14,units = "cm",res = 150)

par(mfrow=c(2,1))
par(mar=c(0,0,0,0))

plot(biomes,col=luSens.cols,border=NA)

plot(biomes,col=climateSens.cols,border=NA)

invisible(dev.off())
