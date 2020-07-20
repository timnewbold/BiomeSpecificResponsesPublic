outDir <- "2_PrepareSiteData/"

dataDir <- "0_data/"
inDir <- "1_PrepareDiversityData/"

sink(file = paste0(outDir,"log.txt"))

t.start <- Sys.time()

print(t.start)

suppressMessages(suppressWarnings(library(yarg)))

print(sessionInfo())

load(paste0(dataDir,"modelling_data.rd"))

sites.div.endem <- sites.div
rm(sites.div)

load(paste(inDir,"diversity_data.Rd",sep=""))

sites.div<-SiteMetrics(diversity=diversity,
                       extra.cols=c("SSB","SSBS","Biome","Sampling_method",
                                    "Study_common_taxon","Sampling_effort",
                                    "Sampling_effort_unit","Biome",
                                    "Predominant_land_use","Class",'Country'),
                       sites.are.unique=TRUE,
                       srEstimators=FALSE,richWeights = diversity$rangeWeight)

sites.div <- sites.div[(sites.div$Predominant_land_use!="Urban"),]

sites.div <- sites.div[(sites.div$Biome!="Tundra"),]
sites.div <- sites.div[(sites.div$Biome!="Flooded Grasslands & Savannas"),]
sites.div <- sites.div[(sites.div$Biome!="Mangroves"),]

sites.div <- droplevels(sites.div)

cat('Arranging land-use and intensity classification\n')
sites.div$LandUse<-paste(sites.div$Predominant_land_use)
sites.div$LandUse[which(sites.div$LandUse=="Primary vegetation")]<-"Primary Vegetation"
sites.div$LandUse[which(sites.div$LandUse=="Secondary vegetation (indeterminate age)")]<-"Secondary Vegetation"
sites.div$LandUse[which(sites.div$LandUse=="Mature secondary vegetation")]<-"Secondary Vegetation"
sites.div$LandUse[which(sites.div$LandUse=="Intermediate secondary vegetation")]<-"Secondary Vegetation"
sites.div$LandUse[which(sites.div$LandUse=="Young secondary vegetation")]<-"Secondary Vegetation"
sites.div$LandUse[which(sites.div$LandUse=="Cannot decide")]<-NA
sites.div$LandUse<-factor(sites.div$LandUse)
sites.div$LandUse<-relevel(sites.div$LandUse,ref="Primary Vegetation")

sites.div$LandUse2 <- dplyr::recode(
  sites.div$LandUse,
  'Primary Vegetation' = 'Primary Vegetation',
  'Secondary Vegetation' = 'Secondary Vegetation',
  'Plantation forest' = 'Plantation forest',
  'Cropland' = 'Agriculture',
  'Pasture' = 'Agriculture')

sites.div$LandUse3 <- dplyr::recode(
  sites.div$LandUse,
  'Primary Vegetation' = 'Primary Vegetation',
  'Secondary Vegetation' = 'Secondary Vegetation',
  'Plantation forest' = 'Harvested',
  'Cropland' = 'Harvested',
  'Pasture' = 'Pasture')

sites.div$LandUse4 <- dplyr::recode(
  sites.div$LandUse,
  'Primary Vegetation' = 'Primary Vegetation',
  'Secondary Vegetation' = 'Secondary Vegetation',
  'Plantation forest' = 'Human',
  'Cropland' = 'Human',
  'Pasture' = 'Human')

sites.div$LandUse5 <- dplyr::recode(
  sites.div$LandUse,
  'Primary Vegetation' = 'Natural',
  'Secondary Vegetation' = 'Natural',
  'Plantation forest' = 'Plantation forest',
  'Cropland' = 'Cropland',
  'Pasture' = 'Pasture')

sites.div$LandUse6 <- dplyr::recode(
  sites.div$LandUse,
  'Primary Vegetation' = 'Natural',
  'Secondary Vegetation' = 'Natural',
  'Plantation forest' = 'Plantation forest',
  'Cropland' = 'Agriculture',
  'Pasture' = 'Agriculture')

sites.div$LandUse7 <- dplyr::recode(
  sites.div$LandUse,
  'Primary Vegetation' = 'Natural',
  'Secondary Vegetation' = 'Natural',
  'Plantation forest' = 'Harvested',
  'Cropland' = 'Harvested',
  'Pasture' = 'Pasture')

sites.div$LandUse8 <- dplyr::recode(
  sites.div$LandUse,
  'Primary Vegetation' = 'Natural',
  'Secondary Vegetation' = 'Natural',
  'Plantation forest' = 'Human',
  'Cropland' = 'Human',
  'Pasture' = 'Human')

sites.div$Biome <- dplyr::recode(
  sites.div$Biome,
  'Boreal Forests/Taiga' = 'Boreal Forest',
  'Temperate Conifer Forests' = 'Temperate Conifer Forest',
  'Temperate Broadleaf & Mixed Forests' = 'Temperate Broadleaf Forest',
  'Montane Grasslands & Shrublands' = 'Montane Grasslands',
  'Temperate Grasslands, Savannas & Shrublands' = 'Temperate Grasslands',
  'Mediterranean Forests, Woodlands & Scrub' = 'Mediterranean',
  'Deserts & Xeric Shrublands' = 'Drylands',
  'Tropical & Subtropical Grasslands, Savannas & Shrublands' = 'Tropical Grasslands',
  'Tropical & Subtropical Coniferous Forests' = 'Tropical Conifer Forest',
  'Tropical & Subtropical Dry Broadleaf Forests' = 'Tropical Dry Broadleaf Forest',
  'Tropical & Subtropical Moist Broadleaf Forests' = 'Tropical Moist Broadleaf Forest')

sites.div$Biome2 <- dplyr::recode(
  sites.div$Biome,
  'Boreal Forest' = 'Boreal Forest',
  'Temperate Conifer Forest' = 'Temperate Forest',
  'Temperate Broadleaf Forest' = 'Temperate Forest',
  'Montane Grasslands' = 'Temperate/Montane Grasslands',
  'Temperate Grasslands' = 'Temperate/Montane Grasslands',
  'Mediterranean' = 'Mediterranean',
  'Drylands' = 'Drylands',
  'Tropical Grasslands' = 'Tropical Grasslands',
  'Tropical Conifer Forest' = 'Tropical Forest',
  'Tropical Dry Broadleaf Forest' = 'Tropical Forest',
  'Tropical Moist Broadleaf Forest' = 'Tropical Forest')

sites.div$Biome3 <- dplyr::recode(
  sites.div$Biome,
  'Boreal Forest' = 'Boreal',
  'Temperate Conifer Forest' = 'Temperate',
  'Temperate Broadleaf Forest' = 'Temperate',
  'Montane Grasslands' = 'Temperate',
  'Temperate Grasslands' = 'Temperate',
  'Mediterranean' = 'Mediterranean',
  'Drylands' = 'Drylands',
  'Tropical Grasslands' = 'Tropical',
  'Tropical Conifer Forest' = 'Tropical',
  'Tropical Dry Broadleaf Forest' = 'Tropical',
  'Tropical Moist Broadleaf Forest' = 'Tropical')

sites.div$Biome4 <- dplyr::recode(
  sites.div$Biome,
  'Boreal Forest' = 'Forest',
  'Temperate Conifer Forest' = 'Forest',
  'Temperate Broadleaf Forest' = 'Forest',
  'Montane Grasslands' = 'Grasslands',
  'Temperate Grasslands' = 'Grasslands',
  'Mediterranean' = 'Mediterranean',
  'Drylands' = 'Drylands',
  'Tropical Grasslands' = 'Grasslands',
  'Tropical Conifer Forest' = 'Forest',
  'Tropical Dry Broadleaf Forest' = 'Forest',
  'Tropical Moist Broadleaf Forest' = 'Forest')

sites.div$Biome5 <- dplyr::recode(
  sites.div$Biome,
  'Boreal Forest' = 'Extratropical',
  'Temperate Conifer Forest' = 'Extratropical',
  'Temperate Broadleaf Forest' = 'Extratropical',
  'Montane Grasslands' = 'Extratropical',
  'Temperate Grasslands' = 'Extratropical',
  'Mediterranean' = 'Extratropical',
  'Drylands' = 'Extratropical',
  'Tropical Grasslands' = 'Tropical',
  'Tropical Conifer Forest' = 'Tropical',
  'Tropical Dry Broadleaf Forest' = 'Tropical',
  'Tropical Moist Broadleaf Forest' = 'Tropical')

table(sites.div$LandUse3,sites.div$Biome2)

cat('With endemicity measure')
sites.div$CWM_log.range <- sites.div.endem$CWM_log.range[
  match(sites.div$SSBS,sites.div.endem$SSBS)]

tempData <- sites.div[!is.na(sites.div$CWM_log.range),]
table(tempData$LandUse3,tempData$Biome2)

site_sample_years <- as.numeric(sapply(
  X = sites.div$Sample_start_earliest,
  FUN = function(d){return(strsplit(
    x = paste(d),split = "-")[[1]][1])}))

cat('Summary of sample years:\n')
cat(paste0('Oldest = ',min(site_sample_years),'\n'))
cat(paste0('Most recent = ',max(site_sample_years),'\n'))
cat(paste0("% since 2000 = ",(length(which(site_sample_years>=2000))/length(site_sample_years))*100,'\n'))

save(sites.div,file=paste(outDir,"modelling_data.Rd",sep=""))

world <- readOGR(dsn = "0_data",layer = "TM_WORLD_BORDERS-0.3",verbose = FALSE)

pdf(file = paste0(outDir,"ExtendedData1.pdf"),width = 18/2.54,height = 13.5/2.54)

par(mar=c(2.5,0,0,0))

par(cex=1.0)
par(cex.axis=1.0)
par(cex.lab=1.0)
par(cex.main=1.0)
par(ps=10)

plot(world,col="#aaaaaa",border=NA)

sites.div$PointCol <- dplyr::recode(sites.div$Biome2,
                                    'Tropical Forest' = '#0072B2',
                                    'Tropical Grasslands' = '#E69F00',
                                    'Drylands' = '#D55E00',
                                    'Mediterranean' = '#CC79A7',
                                    'Temperate Forest' = '#56B4E9',
                                    'Temperate/Montane Grasslands' = '#F0E442',
                                    'Boreal Forest' = '#00000000')

points(sites.div$Longitude,sites.div$Latitude,pch=16,cex=0.5,col=paste(sites.div$PointCol))

par(ps=10)
par(cex=1)
par(cex.lab=1)
par(cex.axis=1)

legend(x = -180,y = -95,legend = c(
  'Tropical Forest','Tropical Grasslands','Drylands','Mediterranean',
  'Temperate Forest','Temperate Grasslands'),
  col = c('#0072B2','#E69F00','#D55E00',
          '#CC79A7','#56B4E9','#F0E442'),
  ncol=3,xpd=TRUE,cex=1,pch=16,bty="n")

invisible(dev.off())

t.end <- Sys.time()

print(round(t.end - t.start,0))

sink()
