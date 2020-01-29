suppressMessages(suppressWarnings(library(yarg)))

inDir <- "1_PrepareDiversityData/"

outDir <- "2_PrepareSiteData/"

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

save(sites.div,file=paste(outDir,"modelling_data.Rd",sep=""))

world <- readOGR(dsn = "0_data",layer = "TM_WORLD_BORDERS-0.3",verbose = FALSE)

tiff(filename = paste0(outDir,"SitesMap.tif"),width = 15.92,height = 12,units = "cm",compression = "lzw",res = 1200)

par(mar=c(2.5,0,0,0))

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

legend(x = -180,y = -95,legend = c(
  'Tropical Forest','Tropical Grasslands','Drylands','Mediterranean',
  'Temperate Forest','Temperate Grasslands'),
  col = c('#0072B2','#E69F00','#D55E00',
          '#CC79A7','#56B4E9','#F0E442'),
  ncol=3,xpd=TRUE,cex=0.9,pch=16)

invisible(dev.off())
