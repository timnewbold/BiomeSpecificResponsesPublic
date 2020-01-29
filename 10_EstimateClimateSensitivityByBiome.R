suppressMessages(suppressWarnings(library(raster)))
suppressMessages(suppressWarnings(library(RColorBrewer)))

print(installed.packages()['base',]['Package'])
print(installed.packages()['base',]['Version'])

print(installed.packages()['graphics',]['Package'])
print(installed.packages()['graphics',]['Version'])

print(installed.packages()['raster',]['Package'])
print(installed.packages()['raster',]['Version'])

print(installed.packages()['RColorBrewer',]['Package'])
print(installed.packages()['RColorBrewer',]['Version'])

dataDir <- "0_data/"
inDir <- "7_CalculateClimateDeltas/"
biomeDir <- "9_ProcessBiomeMap/"

outDir <- "10_EstimateClimateSensitivityByBiome/"

climateScenario <- '85'

load(paste0(dataDir,"77_ProcessFutureProjectionsRandomForestsRealisticDispersal/ProjectionResults.Rd"))

deltaRichness_2070 <- raster(projResults$AllVertebrates[[climateScenario]]$`70`$deltaMap)

richnessLoss_70 <- deltaRichness_2070 * 100

deltaTemperature <- raster::raster(paste0(inDir,"DeltaTemp_",climateScenario,"_2070.tif"))

deltaTemperature <- deltaTemperature/10

richnessSensitivity <- richnessLoss_70/deltaTemperature

saveRDS(object = richnessSensitivity,file = paste0(outDir,"ClimateSensitivity_",climateScenario,".rds"))

biomeMap <- raster::raster(paste0(biomeDir,"BiomeRaster.tif"))

biome_key <- read.csv(paste0(dataDir,"biomes_key_with_cols.csv"))

stopifnot(all.equal(extent(deltaRichness_2070),extent(deltaTemperature)))
stopifnot(all.equal(res(deltaRichness_2070),res(deltaTemperature)))

stopifnot(all.equal(extent(deltaRichness_2070),extent(biomeMap)))
stopifnot(all.equal(res(deltaRichness_2070),res(biomeMap)))

biomeNames <- biome_key$WWF_MHTNAM[match(values(biomeMap),biome_key$WWF_MHTNUM)]

biomeNames <- dplyr::recode(biomeNames,
                            'Boreal Forests/Taiga' = 'Bor F',
                            'Deserts and Xeric Shrublands' = 'Dry',
                            'Flooded Grasslands and Savannas' = NA_character_,
                            'Inland Water' = NA_character_,
                            'Mangroves' = NA_character_,
                            'Mediterranean Forests, Woodlands and Scrub' = 'Med',
                            'Montane Grasslands and Shrublands' = 'Temp G',
                            'Rock and Ice' = NA_character_,
                            'Temperate Broadleaf and Mixed Forests' = 'Temp F',
                            'Temperate Conifer Forests' = 'Temp F',
                            'Temperate Grasslands, Savannas and Shrublands' = 'Temp G',
                            'Tropical and Subtropical Coniferous Forests' = 'Trop F',
                            'Tropical and Subtropical Dry Broadleaf Forests' = 'Trop F',
                            'Tropical and Subtropical Grasslands, Savannas and Shrublands' = 'Trop G',
                            'Tropical and Subtropical Moist Broadleaf Forests' = 'Trop F',
                            'Tundra' = NA_character_)

biomeNames <- factor(biomeNames,levels=c('Trop F','Trop G',
                                        'Dry','Med','Temp F',
                                        'Temp G','Bor F'))

biome.cols <- c('Trop F' = '#0072B2',
                'Trop G' = '#E69F00',
                'Dry' = '#D55E00',
                'Med' = '#CC79A7',
                'Temp F' = '#56B4E9',
                'Temp G' = '#F0E442',
                'Bor F' = '#009E73')

png(filename = paste0(outDir,"BiomeClimateSensitivity_",climateScenario,".png"),
    width = 12.5,height = 8,units = "cm",res = 150)

par(mar=c(2.8,4,0.2,0.2))
par(mgp=c(1.2,0.2,0))
par(las=1)
par(cex.axis=0.7)
par(cex.lab=1.0)
par(tck=-0.01)

df <- na.omit(data.frame(Biome=biomeNames,Sens=values(richnessSensitivity)))

# vioplot::vioplot(formula=df$Sens~df$Biome)
graphics::boxplot(
  df$Sens~df$Biome,outline=FALSE,
  ylab = "Richness change / \u00b0C (%)",
  col=biome.cols[levels(df$Biome)])

abline(h=0,lty=2,col="#00000044")

# graphics::boxplot(
#   df$Sens~df$Biome,
#   horizontal=TRUE,outline=FALSE,
#   xlab="Species loss / \u00b0 temperature increase (%)")

invisible(dev.off())

df <- droplevels(df[(df$Biome != 'Bor F'),])

biomeClimateSens <- tapply(X = df$Sens,INDEX = df$Biome,FUN = mean)
names(biomeClimateSens) <- c('Tropical Forest','Tropical Grasslands',
                             'Drylands','Mediterranean','Temperate Forest',
                             'Temperate Grasslands')

saveRDS(object = biomeClimateSens,file = paste0(outDir,"BiomeClimateSensitivities_",climateScenario,".rds"))

