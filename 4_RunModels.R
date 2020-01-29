suppressMessages(suppressWarnings(library(StatisticalModels)))

dataDir <- "2_PrepareSiteData/"

outDir <- "4_RunModels/"

load(paste0(dataDir,"modelling_data.Rd"))

sites.div$LogAbund <- log(sites.div$Total_abundance+1)

sites.div <- sites.div[!is.na(sites.div$Biome),]
sites.div <- sites.div[!is.na(sites.div$LandUse),]

sites.div <- droplevels(sites.div)

luRichModel1 <- GLMER(modelData = sites.div,responseVar = "Species_richness",
                  fitFamily = "poisson",fixedStruct = "LandUse*Biome",
                  randomStruct = "(1|SS)+(1|SSBS)",REML = FALSE)
luRichModel2 <- GLMER(modelData = sites.div,responseVar = "Species_richness",
                  fitFamily = "poisson",fixedStruct = "LandUse2*Biome",
                  randomStruct = "(1|SS)+(1|SSBS)",REML = FALSE)
luRichModel3 <- GLMER(modelData = sites.div,responseVar = "Species_richness",
                  fitFamily = "poisson",fixedStruct = "LandUse3*Biome",
                  randomStruct = "(1|SS)+(1|SSBS)",REML = FALSE)
luRichModel4 <- GLMER(modelData = sites.div,responseVar = "Species_richness",
                  fitFamily = "poisson",fixedStruct = "LandUse4*Biome",
                  randomStruct = "(1|SS)+(1|SSBS)",REML = FALSE)
luRichModel5 <- GLMER(modelData = sites.div,responseVar = "Species_richness",
                  fitFamily = "poisson",fixedStruct = "LandUse5*Biome",
                  randomStruct = "(1|SS)+(1|SSBS)",REML = FALSE)
luRichModel6 <- GLMER(modelData = sites.div,responseVar = "Species_richness",
                  fitFamily = "poisson",fixedStruct = "LandUse6*Biome",
                  randomStruct = "(1|SS)+(1|SSBS)",REML = FALSE)
luRichModel7 <- GLMER(modelData = sites.div,responseVar = "Species_richness",
                  fitFamily = "poisson",fixedStruct = "LandUse7*Biome",
                  randomStruct = "(1|SS)+(1|SSBS)",REML = FALSE)
luRichModel8 <- GLMER(modelData = sites.div,responseVar = "Species_richness",
                  fitFamily = "poisson",fixedStruct = "LandUse8*Biome",
                  randomStruct = "(1|SS)+(1|SSBS)",REML = FALSE)
luRichModel0 <- GLMER(modelData = sites.div,responseVar = "Species_richness",
                      fitFamily = "poisson",fixedStruct = "Biome",
                      randomStruct = "(1|SS)+(1|SSBS)",REML = FALSE)

save(luRichModel0,luRichModel1,luRichModel2,luRichModel3,luRichModel4,
     luRichModel5,luRichModel6,luRichModel7,luRichModel8,
     file = paste0(outDir,"RichnessLandUseModels.Rd"))

luAbundModel1 <- GLMER(modelData = sites.div,responseVar = "LogAbund",
                      fitFamily = "gaussian",fixedStruct = "LandUse*Biome",
                      randomStruct = "(1|SS)",REML = FALSE)
luAbundModel2 <- GLMER(modelData = sites.div,responseVar = "LogAbund",
                      fitFamily = "gaussian",fixedStruct = "LandUse2*Biome",
                      randomStruct = "(1|SS)",REML = FALSE)
luAbundModel3 <- GLMER(modelData = sites.div,responseVar = "LogAbund",
                      fitFamily = "gaussian",fixedStruct = "LandUse3*Biome",
                      randomStruct = "(1|SS)",REML = FALSE)
luAbundModel4 <- GLMER(modelData = sites.div,responseVar = "LogAbund",
                      fitFamily = "gaussian",fixedStruct = "LandUse4*Biome",
                      randomStruct = "(1|SS)",REML = FALSE)
luAbundModel5 <- GLMER(modelData = sites.div,responseVar = "LogAbund",
                      fitFamily = "gaussian",fixedStruct = "LandUse5*Biome",
                      randomStruct = "(1|SS)",REML = FALSE)
luAbundModel6 <- GLMER(modelData = sites.div,responseVar = "LogAbund",
                      fitFamily = "gaussian",fixedStruct = "LandUse6*Biome",
                      randomStruct = "(1|SS)",REML = FALSE)
luAbundModel7 <- GLMER(modelData = sites.div,responseVar = "LogAbund",
                      fitFamily = "gaussian",fixedStruct = "LandUse7*Biome",
                      randomStruct = "(1|SS)",REML = FALSE)
luAbundModel8 <- GLMER(modelData = sites.div,responseVar = "LogAbund",
                      fitFamily = "gaussian",fixedStruct = "LandUse8*Biome",
                      randomStruct = "(1|SS)",REML = FALSE)
luAbundModel0 <- GLMER(modelData = sites.div,responseVar = "LogAbund",
                       fitFamily = "gaussian",fixedStruct = "Biome",
                       randomStruct = "(1|SS)",REML = FALSE)

save(luAbundModel0,luAbundModel1,luAbundModel2,luAbundModel3,luAbundModel4,
     luAbundModel5,luAbundModel6,luAbundModel7,luAbundModel8,
     file = paste0(outDir,"AbundanceLandUseModels.Rd"))

biomeRichModel1 <- GLMER(modelData = sites.div,responseVar = "Species_richness",
                         fitFamily = "poisson",fixedStruct = "LandUse*Biome",
                         randomStruct = "(1|SS)+(1|SSBS)",REML = FALSE)
biomeRichModel2 <- GLMER(modelData = sites.div,responseVar = "Species_richness",
                         fitFamily = "poisson",fixedStruct = "LandUse*Biome2",
                         randomStruct = "(1|SS)+(1|SSBS)",REML = FALSE)
biomeRichModel3 <- GLMER(modelData = sites.div,responseVar = "Species_richness",
                         fitFamily = "poisson",fixedStruct = "LandUse*Biome3",
                         randomStruct = "(1|SS)+(1|SSBS)",REML = FALSE)
biomeRichModel4 <- GLMER(modelData = sites.div,responseVar = "Species_richness",
                         fitFamily = "poisson",fixedStruct = "LandUse*Biome4",
                         randomStruct = "(1|SS)+(1|SSBS)",REML = FALSE)
biomeRichModel5 <- GLMER(modelData = sites.div,responseVar = "Species_richness",
                         fitFamily = "poisson",fixedStruct = "LandUse*Biome5",
                         randomStruct = "(1|SS)+(1|SSBS)",REML = FALSE)
biomeRichModel0 <- GLMER(modelData = sites.div,responseVar = "Species_richness",
                         fitFamily = "poisson",fixedStruct = "LandUse",
                         randomStruct = "(1|SS)+(1|SSBS)",REML = FALSE)

save(biomeRichModel0,biomeRichModel1,biomeRichModel2,
     biomeRichModel3,biomeRichModel4,biomeRichModel5,
     file = paste0(outDir,"RichnessBiomeModels.Rd"))

biomeAbundModel1 <- GLMER(modelData = sites.div,responseVar = "LogAbund",
                          fitFamily = "gaussian",fixedStruct = "LandUse*Biome",
                          randomStruct = "(1|SS)",REML = FALSE)
biomeAbundModel2 <- GLMER(modelData = sites.div,responseVar = "LogAbund",
                          fitFamily = "gaussian",fixedStruct = "LandUse*Biome2",
                          randomStruct = "(1|SS)",REML = FALSE)
biomeAbundModel3 <- GLMER(modelData = sites.div,responseVar = "LogAbund",
                          fitFamily = "gaussian",fixedStruct = "LandUse*Biome3",
                          randomStruct = "(1|SS)",REML = FALSE)
biomeAbundModel4 <- GLMER(modelData = sites.div,responseVar = "LogAbund",
                          fitFamily = "gaussian",fixedStruct = "LandUse*Biome4",
                          randomStruct = "(1|SS)",REML = FALSE)
biomeAbundModel5 <- GLMER(modelData = sites.div,responseVar = "LogAbund",
                          fitFamily = "gaussian",fixedStruct = "LandUse*Biome5",
                          randomStruct = "(1|SS)",REML = FALSE)
biomeAbundModel0 <- GLMER(modelData = sites.div,responseVar = "LogAbund",
                          fitFamily = "gaussian",fixedStruct = "LandUse",
                          randomStruct = "(1|SS)",REML = FALSE)

save(biomeAbundModel0,biomeAbundModel1,biomeAbundModel2,
     biomeAbundModel3,biomeAbundModel4,biomeAbundModel5,
     file = paste0(outDir,"AbundanceBiomeModels.Rd"))

cat('Richness models - land-use comparison\n')

stats.table <- AIC(luRichModel1$model,luRichModel2$model,
                 luRichModel3$model,luRichModel4$model,
                 luRichModel5$model,luRichModel6$model,
                 luRichModel7$model,luRichModel8$model,
                 luRichModel0$model)

stats.table$delta.AIC <- stats.table$AIC - min(stats.table$AIC)

stats.table$r.squared.conditional <- c(R2GLMER(model = luRichModel1$model)$conditional,
                                       R2GLMER(model = luRichModel2$model)$conditional,
                                       R2GLMER(model = luRichModel3$model)$conditional,
                                       R2GLMER(model = luRichModel4$model)$conditional,
                                       R2GLMER(model = luRichModel5$model)$conditional,
                                       R2GLMER(model = luRichModel6$model)$conditional,
                                       R2GLMER(model = luRichModel7$model)$conditional,
                                       R2GLMER(model = luRichModel8$model)$conditional,
                                       R2GLMER(model = luRichModel0$model)$conditional)

stats.table$r.squared.marginal <- c(R2GLMER(model = luRichModel1$model)$marginal,
                                    R2GLMER(model = luRichModel2$model)$marginal,
                                    R2GLMER(model = luRichModel3$model)$marginal,
                                    R2GLMER(model = luRichModel4$model)$marginal,
                                    R2GLMER(model = luRichModel5$model)$marginal,
                                    R2GLMER(model = luRichModel6$model)$marginal,
                                    R2GLMER(model = luRichModel7$model)$marginal,
                                    R2GLMER(model = luRichModel8$model)$marginal,
                                    R2GLMER(model = luRichModel0$model)$marginal)

print(stats.table)

stats.table$Model <- gsub("[$]model","",row.names(stats.table))

write.csv(x = stats.table,file = paste0(outDir,"StatisticsRichnessLandUseModels.csv"),row.names = FALSE,quote = FALSE)

cat('Abundance models - land-use comparison\n')

stats.table <- AIC(luAbundModel1$model,luAbundModel2$model,
                   luAbundModel3$model,luAbundModel4$model,
                   luAbundModel5$model,luAbundModel6$model,
                   luAbundModel7$model,luAbundModel8$model,
                   luAbundModel0$model)

stats.table$delta.AIC <- stats.table$AIC - min(stats.table$AIC)


stats.table$r.squared.conditional <- c(R2GLMER(model = luAbundModel1$model)$conditional,
                                       R2GLMER(model = luAbundModel2$model)$conditional,
                                       R2GLMER(model = luAbundModel3$model)$conditional,
                                       R2GLMER(model = luAbundModel4$model)$conditional,
                                       R2GLMER(model = luAbundModel5$model)$conditional,
                                       R2GLMER(model = luAbundModel6$model)$conditional,
                                       R2GLMER(model = luAbundModel7$model)$conditional,
                                       R2GLMER(model = luAbundModel8$model)$conditional,
                                       R2GLMER(model = luAbundModel0$model)$conditional)

stats.table$r.squared.marginal <- c(R2GLMER(model = luAbundModel1$model)$marginal,
                                    R2GLMER(model = luAbundModel2$model)$marginal,
                                    R2GLMER(model = luAbundModel3$model)$marginal,
                                    R2GLMER(model = luAbundModel4$model)$marginal,
                                    R2GLMER(model = luAbundModel5$model)$marginal,
                                    R2GLMER(model = luAbundModel6$model)$marginal,
                                    R2GLMER(model = luAbundModel7$model)$marginal,
                                    R2GLMER(model = luAbundModel8$model)$marginal,
                                    R2GLMER(model = luAbundModel0$model)$marginal)

print(stats.table)

stats.table$Model <- gsub("[$]model","",row.names(stats.table))

write.csv(x = stats.table,file = paste0(outDir,"StatisticsAbundanceLandUseModels.csv"),row.names = FALSE,quote = FALSE)

cat('Richness models - biome comparison\n')

stats.table <- AIC(biomeRichModel1$model,biomeRichModel2$model,
                   biomeRichModel3$model,biomeRichModel4$model,
                   biomeRichModel5$model,biomeRichModel0$model)

stats.table$delta.AIC <- stats.table$AIC - min(stats.table$AIC)

stats.table$r.squared.conditional <- c(R2GLMER(model = biomeRichModel1$model)$conditional,
                                       R2GLMER(model = biomeRichModel2$model)$conditional,
                                       R2GLMER(model = biomeRichModel3$model)$conditional,
                                       R2GLMER(model = biomeRichModel4$model)$conditional,
                                       R2GLMER(model = biomeRichModel5$model)$conditional,
                                       R2GLMER(model = biomeRichModel0$model)$conditional)

stats.table$r.squared.marginal <- c(R2GLMER(model = biomeRichModel1$model)$marginal,
                                    R2GLMER(model = biomeRichModel2$model)$marginal,
                                    R2GLMER(model = biomeRichModel3$model)$marginal,
                                    R2GLMER(model = biomeRichModel4$model)$marginal,
                                    R2GLMER(model = biomeRichModel5$model)$marginal,
                                    R2GLMER(model = biomeRichModel0$model)$marginal)

print(stats.table)

stats.table$Model <- gsub("[$]model","",row.names(stats.table))

write.csv(x = stats.table,file = paste0(outDir,"StatisticsRichnessBiomeModels.csv"),row.names = FALSE,quote = FALSE)


cat('Abundance models - biome comparison\n')

stats.table <- AIC(biomeAbundModel1$model,biomeAbundModel2$model,
                   biomeAbundModel3$model,biomeAbundModel4$model,
                   biomeAbundModel5$model,biomeAbundModel0$model)

stats.table$delta.AIC <- stats.table$AIC - min(stats.table$AIC)

stats.table$r.squared.conditional <- c(R2GLMER(model = biomeAbundModel1$model)$conditional,
                                       R2GLMER(model = biomeAbundModel2$model)$conditional,
                                       R2GLMER(model = biomeAbundModel3$model)$conditional,
                                       R2GLMER(model = biomeAbundModel4$model)$conditional,
                                       R2GLMER(model = biomeAbundModel5$model)$conditional,
                                       R2GLMER(model = biomeAbundModel0$model)$conditional)

stats.table$r.squared.marginal <- c(R2GLMER(model = biomeAbundModel1$model)$marginal,
                                    R2GLMER(model = biomeAbundModel2$model)$marginal,
                                    R2GLMER(model = biomeAbundModel3$model)$marginal,
                                    R2GLMER(model = biomeAbundModel4$model)$marginal,
                                    R2GLMER(model = biomeAbundModel5$model)$marginal,
                                    R2GLMER(model = biomeAbundModel0$model)$marginal)

print(stats.table)

stats.table$Model <- gsub("[$]model","",row.names(stats.table))

write.csv(x = stats.table,file = paste0(outDir,"StatisticsAbundanceBiomeModels.csv"),row.names = FALSE,quote = FALSE)


