outDir <- "4_RunModels/"

dataDir <- "2_PrepareSiteData/"

sink(file = paste0(outDir,"log.txt"))

t.start <- Sys.time()

print(t.start)

suppressMessages(suppressWarnings(library(StatisticalModels)))

print(sessionInfo())

cat('Loading data\n')
load(paste0(dataDir,"modelling_data.Rd"))

sites.div$LogAbund <- log(sites.div$Total_abundance+1)

sites.div <- sites.div[!is.na(sites.div$Biome),]
sites.div <- sites.div[!is.na(sites.div$LandUse),]

sites.div <- droplevels(sites.div)

cat('Running land-use combinations - richness model 1 of 9\n')
luRichModel1 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "Species_richness",
                  fitFamily = "poisson",fixedStruct = "LandUse*Biome",
                  randomStruct = "(1|SS)+(1|SSBS)",REML = FALSE))
cat('Running land-use combinations - richness model 2 of 9\n')
luRichModel2 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "Species_richness",
                  fitFamily = "poisson",fixedStruct = "LandUse2*Biome",
                  randomStruct = "(1|SS)+(1|SSBS)",REML = FALSE))
cat('Running land-use combinations - richness model 3 of 9\n')
luRichModel3 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "Species_richness",
                  fitFamily = "poisson",fixedStruct = "LandUse3*Biome",
                  randomStruct = "(1|SS)+(1|SSBS)",REML = FALSE))
cat('Running land-use combinations - richness model 4 of 9\n')
luRichModel4 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "Species_richness",
                  fitFamily = "poisson",fixedStruct = "LandUse4*Biome",
                  randomStruct = "(1|SS)+(1|SSBS)",REML = FALSE))
cat('Running land-use combinations - richness model 5 of 9\n')
luRichModel5 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "Species_richness",
                  fitFamily = "poisson",fixedStruct = "LandUse5*Biome",
                  randomStruct = "(1|SS)+(1|SSBS)",REML = FALSE))
cat('Running land-use combinations - richness model 6 of 9\n')
luRichModel6 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "Species_richness",
                  fitFamily = "poisson",fixedStruct = "LandUse6*Biome",
                  randomStruct = "(1|SS)+(1|SSBS)",REML = FALSE))
cat('Running land-use combinations - richness model 7 of 9\n')
luRichModel7 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "Species_richness",
                  fitFamily = "poisson",fixedStruct = "LandUse7*Biome",
                  randomStruct = "(1|SS)+(1|SSBS)",REML = FALSE))
cat('Running land-use combinations - richness model 8 of 9\n')
luRichModel8 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "Species_richness",
                  fitFamily = "poisson",fixedStruct = "LandUse8*Biome",
                  randomStruct = "(1|SS)+(1|SSBS)",REML = FALSE))
cat('Running land-use combinations - richness model 9 of 9\n')
luRichModel0 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "Species_richness",
                      fitFamily = "poisson",fixedStruct = "Biome",
                      randomStruct = "(1|SS)+(1|SSBS)",REML = FALSE))

save(luRichModel0,luRichModel1,luRichModel2,luRichModel3,luRichModel4,
     luRichModel5,luRichModel6,luRichModel7,luRichModel8,
     file = paste0(outDir,"RichnessLandUseModels.Rd"))

cat('Running land-use combinations - abundance model 1 of 9\n')
luAbundModel1 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "LogAbund",
                      fitFamily = "gaussian",fixedStruct = "LandUse*Biome",
                      randomStruct = "(1|SS)",REML = FALSE))
cat('Running land-use combinations - abundance model 2 of 9\n')
luAbundModel2 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "LogAbund",
                      fitFamily = "gaussian",fixedStruct = "LandUse2*Biome",
                      randomStruct = "(1|SS)",REML = FALSE))
cat('Running land-use combinations - abundance model 3 of 9\n')
luAbundModel3 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "LogAbund",
                      fitFamily = "gaussian",fixedStruct = "LandUse3*Biome",
                      randomStruct = "(1|SS)",REML = FALSE))
cat('Running land-use combinations - abundance model 4 of 9\n')
luAbundModel4 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "LogAbund",
                      fitFamily = "gaussian",fixedStruct = "LandUse4*Biome",
                      randomStruct = "(1|SS)",REML = FALSE))
cat('Running land-use combinations - abundance model 5 of 9\n')
luAbundModel5 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "LogAbund",
                      fitFamily = "gaussian",fixedStruct = "LandUse5*Biome",
                      randomStruct = "(1|SS)",REML = FALSE))
cat('Running land-use combinations - abundance model 6 of 9\n')
luAbundModel6 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "LogAbund",
                      fitFamily = "gaussian",fixedStruct = "LandUse6*Biome",
                      randomStruct = "(1|SS)",REML = FALSE))
cat('Running land-use combinations - abundance model 7 of 9\n')
luAbundModel7 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "LogAbund",
                      fitFamily = "gaussian",fixedStruct = "LandUse7*Biome",
                      randomStruct = "(1|SS)",REML = FALSE))
cat('Running land-use combinations - abundance model 8 of 9\n')
luAbundModel8 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "LogAbund",
                      fitFamily = "gaussian",fixedStruct = "LandUse8*Biome",
                      randomStruct = "(1|SS)",REML = FALSE))
cat('Running land-use combinations - abundance model 9 of 9\n')
luAbundModel0 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "LogAbund",
                       fitFamily = "gaussian",fixedStruct = "Biome",
                       randomStruct = "(1|SS)",REML = FALSE))

save(luAbundModel0,luAbundModel1,luAbundModel2,luAbundModel3,luAbundModel4,
     luAbundModel5,luAbundModel6,luAbundModel7,luAbundModel8,
     file = paste0(outDir,"AbundanceLandUseModels.Rd"))


cat('Running land-use combinations - endemicity model 1 of 9\n')
luEndemModel1 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "CWM_log.range",
                       fitFamily = "gaussian",fixedStruct = "LandUse*Biome",
                       randomStruct = "(1|SS)",REML = FALSE))
cat('Running land-use combinations - endemicity model 2 of 9\n')
luEndemModel2 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "CWM_log.range",
                       fitFamily = "gaussian",fixedStruct = "LandUse2*Biome",
                       randomStruct = "(1|SS)",REML = FALSE))
cat('Running land-use combinations - endemicity model 3 of 9\n')
luEndemModel3 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "CWM_log.range",
                       fitFamily = "gaussian",fixedStruct = "LandUse3*Biome",
                       randomStruct = "(1|SS)",REML = FALSE))
cat('Running land-use combinations - endemicity model 4 of 9\n')
luEndemModel4 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "CWM_log.range",
                       fitFamily = "gaussian",fixedStruct = "LandUse4*Biome",
                       randomStruct = "(1|SS)",REML = FALSE))
cat('Running land-use combinations - endemicity model 5 of 9\n')
luEndemModel5 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "CWM_log.range",
                       fitFamily = "gaussian",fixedStruct = "LandUse5*Biome",
                       randomStruct = "(1|SS)",REML = FALSE))
cat('Running land-use combinations - endemicity model 6 of 9\n')
luEndemModel6 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "CWM_log.range",
                       fitFamily = "gaussian",fixedStruct = "LandUse6*Biome",
                       randomStruct = "(1|SS)",REML = FALSE))
cat('Running land-use combinations - endemicity model 7 of 9\n')
luEndemModel7 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "CWM_log.range",
                       fitFamily = "gaussian",fixedStruct = "LandUse7*Biome",
                       randomStruct = "(1|SS)",REML = FALSE))
cat('Running land-use combinations - endemicity model 8 of 9\n')
luEndemModel8 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "CWM_log.range",
                       fitFamily = "gaussian",fixedStruct = "LandUse8*Biome",
                       randomStruct = "(1|SS)",REML = FALSE))
cat('Running land-use combinations - endemicity model 9 of 9\n')
luEndemModel0 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "CWM_log.range",
                       fitFamily = "gaussian",fixedStruct = "Biome",
                       randomStruct = "(1|SS)",REML = FALSE))

save(luEndemModel0,luEndemModel1,luEndemModel2,luEndemModel3,luEndemModel4,
     luEndemModel5,luEndemModel6,luEndemModel7,luEndemModel8,
     file = paste0(outDir,"EndemicityLandUseModels.Rd"))

cat('Running biome combinations - richness model 1 of 6\n')
biomeRichModel1 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "Species_richness",
                         fitFamily = "poisson",fixedStruct = "LandUse*Biome",
                         randomStruct = "(1|SS)+(1|SSBS)",REML = FALSE))
cat('Running biome combinations - richness model 2 of 6\n')
biomeRichModel2 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "Species_richness",
                         fitFamily = "poisson",fixedStruct = "LandUse*Biome2",
                         randomStruct = "(1|SS)+(1|SSBS)",REML = FALSE))
cat('Running biome combinations - richness model 3 of 6\n')
biomeRichModel3 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "Species_richness",
                         fitFamily = "poisson",fixedStruct = "LandUse*Biome3",
                         randomStruct = "(1|SS)+(1|SSBS)",REML = FALSE))
cat('Running biome combinations - richness model 4 of 6\n')
biomeRichModel4 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "Species_richness",
                         fitFamily = "poisson",fixedStruct = "LandUse*Biome4",
                         randomStruct = "(1|SS)+(1|SSBS)",REML = FALSE))
cat('Running biome combinations - richness model 5 of 6\n')
biomeRichModel5 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "Species_richness",
                         fitFamily = "poisson",fixedStruct = "LandUse*Biome5",
                         randomStruct = "(1|SS)+(1|SSBS)",REML = FALSE))
cat('Running biome combinations - richness model 6 of 6\n')
biomeRichModel0 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "Species_richness",
                         fitFamily = "poisson",fixedStruct = "LandUse",
                         randomStruct = "(1|SS)+(1|SSBS)",REML = FALSE))

save(biomeRichModel0,biomeRichModel1,biomeRichModel2,
     biomeRichModel3,biomeRichModel4,biomeRichModel5,
     file = paste0(outDir,"RichnessBiomeModels.Rd"))

cat('Running biome combinations - abundance model 1 of 6\n')
biomeAbundModel1 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "LogAbund",
                          fitFamily = "gaussian",fixedStruct = "LandUse*Biome",
                          randomStruct = "(1|SS)",REML = FALSE))
cat('Running biome combinations - abundance model 2 of 6\n')
biomeAbundModel2 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "LogAbund",
                          fitFamily = "gaussian",fixedStruct = "LandUse*Biome2",
                          randomStruct = "(1|SS)",REML = FALSE))
cat('Running biome combinations - abundance model 3 of 6\n')
biomeAbundModel3 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "LogAbund",
                          fitFamily = "gaussian",fixedStruct = "LandUse*Biome3",
                          randomStruct = "(1|SS)",REML = FALSE))
cat('Running biome combinations - abundance model 4 of 6\n')
biomeAbundModel4 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "LogAbund",
                          fitFamily = "gaussian",fixedStruct = "LandUse*Biome4",
                          randomStruct = "(1|SS)",REML = FALSE))
cat('Running biome combinations - abundance model 5 of 6\n')
biomeAbundModel5 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "LogAbund",
                          fitFamily = "gaussian",fixedStruct = "LandUse*Biome5",
                          randomStruct = "(1|SS)",REML = FALSE))
cat('Running biome combinations - abundance model 6 of 6\n')
biomeAbundModel0 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "LogAbund",
                          fitFamily = "gaussian",fixedStruct = "LandUse",
                          randomStruct = "(1|SS)",REML = FALSE))

save(biomeAbundModel0,biomeAbundModel1,biomeAbundModel2,
     biomeAbundModel3,biomeAbundModel4,biomeAbundModel5,
     file = paste0(outDir,"AbundanceBiomeModels.Rd"))

cat('Running biome combinations - endemicity model 1 of 6\n')
biomeEndemModel1 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "CWM_log.range",
                          fitFamily = "gaussian",fixedStruct = "LandUse*Biome",
                          randomStruct = "(1|SS)",REML = FALSE))
cat('Running biome combinations - endemicity model 2 of 6\n')
biomeEndemModel2 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "CWM_log.range",
                          fitFamily = "gaussian",fixedStruct = "LandUse*Biome2",
                          randomStruct = "(1|SS)",REML = FALSE))
cat('Running biome combinations - endemicity model 3 of 6\n')
biomeEndemModel3 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "CWM_log.range",
                          fitFamily = "gaussian",fixedStruct = "LandUse*Biome3",
                          randomStruct = "(1|SS)",REML = FALSE))
cat('Running biome combinations - endemicity model 4 of 6\n')
biomeEndemModel4 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "CWM_log.range",
                          fitFamily = "gaussian",fixedStruct = "LandUse*Biome4",
                          randomStruct = "(1|SS)",REML = FALSE))
cat('Running biome combinations - endemicity model 5 of 6\n')
biomeEndemModel5 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "CWM_log.range",
                          fitFamily = "gaussian",fixedStruct = "LandUse*Biome5",
                          randomStruct = "(1|SS)",REML = FALSE))
cat('Running biome combinations - endemicity model 6 of 6\n')
biomeEndemModel0 <- suppressWarnings(GLMER(modelData = sites.div,responseVar = "CWM_log.range",
                          fitFamily = "gaussian",fixedStruct = "LandUse",
                          randomStruct = "(1|SS)",REML = FALSE))

save(biomeEndemModel0,biomeEndemModel1,biomeEndemModel2,
     biomeEndemModel3,biomeEndemModel4,biomeEndemModel5,
     file = paste0(outDir,"EndemicityBiomeModels.Rd"))


cat('Calculating model statistics\n')
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

cat('Endemicity models - land-use comparison\n')

stats.table <- AIC(luEndemModel1$model,luEndemModel2$model,
                   luEndemModel3$model,luEndemModel4$model,
                   luEndemModel5$model,luEndemModel6$model,
                   luEndemModel7$model,luEndemModel8$model,
                   luEndemModel0$model)

stats.table$delta.AIC <- stats.table$AIC - min(stats.table$AIC)

stats.table$r.squared.conditional <- c(R2GLMER(model = luEndemModel1$model)$conditional,
                                       R2GLMER(model = luEndemModel2$model)$conditional,
                                       R2GLMER(model = luEndemModel3$model)$conditional,
                                       R2GLMER(model = luEndemModel4$model)$conditional,
                                       R2GLMER(model = luEndemModel5$model)$conditional,
                                       R2GLMER(model = luEndemModel6$model)$conditional,
                                       R2GLMER(model = luEndemModel7$model)$conditional,
                                       R2GLMER(model = luEndemModel8$model)$conditional,
                                       R2GLMER(model = luEndemModel0$model)$conditional)

stats.table$r.squared.marginal <- c(R2GLMER(model = luEndemModel1$model)$marginal,
                                    R2GLMER(model = luEndemModel2$model)$marginal,
                                    R2GLMER(model = luEndemModel3$model)$marginal,
                                    R2GLMER(model = luEndemModel4$model)$marginal,
                                    R2GLMER(model = luEndemModel5$model)$marginal,
                                    R2GLMER(model = luEndemModel6$model)$marginal,
                                    R2GLMER(model = luEndemModel7$model)$marginal,
                                    R2GLMER(model = luEndemModel8$model)$marginal,
                                    R2GLMER(model = luEndemModel0$model)$marginal)

print(stats.table)

stats.table$Model <- gsub("[$]model","",row.names(stats.table))

write.csv(x = stats.table,file = paste0(outDir,"StatisticsEndemicityLandUseModels.csv"),row.names = FALSE,quote = FALSE)

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

cat('Endemicity models - biome comparison\n')

stats.table <- AIC(biomeEndemModel1$model,biomeEndemModel2$model,
                   biomeEndemModel3$model,biomeEndemModel4$model,
                   biomeEndemModel5$model,biomeEndemModel0$model)

stats.table$delta.AIC <- stats.table$AIC - min(stats.table$AIC)

stats.table$r.squared.conditional <- c(R2GLMER(model = biomeEndemModel1$model)$conditional,
                                       R2GLMER(model = biomeEndemModel2$model)$conditional,
                                       R2GLMER(model = biomeEndemModel3$model)$conditional,
                                       R2GLMER(model = biomeEndemModel4$model)$conditional,
                                       R2GLMER(model = biomeEndemModel5$model)$conditional,
                                       R2GLMER(model = biomeEndemModel0$model)$conditional)

stats.table$r.squared.marginal <- c(R2GLMER(model = biomeEndemModel1$model)$marginal,
                                    R2GLMER(model = biomeEndemModel2$model)$marginal,
                                    R2GLMER(model = biomeEndemModel3$model)$marginal,
                                    R2GLMER(model = biomeEndemModel4$model)$marginal,
                                    R2GLMER(model = biomeEndemModel5$model)$marginal,
                                    R2GLMER(model = biomeEndemModel0$model)$marginal)

print(stats.table)

stats.table$Model <- gsub("[$]model","",row.names(stats.table))

write.csv(x = stats.table,file = paste0(outDir,"StatisticsEndemicityBiomeModels.csv"),row.names = FALSE,quote = FALSE)

t.end <- Sys.time()

print(round(t.end - t.start,0))

sink()