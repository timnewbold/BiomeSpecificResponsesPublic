suppressMessages(suppressWarnings(library(StatisticalModels)))

dataDir <- "2_PrepareSiteData/"

outDir <- "5_RunFinalModels/"

load(paste0(dataDir,"modelling_data.Rd"))

sites.div$LogAbund <- log(sites.div$Total_abundance+1)

sites.div <- droplevels(sites.div)

finalModelRich <- GLMER(modelData = sites.div,responseVar = "Species_richness",
                        fitFamily = "poisson",fixedStruct = "LandUse3*Biome2",
                        randomStruct = "(1|SS)+(1|SSBS)",REML = TRUE)

finalModelAbund <- GLMER(modelData = sites.div,responseVar = "LogAbund",
                         fitFamily = "gaussian",fixedStruct = "LandUse3*Biome2",
                         randomStruct = "(1|SS)",REML = TRUE)

save(finalModelRich,finalModelAbund,file = paste0(outDir,"FinalModels.Rd"))



